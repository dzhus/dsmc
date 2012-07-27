{-# LANGUAGE OverloadedStrings #-}

-- | Simple parser for body definitions using .geo format.
--
-- Body definition contains a number of solid definitions and ends
-- with the top level object definition. RHS of solid equations may
-- reference other solids to compose into complex bodies.
--
-- Multiple-body compositions are right-associative.
--
-- @
-- # comment
-- solid b1 = sphere (0, 0, 0; 5)
-- solid p1 = plane (0, 0, 0; 1, 0, 0)
-- solid body = b1 and p1;
-- tlo body;
-- @

module DSMC.Traceables.GeoParser
    ( parseBody
    , parseBodyFile
    )

where

import Prelude as P

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Attoparsec.Char8
import Data.ByteString.Char8 as B

import qualified Data.Map as M

import qualified DSMC.Traceables as T
import DSMC.Util.Vector


-- | Transformer which adds lookup table to underlying monad.
type Table a k v = StateT (M.Map k v) a


-- | Add entry to the lookup table.
addEntry :: (Ord k, Monad a) => k -> v -> Table a k v ()
addEntry key value = liftM (M.insert key value) get >>= put


-- | Lookup entry in the table.
getEntry :: (Ord k, Monad a) => k -> Table a k v (Maybe v)
getEntry key = liftM (M.lookup key) get


-- | Parser with lookup table.
type CSGParser = Table Parser String T.Body


lp :: Parser Char
lp = char '('


rp :: Parser Char
rp = char ')'


eq :: Parser Char
eq = char '='


cancer :: Parser Char
cancer = char ';'


comma :: Parser Char
comma = char ','


keywords :: [String]
keywords = [ "solid"
           , "tlo"
           , "plane"
           , "sphere"
           , "cylinder"
           , "cone"
           ]


-- | Read variable name or fail if it's a keyword.
varName :: CSGParser String
varName = do
  k <- lift $ many1 (letter_ascii <|> digit)
  case (P.elem k keywords) of
    False -> return k
    True -> fail $ "Unexpected keyword: " ++ k


-- | Lookup body in table by its name or fail if it is undefined.
readName :: CSGParser T.Body
readName = do
  k <- varName
  v <- getEntry k
  case v of
    Just b -> return b
    _ -> fail $ "Undefined solid: " ++ k


-- | Read plane.
--
-- > <plane> ::=
-- >   'plane (' <triple> ';' <triple> ')'
plane :: Parser T.Body
plane = T.plane <$>
        (string "plane" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> triple <* skipSpace <* rp)


-- | Read sphere.
--
-- > <sphere> ::=
-- >   'sphere (' <triple> ';' <double> ')'
sphere :: Parser T.Body
sphere = T.sphere <$>
        (string "sphere" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp)


-- > <primitive> ::= <plane> | <sphere>
primitive :: Parser T.Body
primitive = plane <|> sphere


-- > <complement> ::= 'not' <body>
complement :: CSGParser T.Body
complement = T.complement <$> (lift (string "not" *> skipSpace) *> body)


-- > <union> ::= <uncomposed-body> 'or' <body>
union :: CSGParser T.Body
union = binary "or" T.unite


-- > <intersection> ::= <uncomposed-body> 'and' <body>
intersection :: CSGParser T.Body
intersection = binary "and" T.intersect


binary :: ByteString -> (T.Body -> T.Body -> T.Body) -> CSGParser T.Body
binary op compose = do
  b1 <- uncomposedBody
  lift (skipSpace *> string op *> skipSpace)
  b2 <- body
  return $ compose b1 b2


-- | Read stamement which adds new solid entry to lookup table.
--
-- > <statement> ::=
-- >   'solid' <varname> '=' <body> ';'
statement :: CSGParser ()
statement = do
  lift $ string "solid" *> skipSpace
  k <- varName
  lift $ skipSpace <* eq <* skipSpace
  v <- body <* lift (cancer *> skipSpace)
  addEntry k v


-- | Expression is either a primitive, a reference to previously
-- defined solid or an operation on expressions.
--
-- > <body> ::= <union> | <intersection> | <complement> | <primitive> | <reference>
body :: CSGParser T.Body
body = union <|> intersection <|> complement <|> uncomposedBody


-- Used to terminate left branch of binary compositions.
-- 
-- > <uncomposed-body> ::= <primitive> | <reference>
uncomposedBody :: CSGParser T.Body
uncomposedBody = lift primitive <|> readName


-- | Read comma-separated three doubles into point.
--
-- > <triple> ::= <double> ',' <double> ',' <double>
triple :: Parser Point
triple = (,,) <$> double
                   <*>
                   (skipSpace *> comma *> skipSpace *>
                    double
                    <* skipSpace <* comma <* skipSpace)
                   <*>
                   double


-- | Top-level object declaration.
--
-- > <tlo> ::= 'tlo' <body> ';'
topLevel :: CSGParser T.Body
topLevel = lift (string "tlo" *> skipSpace) *>
           body
           <* lift (cancer <* skipSpace)


-- | Read one-line comment starting with hash sign.
comment :: Parser ()
comment = char '#' >> (manyTill anyChar endOfLine) >> return ()


-- | Read sequence of statements which define solids, and finally read
-- top level object definition.
--
-- > <geoFile> ::= <statement> <geoFile> | <comment> <geoFile> | <tlo>
geoFile :: CSGParser T.Body
geoFile = (many1 $ statement <|> lift comment) *> topLevel


-- | Try to read body definition from bytestring. Return body or error
-- message if parsing fails.
parseBody :: ByteString -> Either String T.Body
parseBody input =
    case (parseOnly (runStateT geoFile M.empty) input) of
      Right (b, _) -> Right b
      Left msg -> Left msg


-- | Read body definition from file.
parseBodyFile :: FilePath -> IO (Either String T.Body)
parseBodyFile file = parseBody <$> B.readFile file
