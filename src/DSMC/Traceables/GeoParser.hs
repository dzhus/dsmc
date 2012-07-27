{-# LANGUAGE OverloadedStrings #-}

-- | Simple parser for body definitions using .geo format.
--
-- Body definition contains a set of solid definitions and top level
-- object definition. RHS of solid equations may reference other
-- solids to compose into complex bodies.
--
-- @
-- solid b1 = sphere (0, 0, 0; 5)
-- solid p1 = plane (0, 0, 0; 1, 0, 0)
-- solid body = b1 and p1;
-- tlo body;
-- @
--
-- We use custom types till Traceables are implemented.

module DSMC.Traceables.GeoParser
    ( parseBody
    )

where

import Prelude as P

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Attoparsec.Char8
import Data.ByteString.Char8

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


-- | Read stamement which adds new solid entry to lookup table.
--
-- > <statement> ::=
-- >   'solid' <varname> '=' <expression> ';'
statement :: CSGParser ()
statement = do
  lift $ string "solid" *> skipSpace
  k <- varName
  lift $ skipSpace <* eq <* skipSpace
  v <- readExpr <* lift (cancer *> skipSpace)
  addEntry k v



-- | Expression is either a primitive, a reference to previously
-- defined solid or an operation on expressions.
--
-- > <expression> ::= <primitive> | <reference>
readExpr :: CSGParser T.Body
readExpr = lift primitive <|> readName


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
-- > <tlo> ::= 'tlo' <expression> ';'
topLevel :: CSGParser T.Body
topLevel = lift (string "tlo" *> skipSpace) *> 
           readExpr 
           <* lift (cancer <* skipSpace)


-- | Read sequence of statements which define solids, and finally read
-- top level object definition.
--
-- > <geoFile> ::= <statement> <geoFile> | <tlo>
geoFile :: CSGParser T.Body
geoFile = (many1 statement) *> topLevel


-- | Try to read body definition from bytestring. Return body or error
-- message if parsing fails.
parseBody :: ByteString -> Either String T.Body
parseBody input = 
    case (parseOnly (runStateT geoFile M.empty) input) of
      Right (body, _) -> Right body
      Left msg -> Left msg
