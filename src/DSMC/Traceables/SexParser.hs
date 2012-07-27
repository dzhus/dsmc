{-# LANGUAGE OverloadedStrings #-}

-- | Simple parser for body definitions given in s-expressions.
--
--
-- @
-- (and (not (plane (0 0 1) 0))
--      (or (sphere (2 2 0) 4)
--          (sphere (-2 -2 0) 4)))
-- @
--
-- We use custom types till Traceables are implemented.

module DSMC.Traceables.SexParser
    ( parseBody
    )

where

import Prelude as P

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator

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
           ,"sphere"
           , "cylinder"
           , "cone"
           ]


-- TODO Handle unexpected keywords in solid names
varName :: Parser String
varName = many1 (letter_ascii <|> digit)


-- | Lookup body in table by its name.
readName :: CSGParser T.Body
readName = do
  k <- lift varName
  v <- getEntry k
  case v of
    Just b -> return b
    _ -> error $ "Undefined solid: " ++ k


-- | Add new solid entry to lookup table.
statement :: CSGParser ()
statement = do
  k <- lift $
       string "solid" *> skipSpace *>
       varName
       <* skipSpace <* eq <* skipSpace
  v <- readExpr <* lift (cancer *> skipSpace)
  addEntry k v


readExpr :: CSGParser T.Body
readExpr = readName <|> (lift primitive)


topLevel :: CSGParser T.Body
topLevel = lift (string "tlo" *> skipSpace) *> readExpr <* lift (cancer <* skipSpace)


-- | Read comma-separated three doubles into point.
-- @x, y, z@
triple :: Parser Point
triple = (,,) <$> double
                   <*>
                   (skipSpace *> double <* skipSpace)
                   <*>
                   double

-- | Read plane.
--
-- @plane (x, y, z; u, v, w)@
plane :: Parser T.Body
plane = T.plane <$>
        (string "plane" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> triple <* skipSpace <* rp)


primitive :: Parser T.Body
primitive = plane


-- | Read sequence of statements which define solids, and finally read
-- top level object definition.
geoFile :: CSGParser T.Body
geoFile = (many1 statement) *> topLevel


-- | Try to read body definition from bytestring.
--
-- @Left (Just t)@ is returned in case parsing failed, where @t@ is
-- unparsed chunk.
--parseBody :: ByteString -> Either (Maybe (ByteString, String)) T.Body
parseBody input = parseOnly (runStateT geoFile M.empty) input
    
    -- case of
    --   Done _ r -> Right $ fst r
    --   Partial _ -> Left Nothing
    --   Fail t _ msg -> Left (Just (t, msg))
