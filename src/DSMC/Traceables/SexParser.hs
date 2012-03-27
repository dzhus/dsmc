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

where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8
import Data.ByteString

import qualified DSMC.Traceables as T
import DSMC.Util.Vector


-- | Read opening parenthesis.
lp :: Parser ByteString
lp = string "("


-- | Read closing parenthesis.
rp :: Parser ByteString
rp = string ")"


-- | Read vector.
-- @(x.x y.y z.z)@
vector :: Parser Vector
vector = Vector <$> (lp *> double)
                    <*>
                    (skipSpace *> double <* skipSpace)
                    <*>
                    (double <* rp)


-- | Read plane.
--
-- @(plane (x y z) s)@
plane :: Parser T.Body
plane = T.plane <$> (string "plane" *> skipSpace *> vector)
                  <*> (skipSpace *> double)


-- | Read cylinder.
--
-- @(cylinder (x y z) (px py pz) r)@
cylinder :: Parser T.Body
cylinder = T.cylinder <$> (string "cylinder" *> skipSpace *> vector)
                        <*> (skipSpace *> vector)
                        <*> (skipSpace *> double)


-- | Read sphere.
--
-- @(sphere (x y z) r)@
sphere :: Parser T.Body
sphere = T.sphere <$> (string "sphere" *> skipSpace *> vector)
                    <*> (skipSpace *> double)


primitive :: Parser T.Body
primitive = plane <|> cylinder <|> sphere


-- | @(not obj1)@
complement :: Parser T.Body
complement = T.complement <$> (string "not" *> skipSpace *> objParser)


-- | Build parser for @(opName obj1 obj2 …)@
naryOperation :: ByteString -> Parser [T.Body]
naryOperation opName = (string opName *> (many (skipSpace *> objParser)))


-- | @(or obj1 obj2 …)@
union :: Parser T.Body
union = liftM T.union $ naryOperation "or"

-- | @(and obj1 obj2 …)@
intersection :: Parser T.Body
intersection = liftM T.intersection $ naryOperation "and"


objParser :: Parser T.Body
objParser = lp *> (primitive <|> union <|> intersection <|> complement) <* rp


-- | Try to read body definition from bytestring.
--
-- @Left (Just t)@ is returned in case parsing failed, where @t@ is
-- unparsed chunk.
parseBody :: ByteString -> Either (Maybe ByteString) T.Body
parseBody s =
    case (parse objParser s) of
      Done _ r -> Right r
      Partial _ -> Left Nothing
      Fail t _ _ -> Left (Just t)
