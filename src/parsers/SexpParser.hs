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

module SexpParser

where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Data.ByteString as B

import System.Environment

type Vector = (Double, Double, Double)
type Point = Vector


-- Body is a composition of objects
data Body = Sphere Point Double
          | Plane Vector Double
          | Cylinder Vector Point Double
          | Union [Body]
          | Intersection [Body]
          | Complement Body
            deriving Show


-- | Read opening parenthesis.
lp :: Parser ByteString
lp = string "("


-- | Read closing parenthesis.
rp :: Parser ByteString
rp = string ")"


-- | Read vector.
-- @(x.x y.y z.z)@
vector :: Parser Vector
vector = (,,) <$> (lp *> double) 
                  <*> 
                  (skipSpace *> double <* skipSpace) 
                  <*> 
                  (double <* rp)


-- | Read plane.
--
-- @(plane (x y z) s)@
plane :: Parser Body
plane = Plane <$> (string "plane" *> skipSpace *> vector) 
                  <*> (skipSpace *> double)


-- | Read cylinder.
--
-- @(cylinder (x y z) (px py pz) r)@
cylinder :: Parser Body
cylinder = Cylinder <$> (string "cylinder" *> skipSpace *> vector) 
                        <*> (skipSpace *> vector)
                        <*> (skipSpace *> double)


-- | Read sphere.
--
-- @(sphere (x y z) r)@
sphere :: Parser Body
sphere = Sphere <$> (string "sphere" *> skipSpace *> vector) 
                    <*> (skipSpace *> double)


primitive :: Parser Body
primitive = plane <|> cylinder <|> sphere


complement :: Parser Body
complement = Complement <$> (string "not" *> skipSpace *> objParser)


-- | Build parser for @(opName obj1 obj2 …)@
naryOperation :: ByteString -> Parser [Body]
naryOperation opName = (string opName *> (many (skipSpace *> objParser)))


union :: Parser Body
union = liftM Union $ naryOperation "or"


intersection :: Parser Body
intersection = liftM Intersection $ naryOperation "and"


objParser :: Parser Body
objParser = lp *> (primitive <|> union <|> intersection <|> complement) <* rp


-- | Try to read body definition from bytestring.
--
-- @Left (Just t)@ is returned in case parsing failed, where @t@ is
-- unparsed chunk.
parseBody :: ByteString -> Either (Maybe ByteString) Body
parseBody s =
    case (parse objParser s) of
      Done _ r -> Right r
      Partial _ -> Left Nothing
      Fail t _ _ -> Left (Just t)
