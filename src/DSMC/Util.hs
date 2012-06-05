{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

-- | Utility functions.

module DSMC.Util
    ( solveq
    , SquareRoots
    , purifyRandomST
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Control.Monad.ST

import Data.Strict.Maybe
import Data.Strict.Tuple

import System.Random.MWC


type SquareRoots = Maybe (Pair Double Double)

-- | Solve quadratic equation @ax^2 + bx + c = 0@.
--
-- If less than two roots exist, Nothing is returned.
solveq :: Double
       -- ^ a
       -> Double
       -- ^ b
       -> Double
       -- ^ c
       -> SquareRoots
solveq !a !b !c
    | (d >  0) = Just $ min r1 r2 :!: max r1 r2
    | (d <= 0) = Nothing
    where
      d  =   b * b - 4 * a * c
      q  =   sqrt d
      t  =   2 * a
      r  = - b / t
      s  =   q / t
      r1 =   r - s
      r2 =   r + s
{-# INLINE solveq #-}


-- | Conver ST action with PRNG state into a pure function of seed.
purifyRandomST :: (forall s.GenST s -> ST s a) -> Seed -> a
purifyRandomST f seed = runST $ restore seed >>= f
{-# INLINE purifyRandomST #-}
