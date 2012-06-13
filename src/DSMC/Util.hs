{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

-- | Utility functions.

module DSMC.Util
    ( solveq
    , SquareRoots
    , parMapST
    , purifyRandomST
    , splitIn
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Control.Monad.ST
import Control.Parallel.Strategies

import Data.Strict.Maybe
import Data.Strict.Tuple

import qualified Data.Vector.Generic as VG

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


-- | Convert ST action with PRNG state into a pure function of seed.
purifyRandomST :: (forall s.GenST s -> ST s a) -> Seed -> (a, Seed)
purifyRandomST f seed = runST $ do
                          g <- restore seed
                          r <- f g
                          g' <- save g
                          return (r, g')
{-# INLINE purifyRandomST #-}


-- | 'parMap' with 'rpar' over list of data and initial seeds using ST
-- action taking single PRNG state, producing list of results and used
-- seeds.
parMapST :: (forall s.GenST s -> a -> ST s a) -> [(a, Seed)] -> [(a, Seed)]
parMapST f =
    parMap rpar (\(p, seed) -> purifyRandomST (`f` p) seed)
{-# INLINE parMapST #-}


-- | Split vector into list of n subvectors.
splitIn :: VG.Vector v a => v a -> Int -> [v a]
splitIn ens n =
    let
        partSize = (VG.length ens) `div` n
        splitIn1 0 acc _    = acc
        splitIn1 1 acc rest = (rest:acc)
        splitIn1 m acc rest =
            let
                (v1, v2) = VG.splitAt partSize rest
            in
              splitIn1 (m - 1) (v1:acc) v2
    in
      splitIn1 n [] ens
{-# INLINE splitIn #-}
