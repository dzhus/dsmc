{-# LANGUAGE Rank2Types #-}

{-|

Parallel stochastic sampling for 'mwc-random' package.

-}

module Control.Parallel.Stochastic
    ( purifyRandomST
    , ParallelSeeds
    , parMapST
    , splitParMapST
    )

where

import Control.Monad.ST
import Control.Parallel.Strategies

import System.Random.MWC

import Data.Splittable


-- | Convert ST action with PRNG state into a pure function of seed.
purifyRandomST :: (forall s.GenST s -> ST s a) -> Seed -> (a, Seed)
purifyRandomST f seed = runST $ do
                          g <- restore seed
                          r <- f g
                          g' <- save g
                          return (r, g')
{-# INLINE purifyRandomST #-}


type RandomFunction source result = (forall s.GenST s -> source -> ST s result)


-- | 'parMap' with 'rpar' over list of data and initial seeds using ST
-- action which takes single PRNG state; produce list of results and
-- used seeds.
parMapST :: RandomFunction a b -> [(a, Seed)] -> [(b, Seed)]
parMapST f = parMap rpar (\(p, seed) -> purifyRandomST (`f` p) seed)
{-# INLINE parMapST #-}


-- | Split the given source, process subsources in parallel, return
-- combined results and used seeds.
splitParMapST :: (Split source, Combine result) =>
                 RandomFunction source result
              -> source
              -> ParallelSeeds
              -> (result, ParallelSeeds)
splitParMapST f wholeSource oldSeeds =
    let
        sources = (splitIn (length oldSeeds) wholeSource)
        (results, newSeeds) = unzip $ parMapST f $ zip sources oldSeeds
    in
      (combine results, newSeeds)
{-# INLINE splitParMapST #-}


-- | List of seeds which preserve PRNG states between runs of parallel
-- stochastic process sampling.
type ParallelSeeds = [Seed]
