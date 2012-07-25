{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| 

Parallel stochastic sampling for 'mwc-random' package.

-}

module Control.Parallel.Stochastic
    ( purifyRandomST
    , ParallelSeeds
    , parMapST
    )

where

import Control.Monad.ST
import Control.Parallel.Strategies

import System.Random.MWC

-- | Convert ST action with PRNG state into a pure function of seed.
purifyRandomST :: (forall s.GenST s -> ST s a) -> Seed -> (a, Seed)
purifyRandomST f seed = runST $ do
                          g <- restore seed
                          r <- f g
                          g' <- save g
                          return (r, g')
{-# INLINE purifyRandomST #-}


-- | 'parMap' with 'rpar' over list of data and initial seeds using ST
-- action which takes single PRNG state; produce list of results and
-- used seeds.
parMapST :: (forall s.GenST s -> a -> ST s b) -> [(a, Seed)] -> [(b, Seed)]
parMapST f =
    parMap rpar (\(p, seed) -> purifyRandomST (`f` p) seed)
{-# INLINE parMapST #-}


-- | List of seeds which preserve PRNG states between runs of parallel
-- stochastic process sampling.
type ParallelSeeds = [Seed]
