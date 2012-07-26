{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Prelude hiding (splitAt)
import qualified Prelude as P

import Control.Monad.ST
import Control.Parallel.Strategies

import qualified Data.Vector.Unboxed as VU

import System.Random.MWC


-- | Class of tasks which may be splitted into subtasks and combined
-- back into one task.
--
-- 'splitAt' and 'splitIn' must preserve linear ordering on elements
-- of task, if there's any.
class Split source where
    -- | Split the source in two subsources given the size of the first source.
    splitAt :: Int
            -> source
            -> (source, source)

    -- | Calculate the overall size of the source
    size :: source -> Int

    splitIn :: Int
            -- ^ Split the source into that many subsources of equal size.
            -> source
            -> [source]
    splitIn n l =
        let
            partSize = (size l) `div` n
            splitIn1 acc 0 rest = (rest:acc)
            splitIn1 acc m rest = splitIn1 (v1:acc) (m - 1) v2
                                  where
                                    (v1, v2) = splitAt partSize rest
        in
          splitIn1 [] n l
    {-# INLINE splitIn #-}


class Combine result where
    -- | Combine list of results, preserving linear ordering.
    combine :: [result] -> result


instance Split [s] where
    splitAt = P.splitAt
    size = P.length

instance Combine [s] where
    combine = concat


instance (VU.Unbox s) => Split (VU.Vector s) where
    splitAt = VU.splitAt
    size = VU.length

instance (VU.Unbox r) => Combine (VU.Vector r) where
    combine = VU.concat


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


splitParMapST :: (Split task, Combine result) =>
                 RandomFunction task result
              -> task
              -> [Seed]
              -> (result, [Seed])
splitParMapST f task oldSeeds =
    let
        sources = (splitIn (length oldSeeds) task)
        (results, newSeeds) = unzip $ parMapST f $ zip sources oldSeeds
    in
      (combine results, newSeeds)
{-# INLINE splitParMapST #-}


-- | List of seeds which preserve PRNG states between runs of parallel
-- stochastic process sampling.
type ParallelSeeds = [Seed]
