{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

-- | Utility functions and definitions.

module DSMC.Util
    ( solveq
    , SquareRoots
    , fromUnboxed1
    , iforM_
    , randomSeed
    , Time
    , DSMCRootMonad
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Data.Bits
import Data.Word

import Data.Functor
import Data.List

import qualified Data.ByteString as BS

import Data.Strict.Maybe
import Data.Strict.Tuple

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import System.Entropy
import System.Random.MWC

import Data.Splittable

-- | Results of solving a quadratic equation.
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
    | (d > 0)   = Just $ min r1 r2 :!: max r1 r2
    | otherwise = Nothing
    where
      d  =   b * b - 4 * a * c
      q  =   sqrt d
      t  =   2 * a
      r  = - b / t
      s  =   q / t
      r1 =   r - s
      r2 =   r + s
{-# INLINE solveq #-}


-- | Convert between Repa 'R.DIM1'-arrays and unboxed 'VU.Vector's.
fromUnboxed1 :: (VU.Unbox e) => VU.Vector e -> R.Array R.U R.DIM1 e
fromUnboxed1 v = R.fromUnboxed (R.ix1 $ VU.length v) v
{-# INLINABLE fromUnboxed1 #-}


-- | Map monadic action over pairs of vector indices and items and
-- throw away the results.
iforM_ :: (Monad m, VU.Unbox a) =>
          VU.Vector a
       -> ((Int, a) -> m b)
       -> m ()
iforM_ v = VU.forM_ (VU.imap (,) v)
{-# INLINE iforM_ #-}


-- | Fetch vector of random Word32 values from system entropy source.
randomWord32Vector :: Int -> IO (VU.Vector Word32)
randomWord32Vector n =
    let
        -- Left fold accumulator to shift Word8 onto Word32
        accum a o = (a `shiftL` 8) .|. fromIntegral o
    in do
      w8stream <- getEntropy (n * 4)
      -- Split the stream into 4-length lists of Word8
      let w8s = map BS.unpack $ splitIn n w8stream
      return $ VU.fromList $ map (foldl' accum 0) w8s


-- | Fetch new RNG seed from system entropy source.
randomSeed :: IO Seed
randomSeed = toSeed <$> randomWord32Vector 256


-- | Time in seconds.
type Time = Double


-- | Several modules define a chain of monads to maintain context of
-- the running simulation. In its root is the IO monad which we use to
-- send logger messages.
type DSMCRootMonad = IO
