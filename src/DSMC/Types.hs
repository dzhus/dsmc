{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

DSMC definitions.

-}

module DSMC.Types
    ( Time
    , Particle(..)
    , Domain(..)
    )

where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import DSMC.Util.Vector

-- | Time in seconds.
type Time = Double


-- | Gas particle with position and velocity.
data Particle = Particle
                {
                  position :: !Point,
                  velocity :: !Vec3
                }
                deriving Show

newtype instance VU.MVector s Particle = MV_Particles (VU.MVector s Particle)
newtype instance VU.Vector    Particle = V_Particles  (VU.Vector    Particle)

instance VGM.MVector VU.MVector Particle where
   {-# INLINE basicLength #-}
   basicLength (MV_Particles v)         = VGM.basicLength v
   {-# INLINE basicUnsafeSlice #-}
   basicUnsafeSlice i n                 = VGM.basicUnsafeSlice i n
   {-# INLINE basicOverlaps #-}
   basicOverlaps                        = VGM.basicOverlaps
   {-# INLINE basicUnsafeNew #-}
   basicUnsafeNew                       = VGM.basicUnsafeNew
   {-# INLINE basicUnsafeReplicate #-}
   basicUnsafeReplicate n               = VGM.basicUnsafeReplicate n
   {-# INLINE basicUnsafeRead #-}
   basicUnsafeRead (MV_Particles mv)    = VGM.basicUnsafeRead mv
   {-# INLINE basicUnsafeWrite #-}
   basicUnsafeWrite (MV_Particles mv) i = VGM.basicUnsafeWrite mv i
   {-# INLINE basicClear #-}
   basicClear                           = VGM.basicClear
   {-# INLINE basicSet #-}
   basicSet (MV_Particles mv)           = VGM.basicSet mv
   {-# INLINE basicUnsafeCopy #-}
   basicUnsafeCopy                      = VGM.basicUnsafeCopy
   {-# INLINE basicUnsafeGrow #-}
   basicUnsafeGrow (MV_Particles mv)    = VGM.basicUnsafeGrow mv

instance VG.Vector VU.Vector Particle where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze                 = VG.basicUnsafeFreeze
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw                   = VG.basicUnsafeThaw
    {-# INLINE basicLength #-}
    basicLength                       = VG.basicLength
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n              = VG.basicUnsafeSlice i n
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Particles v) = VG.basicUnsafeIndexM v
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Particles mv) = VG.basicUnsafeCopy mv
    {-# INLINE elemseq #-}
    elemseq (V_Particles v)           = VG.elemseq v

instance VU.Unbox Particle


-- | Domain in which particle system evolution is simulated.
data Domain = Box Double Double Double Double Double Double
              deriving Show
