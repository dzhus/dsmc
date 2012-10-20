{-|

Splittable containers abstraction.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Splittable 
    ( Split(..)
    , Combine(..)
    )
    
where

import Prelude hiding (splitAt)
import qualified Prelude as P

import qualified Data.ByteString as BS

import qualified Data.Vector.Generic as VG

    
-- | Class of tasks (containers which hold input data) which may be
-- splitted into subtasks.
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
            -- ^ Split the source into that many subsources of equal
            -- size. This number is capped to the size of source if it
            -- exceeds it.
            -> source
            -> [source]
    splitIn n l | n < 1 = error "Can't split in less than one chunk!"
                | n > (size l) = splitIn (size l) l
                | otherwise =
                    let
                        partSize = (size l) `div` n
                        splitIn1 acc 1 rest = acc ++ [rest]
                        splitIn1 acc m rest = splitIn1 (acc ++ [v1]) (m - 1) v2
                            where
                              (v1, v2) = splitAt partSize rest
                    in
                      splitIn1 [] n l
    {-# INLINE splitIn #-}


-- | A counterpart for 'Split'. Class of containers which may be
-- combined into single container (which holds output data).
class Combine result where
    -- | Combine list of results, preserving linear ordering.
    combine :: [result] -> result


instance Split [s] where
    splitAt = P.splitAt
    size = P.length

instance Combine [s] where
    combine = concat


instance (VG.Vector v e) => Split (v e) where
    splitAt = VG.splitAt
    size = VG.length

instance (VG.Vector v e) => Combine (v e) where
    combine = VG.concat


instance Split BS.ByteString where
    splitAt = BS.splitAt
    size = BS.length
