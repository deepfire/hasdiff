module Util where

import Data.Text
import Data.Text qualified as T


{-# INLINE uncurry5 #-}
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e
