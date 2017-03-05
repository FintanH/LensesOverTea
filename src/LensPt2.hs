{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module LensPt2 where

import LensPt1 (Lens', Lens)

data OldLens s a = OldLens
  { get :: s -> a
  , modify :: (a -> a) -> s -> s }

-- Composes two Lenses together

-- get :: b -> c
-- modify :: (c -> c) -> b -> b

-- get :: a -> b
-- modify :: (b -> b) -> a -> a
-- (@.) :: forall a b c . OldLens b c -> OldLens a b -> OldLens a c
-- (@.) _c _b = OldLens
--     -- get :: a -> c
--   { get = get _c . get _b
--
--     -- modify :: (c -> c) -> a -> a
--   , modify = modify _b . modify _c }

-- Example of creating a Category for OldLens
-- instance Category OldLens where
--   id = OldLens id id
--   (.) = (@.)


-- Lens s t a b :: (a -> f b) -> s -> f t
-- Lens' s a :: (a -> f a) -> s -> f a
-- Lens' b c :: (c -> f c) -> b -> f b
-- Lens' a b :: (b -> f b) -> a -> f a
-- Lens' a c :: (c -> f c) -> a -> f a
(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) = flip (.)
