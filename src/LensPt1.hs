{-# LANGUAGE
    RankNTypes, TupleSections, MultiParamTypeClasses,
    FlexibleInstances, FunctionalDependencies #-}

module LensPt1 where

import Data.Bifunctor
import Control.Monad (liftM, join)
import Control.Applicative
import Control.Arrow ((&&&))
import Data.Monoid

{--
 -- One way of implementing Lens
data Lens s a = Lens {
  getter :: s -> a,
  setter :: a -> s -> s
}

setIth :: Int -> a -> [a] -> [a]
setIth i new l
  | i < 0  = error "Oof no negative indices!"
  | null l = error "Whoops no more elements!"
  | old:rest <- l = if i == 0 then new:rest else old : setIth (i - 1) new rest

ix :: Int -> Lens [a] a
ix i = Lens {
  getter = (!! i),
  setter = setIth i
}
--}

{--
{-- Next implementation --}
type Lens s a = (a -> a) -> s -> (a, s)

-- ix :: Int -> (a -> a) -> [a] -> (a, [a])
ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "Off no negative indices"
  | null list = error "Too many indices there buddy"
  | old:rest <- list = if index == 0
                       then (old, (f old) : rest)
                       else second (old:) $ ix (index - 1) f rest
--}

{--
{-- Moving on up! --}
type Lens s a = forall m. Monad m => (a -> m a) -> s -> (a, m s)

-- ix :: Int -> (a -> [a]) -> [a] -> (a, [[a]])
ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "Off no negative indices"
  | null list = error "Too many indices there buddy"
  | old:rest <- list = if index == 0
                       then (old, liftM (: rest) (f old))
                       else second (liftM (old:)) $ ix (index - 1) f rest
--}

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) a = f <$> a

{-- Moving on down ;) --}
-- But we also have the real type
-- s is source and t is target
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- And then using the real type
type Lens' s a = Lens s s a a

{-- Exploring setter IS getter --}
-- Storey x f a === (x, f a)
data Storey x f a = Storey x (f a) deriving (Show)

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

-- over :: Functor f => (a -> f b) -> s -> f t -> ((a -> b) -> s -> t)
over :: Lens s t a b -> ((a -> b) -> s -> t)
-- Identity . f is the composing the (a -> b) with Identity to get (a -> Identity b)
-- Since l is (a -> f b) -> s -> f t
-- Apply (a -> Identity b) to l we get s -> f t
-- Composing the result of this we finally get s -> t
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> (b -> s -> t)
-- const :: a -> b -> a
-- thus if partially apply (a -> b) we get a
-- and we get (a -> s -> t)
set l f = over l (const f)

view :: Lens s t a b -> (s -> a)
-- ((a -> Const a b) -> s -> Const a t) -> (s -> a)
view l f = unConst $ l Const f
  where unConst (Const a) = a

{-- Some Example Lenses --}

-- Lens on a List
-- ix :: Int -> (a -> [a]) -> [a] -> (a, [[a]])
ix :: Int -> Lens' [a] a
ix index f list
  | index < 0 = error "Off no negative indices"
  | null list = error "Too many indices there buddy"
  | old:rest <- list = if index == 0
                       then (: rest) <$> (f old)
                       else (old:) <$> ix (index - 1) f rest

-- Lenses on tuple
_1 :: Lens (a, x) (b, x) a b
-- _1 :: (a -> f b) -> (a, x) -> f (b, x)
_1 f (a, x) = fmap (,x) (f a)

_2 :: Lens (x, a) (x, b) a b
-- _2 :: (a -> f b) -> (x, a) -> f (x, b)
_2 f (x, a) = fmap (x,) (f a)

-- Make a lense out of a getter and a setter
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- (s -> a) -> (s -> b -> t) -> ((a -> f b) -> s -> f t)
lens get set = \f -> \s -> -- f :: (a -> f b) and s :: s
  let bt = set s -- :: (b -> t)
      a = get s -- :: a
  in bt <$> (f a) -- (f a :: f a), fmap bt (f a) :: f t

-- Combine 2 lenses to make a lens which works on Either.
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
-- choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> ((a -> f b) -> Either s1 s2 -> f (Either t1 t2))
-- choosing :: ((a -> f b) -> s1 -> f t1)
--          -> ((a -> f b) -> s2 -> f t2)
--          -> ((a -> f b) -> Either s1 s2 -> f (Either t1 t2))
choosing l1 l2 f s = case s of
    Left s1  -> Left <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

-- Modify the tagret of a lens and return the result (Bonus if you do it
-- without lambdas and defining new functions)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
-- ((a -> f b) -> s -> f t) -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)
-- join :: (e -> e -> a) -> (e -> a)
-- join (&&&) :: (b -> c) -> b -> (c, c)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l ((,) <*> f)

united :: Lens' s ()
-- (a -> f a) -> s -> f s
-- (() -> f ()) -> s -> f s
united f s = const s <$> f ()


{-- Applicative Lens!!! --}
-- It's the same thing but now we have some more power
type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

-- Define a function _all that works on the old Lens
-- e.g set (_all 0) (-8) [100, 600, 0, 200, 0]
--     [100,600,-8,200,-8]
_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s = ref
    set s new = map (\old -> if old == ref then new else old) s

-- We are lacking some power here where we have to stick with only using
-- one value for ref and cannot define a new one as we traverse
-- e.g (_all 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]
--     ? new: 13
--     [100, 600, 13, 200, 13]
-- this weakness comes from fmap

-- Instead we can define it using Applicative
_all' :: Eq a => a -> AppLens' [a] a
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
_all' ref f s = traverse update s
  where
    -- update gives us a way to update values in our context
    -- f :: a -> f a <-- Changes our old values
    -- pure :: f a   <-- Persists our old values
    update old = if old == ref then f old else pure old

-- Type synonyms for Getting and Setting
type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- We can now specialise view, over and set to use the correct functors
-- over :: Functor f => (a -> f b) -> s -> f t -> ((a -> b) -> s -> t)
over' :: Setting s t a b -> (a -> b) -> s -> t
over' l f = runIdentity . l (Identity . f)

set' :: Setting s t a b -> b -> s -> t
set' l b = over' l (const b)

view' :: Getting' a s a -> s -> a
view' l = getConst . l Const

-- Gather a list of results
-- Not implemented here but a difference list is a much better stucture
-- for getting the list of results
toListOf :: Getting' [a] s a -> s -> [a]
toListOf l = getConst . l (\x -> Const [x])

preview :: Getting' (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))

has :: Getting' Any s a -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))

-- We can simply add another parameter to Getting to obtain the Monoid use
type Getting' r s a = (a -> Const r a) -> s -> Const r s

-- So AppLens is actually called Traversal!
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

-- To allow for choice of containers there's a typeclass with each as its
-- only function
-- s -> a etc are functional dependencies which stop ambiguous type errors
-- when using each
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

-- WHOA! each is traverse!
instance Traversable t => Each (t a) (t b) a b where
  each = traverse

-- more lenses
_head :: Traversal' [a] a
_head _ []     = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs
