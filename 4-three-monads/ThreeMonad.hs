{-# language InstanceSigs #-}
{-# language TypeSynonymInstances #-}
module ThreeMonads where

import Control.Applicative

data Option a = None | Some a
              deriving (Eq, Show)

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap f x = undefined

instance Monoidal Option where
  unit :: Option ()
  unit = undefined
  tuple :: Option a -> Option b -> Option (a, b)
  tuple x y = undefined

instance Alternative Option where
  empty :: Option a
  empty = undefined
  (<|>) :: Option a -> Option a -> Option a
  x <|> y = undefined

instance Monad Option where
  return :: a -> Option a
  return x = undefined
  (>>=) :: Option a -> (a -> Option b) -> Option b
  x >>= f = undefined

data Env e a = Env (e -> a)

runEnv :: e -> Env e a -> a
runEnv e (Env f) = f e

askEnv :: Env e e
askEnv = Env (\e -> e)

instance Functor (Env e) where
  fmap :: (a -> b) -> Env e a -> Env e b
  fmap f (Env g) = Env (\e -> undefined)

instance Monoidal (Env e) where
  unit :: Env e ()
  unit = Env (\e -> undefined)
  tuple :: Env e a -> Env e b -> Env e (a, b)
  tuple (Env f) (Env g) = Env (\s -> undefined)

{- Env is not Alternative
instance Alternative (Env e) where
-}

instance Monad (Env e) where
  return :: a -> Env e a
  return x = Env (\e -> undefined)
  (>>=) :: Env e a -> (a -> Env e b) -> Env e b
  Env f >>= k = undefined

data List a = Nil | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f xs = undefined

instance Monoidal List where
  unit :: List ()
  unit = undefined
  tuple :: List a -> List b -> List (a, b)
  tuple xs ys = undefined

instance Alternative List where
  empty :: List a
  empty = undefined
  (<|>) :: List a -> List a -> List a
  xs <|> ys = undefined

instance Monad List where
  return :: a -> List a
  return x = undefined
  (>>=) :: List a -> (a -> List b) -> List b
  xs >>= f = undefined


-- Applicative from Monoidal

class Functor f => Monoidal f where
  unit  :: f ()
  tuple :: f a -> f b -> f (a, b)

pureMo :: Monoidal f => a -> f a
pureMo x = fmap (\() -> x) unit

apMo :: Monoidal f => f (a -> b) -> f a -> f b
apMo f x = fmap apply (tuple f x)
  where apply :: (a -> b, a) -> b
        apply (g, y) = g y

instance Applicative Option where
  pure  = pureMo
  (<*>) = apMo
instance Applicative List where
  pure  = pureMo
  (<*>) = apMo
instance Applicative (Env e) where
  pure  = pureMo
  (<*>) = apMo