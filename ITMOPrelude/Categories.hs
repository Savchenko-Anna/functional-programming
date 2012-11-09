{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Category (->) where
			id = \x -> x
			f . g = \x -> f ( g x )
	
instance Functor List where
		fmap = map

instance Functor Tree where
		fmap = treeMap

instance Functor Maybe where
		fmap f (Just a) = Just $ f a
		fmap f Nothing = Nothing
		
instance Functor (Either a) where
		fmap f (Right b) = Right $ f b
		fmap f (Left b) = Left b
		
instance Monad List where
		return a = Cons a Nil
		xs >>= f = concatMap f xs
		
--instance Monad Tree where
		--return a= addRoot a Null

instance Monad Maybe where
		return = Just
		Nothing >>= _ = Nothing
		(Just x) >>= f = f  x

--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
		return a = State $ \s -> (s, a)
		state >>= f = State $ \s -> let (state', a) = runState state s in runState (f a) state'