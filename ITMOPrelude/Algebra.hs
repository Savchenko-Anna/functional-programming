{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
			mempty :: a
			mappend :: a -> a -> a

class Monoid a => Group a where
			gempty :: a
			gappend :: a-> a -> a
			ginv :: a -> a

-- Инстансы писать сюда

instance Monoid Nat where
			mempty = natOne
			mappend = (*.)

instance Monoid Int where
			mempty = intOne
			mappend = (.*.)
	
instance Monoid Rat where
			mempty = Rat intOne Zero
			mappend = (%*)

instance Monoid (List a) where
			mempty = Nil
			mappend = (++)
	
instance Group Nat where
			gempty = natOne
			gappend = (*.)
			ginv = natDiv natOne

instance Group Rat where
			gempty = Rat intZero natOne
			gappend = (%*)
			ginv = ratInv