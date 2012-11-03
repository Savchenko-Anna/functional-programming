{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

data Tree t = T t (Tree t) (Tree t) | Null deriving (Read, Show)

createEmpty  :: Tree t
createEmpty = Null

addRoot :: t ->Tree t -> Tree t
addRoot r tree = T r tree Null

addLeftmost :: t ->Tree t -> Tree t
addLeftmost a Null = T a Null Null
addLeftmost a (T node left right) = T node (addLeftmost a left) right

addRightmost :: t ->Tree t -> Tree t
addRightmost a Null = T a Null Null
addRightmost a (T node left right) = T node left (addRightmost a right) 

rotateRight :: Tree t -> Tree t
rotateRight (T a (T b leftb rightb) righta) = T b leftb (T a rightb righta)

rotateLeft :: Tree t -> Tree t
rotateLeft (T a lefta (T b leftb rightb)) = T b (T a lefta leftb) rightb

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Null = Null
treeMap f (T a left right) = T (f a) (treeMap f left) (treeMap f right)

treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f a Null = a
treeFoldr f a (T r left right) = f r (treeFoldr f (treeFoldr f a right) left)