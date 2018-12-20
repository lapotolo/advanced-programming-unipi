-- author: Lapo Toloni, ID: 568235

{-# LANGUAGE TemplateHaskell #-}
module Ex6 where

import Ex1
import Ex2

-- Consider abstract data types as defined here, and in particular in Section 3.1.
-- The goal of this exercise is to re-engineer the definition of ListBag of Exercise 1 and Exercise 2 as specific instances
-- of an Abstract Data Type Constructor called MultiSet.

-- Define a new constructor class MultiSet defining an abstract data type with the same constructors and operations as ListBag
-- Make ListBag an instance of MultiSet
-- Provide a second, different instance of MultiSet, by either exploiting a new concrete representation
-- of multisets or by reusing some data structure provided by the Haskell API.
-- Write a simple function manipulating Multiset, and show that it can be applied to both implementations.
-- Expected output: A Haskell source file Ex6.hs containing a module called Ex6, which imports modules Ex1 and Ex2,
-- and includes only the new definitions developed for this exercise.
-- Note: The file has to be adequately commented, and each function definition has to be preceded by its type, as inferred by the Haskell compiler.
