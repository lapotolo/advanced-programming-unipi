{-# LANGUAGE TemplateHaskell #-}
module Ex5 where

import Ex1
import Ex2


-- EXERCISE TEXT
-- The goal of this exercise is to understand how multisets can be equipped with a monadic structure.
-- The resulting monad represents conceptully both a container
-- (a bag of distinct elements, each one with its multiplicity),
-- and a computational effect (a function a -> ListBag b is a non-deterministic function returning the multiset of possible results).

-- Define the operations returnLB and bindLB, taking inspiration from the operations of the Monad type constructor.
-- Try to define an instance of Monad for ListBag using the functions just defined.
-- Discuss whether this is possible or not, and if not what conditions have to be released in order to obtain an instance of Monad
-- Write some tests for the functions just implemented.
-- includes only the new functions defined for this exercise, the requested comments, and the tests.
-- Note: The file has to be adequately commented, and each function definition has to be preceded by its type, as inferred by the Haskell compiler.

instance Monad ListBag
bindLB
returnLB
