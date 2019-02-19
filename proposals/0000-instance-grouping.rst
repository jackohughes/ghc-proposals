Instance declaration grouping
=============================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

We propose a new extension `InstanceGrouping` (Name have to be discussed) which allows a more compact way of declaring instances.

For example, the following ::

  instance Functor Foo where
    fmap (Foo ...) = ...

  instance Applicative Foo where
    pure v = ...
    (Foo ...) <*> (Foo ...) = ...

  instance Monad Foo where
    (>>=) a b = ...

Coulde be compacted as such ::

  instance Monad Foo where
    fmap (Foo ...) = ...
    pure v = ...
    (Foo ...) <*> (Foo ...) = ...
    (>>=) a b = ...

Motivation
------------

This proposal tries to reduce a bit of boilerplate when declaring instances, especially in the context of deep type classe hierarchy as the one we can find in 

The second important point is that it may open the room for future typeclass refactoring without breaking.

For example, the current `Num` class is as following ::

  class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

We can imagine a future typeclass refactoring in which the different functions will be split to differents classes. This refactoring will have huge impact on all the haskell ecosystem because it will break all libraries which use `Num`.

By enabling (by default) `InstanceGrouping`, all legacy code which still be compatible.

I think the `Semigroup` / `Monoid` and `MonaidFail` refactors may have been smoothers with this approach.

    
Proposed Change Specification
-----------------------------

* in instance declaration, when a symbol (type, function, ...) is defined and it does not belongs to the current type class, but to a parent type class for which there is not a defined instance, it will automatically instanciate the parent type class


Effect and Interactions
-----------------------

TBD

Costs and Drawbacks
-------------------

TBD.

- Visibility. If someoen grep for `instance Functor Foo`, he will have no answer if `fmap` is defined in the `instance Monad Foo` block.

Alternatives
------------

TBD.

Unresolved Questions
--------------------

TDB.

Implementation Plan
-------------------

TBD.
