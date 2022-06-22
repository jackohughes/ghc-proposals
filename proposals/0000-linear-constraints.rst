Linear Constraints
==============

.. author:: Your name
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

Since the introduction of linear types in GHC 9.0.1, programmers have been able to write programs with safe manual memory management à la Rust. However, using linear types in this way often requires writing code with substantial boilerplate, which can turn writing such code into a chore of carefully ensuring that linear resources are threaded through the program correctly.

This proposal seeks to make programming with linear types more ergonomic through the introduction of a linear constraint system, a Linear analogue to Haskell's existing type class constraints. Linear constraints are implicit *linear* arguments that are resolved automatically at compile time. Thus, we may use a resource in an unrestricted way, while assigning linear capabilities to it through constraints. The theoretical basis of this proposal can be found in the `paper <https://arxiv.org/abs/2103.06127/>`_  by Spiwack et al.


Motivation
----------

The motivation of this proposal is to remove unnecessary boilerplate that is often introduced when programming with linear types. Consider the following program:
::
   read2AndDiscard :: MArray a %1 -> (Ur a, Ur a)
   read2AndDiscard arr0 =
     let (arr1, x) = read arr0 0
         (arr2, y) = read arr1 1
         () = free arr2
     in (x, y)

This is a function which takes an array as a linear argument, reads the first two elements, and then de-allocates it before returning the two elements. The fact that the array is a linear resource provides some nice guarantees which ensure memory safety in a manner which emulates Rust's ownership model. For example, there is no way we can go on to erroneously read from the array after calling de-allocating it. Instead, when ``arr0`` is used as an argument to ``read``, ``read`` consumes it and returns a newly allocated array ``arr1``. This process then repeats before finally ``free`` de-allocates it.

While this code ensures that we use the ``MArray`` in a memory-safe way, this re-naming process introduces boilerplate and becomes cumbersome to both write and read.

Functions like the above are made possible through the power of Haskell's LinearTypes extension, which provides a general purpose framework for programming in a resource-aware manner. This proposal aims to show how using linear constraints can make writing such code more straightforward by sidestepping the bureaucracy of threading resources throughout the code such that linearity is not violated.

Proposed Change Specification
-----------------------------

Syntax Changes
^^^^^^^^^^^^^^
This proposal introduces a new syntax for writing linear constraints when the
``-XLinearTypes`` language extension is enabled.

Currently, type class constraints in GHC do not support multiplicty annotations. Haskell 2010 defines the syntax for type signatures as:
::
   vars :: [context =>] type
where ``vars`` is a list of one or more variables to which the type will be assigned. Type class constraints are represented here through ``context``, a list of zero or more type classes followed by an ``=>``. These class constraints impose no restriction on how they are used in the resulting function body.

We propose the introduction of an alternative *linear* constraint arrow ``=>.``, amending the above grammar to:
::
   vars :: [context =>.] [context =>] type
   vars :: ([context =>.] [context =>]) | ([context =>] [context =>.]) type

Standard non-linear constraints can still be used in combination with linear ones.

Typing Changes
^^^^^^^^^^^^^^

All constraints that occur to the left of this new ``=>.`` arrow must be consumed linearly in the body of the function.

For example, in the following code:
::
   useC :: C =>. Int
   useC = undefined

   addC :: C =>. Int -> Int
   addC n = useC + n

This is acceptable as the constraint ``C`` is used exactly once by ``addC`` (by passing it to ``useC``, where we may assume it is used linearly).

Conversely, the following program is rejected:
::
   add :: C =>. Int -> Int
   add n = n + n

as ``C`` is never consumed. Furthermore, as with linear arguments, we must *guarantee* that ``C`` will be used, regardless of the conditional branching. Thus, the following would also be rejected:
::
   dithering :: C =>. Bool -> Int
   dithering x = if x then useC else 10
as ``useC`` is only consumed when ``x == True``.

These examples show programs rejected by failing to guarantee that ``C`` will be used. However, as we are in a linear context, we must also ensure that it isn't *overused*:
::
   overusing :: C =>. (Int, Int)
   overusing = (useC, useC)

Since ``overusing`` consumes a linear ``C`` constraint twice, it is rejected for violating linearity. We may amend the type scheme of ``overusing`` in the following way, however:
::
   using :: (C, C) =>. (Int, Int)
   using = (useC, useC)
By providing an additional linear constraint, the resource usage guarantees are satisfied and the program type checks.

Examples
--------

We refer back now to the example from the motivation section, which showed how writing a function which reads the first two elements of an array became a tedious exercise of threading our linear resource through the function. Using linear constraints, however, such a function can be written as:
::

   read2AndDiscard ::  (Read, Write) =>. UArray a -> (Ur a, Ur a)
   read2AndDiscard arr = do
        Pack x <- read arr 0
        Pack y <- read arr 1
        () <- free arr
        return (x, y)

The main way in which this differs from our previous function is that our array is no longer a linear resource - it is *unrestricted*. However, we maintain the guarantee that it is used in a way which does not violate linearly through the ``Read`` and ``Write`` linear constraints.

The type signatures for  ``read`` and ``free`` are:
::
   read  :: Read =>. UArray a -> (a .<= Read)

   free :: (Read, Write) =>. UArray a -> ()

The ``Pack`` data type is defined a:
::
   data a .<= c where
     Pack :: c =>. a -> a .<= c

This allows us to access a linear constraint by pattern matching on it. By doing so, the linear constraint from the first ``read`` call is accessed, and a new linear constraint is introduced (via Pack's ``.<= c``). This is repeated before calling ``free``, which de-allocates the array, consuming the remaining ``Read`` and ``Write`` constraints.

Thus we eliminate the need to manually thread the ownership of the array
through the function, whilst maintaining the guarantees of unique ownership via the linear constraints.

For a more substantial example that builds on this, refer to section 4 of `the paper <https://arxiv.org/abs/2103.06127/>`_.



Effect and Interactions
-----------------------

Aside from introducing new syntax for linear constraint arrows, the majority of changes to GHC are localised to GHC's constraint generation and solving.

As this change affects constraints, some care must be taken with regard to how linear constraints interact with existing features. These are detailed below.

Superclasses
^^^^^^^^^^^^

Equality Constraints
^^^^^^^^^^^^^^^^^^^^


Costs and Drawbacks
-------------------


Alternatives
------------


Unresolved Questions
--------------------



Implementation Plan
-------------------
A prototype implementation is available `here <https://archive.softwareheritage.org/browse/revision/f6fc5ba23770b42d1d6020e177787757b16a9ea0/?origin_url=https://github.com/kcsongor/ghc&snapshot=aa61d803eaec9eb4425e3eb8ed2b0fbbd60633cc/>`_.

Endorsements
-------------
