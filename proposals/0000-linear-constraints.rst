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

Currently, type class constraints in GHC do not support multiplicty annotations.
GHC currently defines the syntax for type signatures as:
::

   ctype   ::= context '=>' ctype | type | ...

Essentially, type signatures can consist of (among other things which we ignore
here) zero or more qualified type arrows ``=>`` followed by a type. Here
``context`` is a list of class constraints.

Function type arrows are defined in the grammar for ``type``, where they may
either be unrestricted or annotated with a multiplicity:
::
   type  ::= btype '->' ctype | btype mult '->' ctype | ...

   mult  ::= % atype
   atype ::= ... | INTEGER | CHAR | STRING  | ...
   btype ::= ...

We propose the introduction of an alternative constraint arrow in the ``ctype``
grammar, annotated with a multiplicity following the definition of linear function type
arrows:
::
   ctype ::= context mult '=>' ctype | context '=>' ctype | type | ...

Standard non-linear constraints can then still be used in combination with
linear ones, with the order of linear and unrestricted constraints not mattering.

Note this grammar above is based on the GHC's parser (as of 9.2.3), which differs from
Haskell 2010 which only permits a single qualified type arrow to the left of one or more constraints.


Typing Changes
^^^^^^^^^^^^^^

All constraints that occur to the left of this new multiplicity annotated
constraint arrow must be consumed linearly in the body of the function.

For example, in the following code:
::
   useC :: C % 1 => Int
   useC = undefined

   addC :: C % 1 => Int -> Int
   addC n = useC + n

This is acceptable as the constraint ``C`` is used exactly once by ``addC`` (by passing it to ``useC``, where we may assume it is used linearly).

Conversely, the following program is rejected:
::
   add :: C % 1 => Int -> Int
   add n = n + n

as ``C`` is never consumed. Furthermore, as with linear arguments, we must *guarantee* that ``C`` will be used, regardless of the conditional branching. Thus, the following would also be rejected:
::
   dithering :: C % 1 => Bool -> Int
   dithering x = if x then useC else 10
as ``useC`` is only consumed when ``x == True``.

These examples show programs rejected by failing to guarantee that ``C`` will be used. However, as we are in a linear context, we must also ensure that it isn't *overused*:
::
   overusing :: C % 1 => (Int, Int)
   overusing = (useC, useC)

Since ``overusing`` consumes a linear ``C`` constraint twice, it is rejected for violating linearity. We may amend the type scheme of ``overusing`` in the following way, however:
::
   using :: (C, C) % 1 => (Int, Int)
   using = (useC, useC)
By providing an additional linear constraint, the resource usage guarantees are satisfied and the program type checks.

A full declarative specification of the linearly qualified type system can be
found in section 5 of the paper.

Changes to constraint generation and solving
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We elide the details of the linearly qualified type system's constrain generation
and solving algorithm itself, and instead take a more abstract view, focusing on
how this system deviates from GHC's existing qualified type system (for the
details see section 6 of the paper). Constraint inference may be expressed in the form of a judgement. Consider the existing qualified type system based on the `OutsideIn paper <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fconstraints%2Fjfp-outsidein.pdf>`_. The solver judgement for this system has the form:
::
   Q ; Q_given ; [α]_tch ⊢ C_wanted ~> Q_res ; θ

In brief, we can read this as saying: from the *given* (generated) constraints ``Q_given``, the
solver finds a solution to the *wanted* constraint ``C_wanted``, yielding a set
of residual constraints ``Q_res`` - the constraints that could not be solved
when searching for this solution.

For linearly qualified types, we propose an alternative, simplified judgement:
::
   U ; L_i ⊢ C_wanted ~> L_o

The solver judgement for the linearly qualified type system divides the *given* constraints ``Q_given``
from OutsideIn into two sets ``U`` and ``L``, which represent the
unrestricted and linear constraint sets respectively. Using a solver based on
methods from linear logic proof search, we then attempt to find a solution to
``C_wanted`` which uses the constraints in ``L_i`` linearly. The output of our
judgement ``L_o`` represents the constraints from the linear given constraints
``L_i`` which were not needed in the solution for ``C_wanted`` (unlike in
OutsideIn where ``Q_res`` contained constraints which could not be solved). In
OutsideIn, these unsolved constraints are then generalised over and used in type
inference - something which we do not concern ourselves with here, hence we also
do not need to return a type substitution (``θ``) as an output of our judgement.

An important property of GHC's constraint solver is that it avoides guesses - if
there is ambiguity in the program's constraints then it is rejected. The
linear constraint solver maintains this property.

The existing system also has only a single form of conjunction in its constraint language
``⊗``. This proposal introduces an additional form ``&`` (corresponding to
Linear Logic's *with*), which ensures that linear constraints are consumed in
the same way accross branches in ``case`` statements. In contrast, the existing
system simply accumulates constraints accross branches.

In the implementation of GHC's constraint solver, these changes require
constraints to be annotated with a multiplicity, depending on the context from
which they arise: linear constraints with ``1``, unrestricted constraints with ``ω``.
Fortunately, the groundwork for this has been laid by the LinearTypes proposal
which introduced scaling of term variables. For this proposal we simply need to
ammend this scaling function to scale generated constraints according to their context.


Desugaring
^^^^^^^^^^
As constraints are solved in GHC, a term is constructed in GHC Core which
explicit evidence. Through a desugaring procedure we translate terms from the
linearly qualified type system into GHC Core (which already supports
representation of linear types thanks to the work from the `linear types proposal  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst>`_)

The desugaring is a fairly straightforward inductive procedure. Constraints are
translated into GHC Core's explicit dictionary-passing style by assuming a type
``[[q]]`` in the GHC Core for each atomic constraint ``q`` in the qualified type system. The operation ``[[_]]``
then performs a translation from constraints into explicit evidence defined as:
::
   [[ 1 * q ]]   = [[q]]
   [[ ω * q ]]   = Ur ([[ q ]])
   [[ Ɛ ]]       = 1
   [[ Q1 ⊗ Q2 ]] = [[ Q1 ]] ⊗ [[ Q2 ]]

This then extends to desugaring of simple constraints (e.g. conjunctions of
atomic constraints), type schemes, types, contexts, and
typing derivations of the qualified system into their respective linear GHC Core
counterparts.

This desugaring means that changes to GHC Core itself are not required - since
we can construct an evidence-witnessing term with the existing infrastructure.
As Core is typed, the typechecker verifies that the resulting
program is correct with respect to linearity, before code generation.

Examples
--------

We refer back now to the example from the motivation section, which showed how writing a function which reads the first two elements of an array became a tedious exercise of threading our linear resource through the function. Using linear constraints, however, such a function can be written as:
::

   read2AndDiscard ::  (Read, Write) % 1 => UArray a -> (Ur a, Ur a)
   read2AndDiscard arr = do
        Pack x <- read arr 0
        Pack y <- read arr 1
        () <- free arr
        return (x, y)

The main way in which this differs from our previous function is that our array is no longer a linear resource - it is *unrestricted*. However, we maintain the guarantee that it is used in a way which does not violate linearly through the ``Read`` and ``Write`` linear constraints.

The type signatures for  ``read`` and ``free`` are:
::
   read  :: Read 1 % => UArray a -> (a <= % 1 Read)

   free :: (Read, Write) 1 % => UArray a -> ()

The ``Pack`` data type is defined a:
::
   data a <= % 1 c where
     Pack :: c 1 % => a -> a <= % 1 c

This allows us to access a linear constraint by pattern matching on it. By doing so, the linear constraint from the first ``read`` call is accessed, and a new linear constraint is introduced (via Pack's ``.<= c``). This is repeated before calling ``free``, which de-allocates the array, consuming the remaining ``Read`` and ``Write`` constraints.

Thus we eliminate the need to manually thread the ownership of the array
through the function, whilst maintaining the guarantees of unique ownership via the linear constraints.

For a more substantial example that builds on this, refer to section 4 of `the paper <https://arxiv.org/abs/2103.06127/>`_.

Pack and Unpack
^^^^^^^^^^^^^^^
The *unpacking* of linear constraints via the ``Pack`` construct in the
examples above can actually be inferred automatically, thanks to recent
`work by Eisenberg, Duboc, et al.
<https://richarde.dev/papers/2021/exists/exists.pdf />`_ The prototype
implementation for linearly qualified type system follows a similar approach to
the one outlined in this work, making
adopting this automatic inference fairly straightforward. This eliminates the need for the
programmer to manually write and pattern match on this ``Pack``.


Effect and Interactions
-----------------------

The changes described in the above section equip GHC with a *linearly* qualified type system, allowing us to write programs with linear capabilities which are inferred to be correct implicitly. Primarily, we can now write programs like the one given , which no longer require the manual threading of a linear resource to ensure that the resource is used in a linear way - all the programmer has to do is ensure the linear constraints are satisfied within the program.

Aside from introducing new syntax for linear constraint arrows, the majority of changes to GHC are localised to GHC's constraint generation and solving. Some care must therefore be taken with regard to how linear constraints interact with existing features of GHC's constraint solver: namely the interaction between linear constraints with superclasses in type class constraints and with equality constraints:

Superclasses
^^^^^^^^^^^^

A type class in Haskell can have a *superclass*, which place constraints on
all instances of that class e.g.
::
   class Eq a => Ord a where ...
Every ``Ord a`` must also support ``Eq a`` - if our type is ordered then it must also support equality.

With linear constraints, this means we can end up in a situation where we have a linear constraint (e.g. ``Ord a``) belonging to an unrestricted superclass (``Eq a``). This is not desireable, and
violates one of the lemmas of the qualified type system's entailment relation
(see lemma 5.5 in the paper).

But what if our superclass is also linear? This too causes issues. When given a linear ``Ord a``, do we keep it as is, or rewrite it to the superclass (a linear ``Eq a``) via the entailment relation?

In order to know, the solver has to make a guess and possibly backtrack. As mentioned earlier, this change will not introduce backtracking to GHC's constraint solver. For these reasons, we propose that superclasses of a linear constraint are simply ignored.

Equality Constraints
^^^^^^^^^^^^^^^^^^^^

While the constraint inference of the linearly qualified type system can be performed independenlty of type inference, some care must be taken to ensure this is the case in the presence of GHC's *equality constraints*.

Equality constraints are constraints which arise during type checking that allow unification to be
deferred, and possibly only solvable after solving other constraints. In this way they blur the line between type inference and constraint inference.

This presents an issue as we need to ensure that the solving of these equality constraints does not depend on the solving of linear constraints. We do this by representing
equality constraints in unification as *unrestricted* constraints. Therefore, linear equality constraints cannot be used during type inference, and are solved just like any other linear constraint during constraint inference.

Costs and Drawbacks
-------------------
N/A


Alternatives
------------
N/A


Unresolved Questions
--------------------
N/A


Implementation Plan
-------------------
A prototype implementation is available `here
<https://archive.softwareheritage.org/browse/revision/f6fc5ba23770b42d1d6020e177787757b16a9ea0/?origin_url=https://github.com/kcsongor/ghc&snapshot=aa61d803eaec9eb4425e3eb8ed2b0fbbd60633cc
/>`_.

Endorsements
-------------
