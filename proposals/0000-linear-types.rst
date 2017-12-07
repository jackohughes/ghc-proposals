.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/91>`_.

.. contents::

Linear Types
============

This proposal introduces a notion of *linear function* to GHC. Linear
functions are regular functions that guarantee that they will use
their argument exactly once. Whether a function ``f`` is linear or not
is called the *multiplicity* of ``f``. We propose a new language
extension, ``-XLinearTypes``.

When turned on, the user can enforce a given multiplicity for ``f``
using a type annotation. By constraining the multiplicity of
functions, API can enforce invariants which are inaccessible with
current Haskell.

The theory behind this proposal has been fully developed in a peer
reviewed conference publication that will be presented at POPL'18. See
the `extended version of the paper <https://arxiv.org/abs/1710.09756>`_.

Motivation
----------

Type safety enforces that *well-typed program do not go
wrong*. Programs will sometimes crash, or fail to terminate, but they
do not segfault. Through well-chosen abstractions, types can be used
to enforce further properties, such as trees being
well-balanced. Among such properties, is *resource safety*: system
resources that these programs manipulate have changing states, need to
be initialized before use and conversely, must be freed in a timely
manner. We want abstractions that enforce that programs do not rewind
the state of I/O resources, these resources are never used before they
are initialized, are guaranteed to be freed by the time control flow
exits user defined scopes, and never used after being freed. Resource
safety is hard to enforce with current Haskell.

This proposal hits another goal as a side benefit. In Haskell, impure
computations are typically structured as a sequence of steps, be it in
the ``IO`` monad or in ``ST``. The latter in particular serves to
precisely control which effects are possible and the scope within
which they are visible. But using monads to write "locally impure"
computations that still look pure from the outside has an unfortunate
consequence: computations are oversequentialized, making it hard for
the compiler to recover lost opportunities for parallelism.

Linear types enable better solutions to both problems:

1. using types to guarantee resource safety, and
2. using types to control the scope of effects without forcing an
   unnatural sequencing of mutually independent effects.

In our `paper <https://arxiv.org/abs/1710.09756>`_, which should be
read to fully appreciate this proposal, we have worked out several
examples in details. @gelisam also designed `a linear API
<https://github.com/gelisam/linear-examples>`_ for `3d-printable
models
<https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable>`_.
In `this blog post
<http://www.tweag.io/posts/2017-11-29-linear-jvm.html>`_,
@facundominguez shows how linear types help use Java references from
Haskell.

Let us, nevertheless, briefly discuss some examples. The following
example (summarized from the
`paper<https://arxiv.org/abs/1710.09756>`_) illustrates points (1)
and (2) above. Using linear types, we express a pure API for mutable
array construction (the type ``a ->. b`` is the type of linear
functions, ``Unrestricted`` is such that ``Unrestricted a ->. b`` is
isomorphic to ``a -> b``):

::

  data MArray a
  data Array a
  newMArray :: Int -> (MArray a ->. Unrestricted b) ->. Unrestricted b
  write :: MArray a ->. (Int, a) -> MArray a
  read :: MArray a ->. Int -> (MArray a, Unrestricted a)
  freeze :: MArray a ->. Unrestricted (Array a)

The types in this interface ensure that values of type ``MArray a``
are always *unique* references to a mutable array. As a consequence,
mutations cannot be observed by the context, because references
aliasing each other is ruled out. Referencial transparency is
preserved.

The two main benefits of this API are:

- reads and writes on distinct arrays are not sequenced. This means
  that the compiler is free to reorder them, *e.g.* as an optimisation.
  We could go further and introduce `fork-join parallelism
  <https://en.wikipedia.org/wiki/Fork%E2%80%93join_model>`_ primitives
  where disjoint slices can be mutated in parallel, *e.g.* by
  different cores.
- The ``freeze`` function consumes the unique ``MArray`` by turning it
  into a non-unique immutable array. ``freeze`` does not, in fact,
  copy the array, it just changes its (static!) state. In the ``ST``
  implementation of ``MArray``, the primitive is ``unsafeFreeze``
  because it is up to the programmer to promise that they won't ever
  mutate the frozen ``MArray`` again. This shrinks the trusted code
  base. Or to put it another way: the user can now write more
  efficient code even when keeping to safe primitives only.

We argue that linear types have far ranging consequences for the
language. Systems programming with quasi real-time requirements can
often benefit from easing pressure on the GC by taking long-lived
objects out of the GC-managed heap entirely. Fewer long-lived objects
in the heap means faster major collection times, hence shorter GC
pauses. Linear types enable *safe* manual memory management for
long-lived objects.

With linear types, we can write an interface to ``malloc`` and
``free`` as follows:

::

  malloc :: Storable a => a ->. (Ptr a ->. Unrestricted b) ->. Unrestricted b
  read :: Storable a => Ptr a ->. (Ptr a, a)
  free :: Ptr a ->. ()

This interface is safe in the sense that users of this interface get
two strong static guarantees:

1. that all that they allocate will eventually be freed, and
2. that after freeing the associated pointer can never be read.

With these two guarantees in hand, users no longer need to rely on the
GC for managing all resources, hence benefiting from lower tail
latencies and potentially higher throughput, while still getting
freedom from segfaults.

Linear types don't just enable using Haskell for more use cases
(low-latency trading appliances, low-level services in
high-performance scientific computing clusters, etc). Correctly
tracking the lifecycle of I/O resources has been a vexing issue for
many network services. Creating a variant of the BSD socket API that
statically guarantees ordering constraints between API calls becomes
possible without the overhead of heavyweight encodings based *e.g.* on
parameterized monads (see the `paper
<https://arxiv.org/abs/1710.09756>`_ for more on this example).

::

  -- We need an variant of the IO monad where actions are linear
  data RIO a
  returnL :: a ->. RIO a
  bindL :: RIO a ->. (a ->. RIO b) ->. RIO b

  -- Definition of sockets
  data State = Unbound | Bound | Listening | Connected
  data Socket (s :: State)
  data SocketAddress

  -- When a (TCP) socket is created it is Unbound.
  socket :: RIO (Socket Unbound)
  -- To bind a socket to a port we take an Unbound socket, and make it
  -- Bound. The type of bindL will ensure that the socket is threaded
  -- through the computation, so that the (Socket Unbound) is not
  -- accessible: we cannot bind a socket twice.
  bind :: Socket Unbound ->. SocketAddress -> RIO (Socket Bound)
  -- A socket must be bound to a port before we start listening
  listen :: Socket Bound->. RIO (Socket Listening)
  -- A socket can accept multiple connection, therefore, the socket is
  -- returned in the same state by accept. A second, bidirectional,
  -- socket representing the connection is also returned. Both have to
  -- be used in a single-threaded fashion.
  accept :: Socket Listening ->. RIO (Socket Listening, Socket Connected)
  connect :: Socket Unbound ->. SocketAddress -> RIO (Socket Connected)
  send :: Socket Connected ->. ByteString -> RIO (Socket Connected, Unrestricted Int)
  receive :: Socket Connected -> RIO (Socket Connected, Unrestricted ByteString)
  close :: ∀s. Socket s -> RIO ()

.. _Specification:

Proposed Change Specification
-----------------------------

We introduce a new language extension. Types with a linearity
specification are syntactically legal anywhere in a module if and only
if ``-XLinearTypes`` is turned on.

This proposal only introduces new type for functions, it does not
affect the run-time system, and does not enforce resource-safety by
itself. Linear types are meant to be used in the design of
abstractions, in particular to enforce resource safety.

Definition
~~~~~~~~~~

We say that a function ``f`` is *linear* when ``f u`` is consumed
exactly once implies that ``u`` is *consumed exactly once* (defined
as follows).

- Consuming a value of a data type exactly once means evaluating it to
  head normal form exactly once, then consuming its fields exactly
  once
- Consuming a function exactly once means applying it and consuming
  its result exactly once

The type of linear function from type ``A`` to type ``B`` is written
``A ->. B`` (see Syntax_).

Linearity is a strengthening of the contract of the regular function
type ``A -> B``, which will be called the type of *unrestricted*
functions.

Remark: linear function ``f`` can diverge (*i.e.* either not terminate
or throw an exception) or be called on diverging data. It may feel
weird because ``f`` will not necessarily consume its argument. But
it's alright: we can still make safe interface, as explained in the
Exceptions_ section below).

Polymorphism
~~~~~~~~~~~~

In order for linear functions and unrestricted functions not to live
in completely distinct worlds, to avoid code duplication, we
introduce a notion of polymorphism, dubbed *multiplicity polymorphism*,
over whether a function is linear.

A linear function is said to have multiplicity ``1`` while an
unrestricted function is said to have multiplicity ``ω``. Multiplicity
polymorphic functions may have variable multiplicity (see also Syntax_), *e.g.*

::

  map :: (a :p-> b) -> [a] :p-> [b]

without polymorphism we would need two implementations of `map`. With
the exact same code: one for ``p=1`` and one for ``p=ω``. Function
composition is even worse: it takes two multiplicity parameters, hence,
would require four identical implementations:

::

  (.) :: (b :p-> c) -> (a :q-> b) -> a :(p ':* q)-> c

.. _Syntax:

Syntax
~~~~~~

The new primary constructs are: multiplicities and the multiplicity
indexed arrow.

- Multiplicities are a datatype:

  ::

    data Multiplicity
      = One
      | Omega

  In addition, two specially recognised type families:

  ::

    type family (:+) :: Multiplicity -> Multiplicity -> Multiplicity
    type family (:*) :: Multiplicity -> Multiplicity -> Multiplicity

  In the following, for conciseness ``1`` for ``One`` and ``U``
  (ASCII) or ``ω`` (Unicode) for ``Omega``. Note: unification of
  multiplicities will be performed up to the semiring laws for
  ``(:+)`` and ``(:*)`` (see Specification_).
- The multiplicity annotated arrow, for polymorphism, is written
  ``a :p-> b`` (where ``a`` and ``b`` are types and ``p`` is a
  multiplicity). To avoid introducing a new notion of "mixfix"
  operators, we introduce a familly of (infix) type constructors:
  ``(:p->)`` for each multiplicity ``p``. This technically steals
  syntax as ``(:)`` is a valid type operator under the discouraged
  ``-XDataKinds`` syntax. But this should not be a problem in
  practice.

The linear and unrestricted arrows are aliases:

- ``(->)`` is an alias for ``(:'U ->)``
- ``(->.)`` (ASCII syntax) and ``(⊸)`` (Unicode syntax) are aliases
  for ``(:'1 ->)``

Constructors & pattern-matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constructors of data types defined with the Haskell'98 syntax

::

  data Foo
    = Bar A B
    | Baz C

have linear function types, that is ``Bar :: A ->. B ->. Foo``. This
is true in every module, including those without ``-XLinearTypes``
turned on. This implies that most types in ``base`` (``Maybe``,
``[]``, etc…) have linear constructors. We also make the constructor
of primitive tuples ``(,)`` linear in their arguments.

With the GADT syntax, multiplicity of the arrows is honored:

::

  data Foo2 where
    Bar2 :: A ->. B -> C

then ``Bar2 :: A ->. B -> C``

The definition of consuming a value in a data type exactly once must
be refined to take the multiplicities of fields into account:

- Consuming a value in a datatype exactly once means evaluating it to
  head normal form and consuming its *linear* fields exactly once

When pattern macthing a linear argument, linear fields are introduced
as linear variables, and unrestricted fields as unrestricted
variables:

::

  f :: Foo2 ->. A
  f (Bar2 x y) = x  -- y is unrestricted, hence does not need to be consumed

An exception to this rule is ``newtype`` declarations in GADT syntax:
``newtype``-s' argument must be linear (see Interactions_
below). For backward compatibility, we propose to make unrestricted arrows
``(->)`` in ``newtype``-s be interpreted as linear arrows, and create
a new warning ``unrestricted-newtype`` triggered when this happens.

Base
~~~~

Because linear functions only strengthen the contract of unrestricted
functions, a number of functions of ``base`` can get a more precise
type. However, for pedagogical reason, to prevent linear types from
interfering with newcomers' understanding the ``Prelude``, this
proposal does not modify ``base``. Instead we will release a library
exposing the stronger types for ``base`` functions. This effort has
been started `here <https://github.com/tweag/linear-base>`_.

This library will not redefine any type, and instead takes advantage
of the fact that data types in ``base`` are linear by default to
reuse the same types, hence remain compatible with base.

The only function which will need to change is ``($)`` because its
typing rule is built in the type checker. Ignoring the details about
levity and higher-rank polymorphism in the typing rule, the type
``($)`` will be:

::

  ($) :: (a :p-> b) ⊸ a :p-> b

The precise content of the library is out of scope of this proposal:
future standardisation of library content is the competence of
the CLC.  However the library will also contain convenient types to
work with linear types, with the understanding that when the new types
are standardised in ``base`` the library would re-export them rather
than define them, such as:

::

   data Unrestricted a where
     Unrestricted :: a -> Unrestricted a

.. _Multiplicities:

Multiplicities
~~~~~~~~~~~~~~

So far, we have considered only two multiplicities ``1`` and
``ω``. But the metatheory works with any so-called
sup-semi-lattice-ordered semi-ring (without a 0) of
multiplicities. That is: there a 1, a sum and a product with the usual
distributivity laws, a (computable) order compatible with the sum and
product, such that each pair of multiplicities has a (computable)
join. Even if there is only two multiplicities in this proposal, the
proposal is structured to allow future extensions.

Here is the definition of sum, product and order for this proposal's
multiplicities (in Haskell pseudo-syntax):

::

   _ + _ = ω

   1 * x = x
   x * 1 = 1
   ω * ω = ω

   _ ⩽ ω = True
   x ⩽ y = x == y

Every variable in the environment is annotated with its multiplicity,
which constrains how it can be used. A variable usage is said to be
of multiplicity ``p``, or ``0``, in a term ``u`` if:

- ``p=0`` and ``x`` is not free in ``u``
- ``p=1`` and ``u = x``
- ``p=p1+q*p2`` and ``u = u1 u2`` with ``u1 :: a :q-> b`` and the
  usage of ``x`` in ``u1`` is ``p1``, and in ``u2`` is ``p2``
- ``u = λy. v`` and the usage of ``x`` in ``v`` is ``p``.

A variable's usage is correct if it is smaller than or equal to the
multiplicity annotation of the variable. Incorrect usage results in a
type error.

The multiplicity of a variable introduced by a λ-abstraction is taken
from the surrounding typing information (typically a type annotation
on an equation). For instance

::

  foo :: A :p-> B
  foo x = …  -- x has multiplicity p

The above takes care of the pure λ-calculus part of Haskell. We also
need to consider ``let`` and ``case``.

A ``let`` binding is considered to have an implicit multiplicity
annotation (the annotation is inferred). The variables introduced by a
``let`` bindings with annotation ``p`` all have multiplicity
``p``. And the usage of ``x`` in ``let_p {y1 = u1; … ;yn = un} in v``
(where the ``yi`` are variables) is ``p*q1 + … + p*qn + q`` where the
usage of ``x`` in ``ui`` is ``qi`` and in ``v`` is ``q``.

If a let has recursive binders, then ``p`` must be ``ω``.

A ``case`` expression has an implicity multiplicity annotation, like
``let`` binding. It if often inferred from the type annotation of an
equation. The usage of ``x`` in ``case_p u of { … }`` where the usage
of ``x`` in ``u`` is ``q`` is ``p*q`` plus the *join* of the usage of
``x`` in each branch.  Note that, in usages, ``0 ≰ 1`` as arguments
with multiplicity ``1`` are consumed exactly once, which doesn't
include not being consumed at all.

The multiplicity annotation of variables introduced by a pattern depend
on the constructor and on the implicit annotation of the
``case``. Specifically in ``case_p u of {…; C x1 … xn -> …; …}`` Where ``C :: a1 :q1-> … an :qn-> A``,
Then ``xi`` has multiplicity annotation ``p*qi``. For instance

::

  bar :: (a,b) :p-> c
  bar (x,y) = … -- Since (,) :: a ->. b ->. (a,b), x and y have
                -- multiplicity p

Deep patterns & multiple equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO: deep patterns and multiple equations

Unboxed data types
~~~~~~~~~~~~~~~~~~

GHC supports unboxed data types such as ``(#,#)`` (unboxed pair) and
``(#|#)`` (binary unboxed sum). The proposal treats them as their boxed
equivalent (``(,)`` and ``Either``, respectively, for these two
examples): the constructors are linear (and case can have various
multiplicities).

Subtyping
~~~~~~~~~

The type ``A->.B`` is a strengthening of ``A->B``, but the type
checker doesn't do subtyping. It relies on polymorphism
instead. However, following the definition above, note that

::

  f :: A ->. B

  g :: A -> B
  g = f  -- should not be well-typed
  g x = f x  -- is well-typed

It would be unfortunate if this rule was actually enforced: for instance a linear function in a
library could not be used with ``map`` from base. Which means that
everybody would have to start caring about linearity. Worse: every use
of ``map Just`` would now be untyped. Fortunately, this sort of
opportunity is easily detected and the former definition of ``g`` is
understood as the latter, well-typed, one. It means that is not a
breaking change to strengthen a *first-order* regular arrow ``->``
into a linear ``->.`` in an interface.

Records and projections
~~~~~~~~~~~~~~~~~~~~~~~

Records constructors

::

   data R = R {f1 :: A1, … fn :: An}

are linear constructors: ``R :: A1 ->. … ->. An ->. R``. Projections
take an *unrestricted* record as argument: ``f1 :: R -> A1`` (because
otherwise the other fields would not be consumed). There is an
exception to this rule: if all the other fields are unrestricted (in
the current proposal, it means that ``f1`` is the *only* field, but
see `Binders with multiplicity`_), then ``f1`` is made linear:
``f1 :: R ->. A1``. This non-uniformity is justified by the standard
``newtype`` idiom:

::

  newtype Foo = Foo { unFoo :: A }

which becomes much less useful in linear code if ``unFoo :: Foo ->
A``. Our practice of linear Haskell code indicates that this feature,
while a mere convenience, is desirable (see *e.g.* `here
<https://github.com/tweag/linear-base/blob/e72d996b5d0600b2d5f2483b95b064d524c83e46/src/System/IO/Resource.hs#L59-L61>`_).

Inference
~~~~~~~~~

TODO

There are unresolved issues regarding inference (see `Unresolved
questions`_ below for a more precise description):

- There is no account of multiplicity inference. A better
  understanding would make inference more predictable.
- For ``let`` bindings and ``case`` expressions which are not part of
  an equation, we want to infer the multiplicity annotation. The
  process for this is not yet defined.

.. _Exceptions

Non-termination, exceptions & catch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In presence of non-termination or exceptions, linear functions may
fail to fully consume their argument. We can think of it as: the
consumption of the result of the function was never complete, so the
consumption of the argument need not be either. However, because
exceptions can be caught, a program can observe a state where a value
``v`` has been passed to a linear function ``f`` but the call ``f v``
has exited (with an exception) without consuming ``v``. So while, the
guarantee provided by linear functions holds for converging
computations, we must weaken it in case of divergence:

- Attempting to consume exactly once ``f v``, when ``f`` is a linear
  function, will consume ``v`` exactly once if the consumption of ``f
  v`` converges, and *at most once* if it diverges.

Where "consuming at most once" is defined by induction, like
"consuming exactly once", but every sub-consumption is optional.

In the paper, we gave a simplified specification of a linear ``IO``
monad (called ``IOL``) which ignored the issue of exception for the
sake of simplicity. Can we, still, write a resource-safe ``RIO`` monad
with linear types despite the added difficulty of exception? Yes, as
this section will show.

Concretely, how do we ensure that the sockets from the example API are
always closed, even in presence of exceptions? This boils down to how
the ``RIO`` monad is implemented. Below is a sketch of one possible
implementation of ``RIO`` (see `here
<https://github.com/tweag/linear-base/blob/master/src/System/IO/Resource.hs>`_
for a detailed implementation).


First, note that since Haskell program are of type ``IO ()``, we need a
way to run ``RIO`` in an ``IO`` computation, this is provided by the
function

::

  runRIO :: RIO (Unrestricted a) -> IO a

In order to achieve resource safety in presence of exception, ``runRIO``
is tasked with releasing any live resource in case of exception.

To implement this, ``RIO`` keeps a table of release actions, to be used
in case of exceptions. Each resource implemented in the ``RIO``
abstraction registers a release action in the release action table
when they are acquired.

If no exception occurs, then all resources have been released by the
program. In case of exception, the program jumps to ``runRIO``, which
releases the leftover resources.

An alternative strategy would be to add terminators on every resources
acquired in ``RIO``. Release in the non-exceptional case would still
be performed by the program, and the GC would be responsible for
releasing resources in case of exception. The release in case of
exception would be, however, less timely.

Can ``RIO`` have a ``catch``?
=============================

It is possible to catch exceptions inside of ``RIO``, but in order to
ensure resource safety, the type must be restricted:

::

  catchL :: Exception e
         => RIO (Unrestricted a) -> (e -> RIO (Unrestricted a)) -> RIO (Unrestricted a)

That is: no linear resource previously allocated can be referenced in
the body or the handler, and no resource allocated in the body or
handler can be returned. In effect, ``catchL`` delimits an new scope,
in which linear resources are isolated. To implement ``catchL``, we
simply give it its own release action table, so that in case of
exceptions all the local resources are released by ``catchL``, as
``runRIO`` does, before the handler is called. The original release
action table is then reinstated.

With this implementation it is clear that capturing linear resources
from the outside scope would compromise timely release, and returning
locally acquired resources would leak resources in case of exception.

The latter restriction can be lifted as follows: instead of
reinstating the original release action table in the non-exceptional
case, instate the *union* of the original table and the local one. In
this case the type of ``catchL`` would be the following:

::

  catchL :: Exception e
         => RIO a -> (e -> RIO a) -> RIO a

Even with this type, however, exception handling remains clumsy, and
it may prove more convenient to use a more explicit exception-management
mechanism for linear resources, such as the ``EitherT`` monad.

The choice between these two types (and corresponding implementation)
for ``catch``, or the absence of ``catch`` altogether, is a design
question for the library that implements a monad such as ``RIO``.

Can I throw linear exceptions?
==============================

In the type of ``catchL`` above, the type of the handler is ``e -> RIO
a``. Correspondingly, the type of the exception-throwing primitives are:

::

  throwRIO :: Exception e => e -> RIO a
  trow :: Exception e => e -> a

That is exceptions don't have linear payload.

While there does not seem to be any conceptual difficulty in throwing
exception with linear payload, we have noticed that, in practice, many
(linearly typed) abstractions which we have come up with rely on
values not escaping a given scope. Barring a mechanism to delimit the
scope of exceptions with linear payload, such linear exceptions may
compromise such abstractions.

To be conservative, and avoid potential such issue, we currently
consider exceptions as only carrying unrestricted payloads in our
library.

.. _Interactions:

Effect and Interactions
-----------------------

A staple of this proposal is that it does not modify Haskell for those
who don't want to use it, or don't know of linear types. Even if an
API exports linear types, they are easy to ignore: just imagine that
the arrows are regular arrows, it will work as expected.

Linear data types are just regular Haskell types, which means it is cheap
to interact with existing libraries. That is, unless there are linear
arrows in argument position. In which case, attempt to use a
non-linear function will raise a linear-type error. The motivating
examples are all like this: they are libraries which require linear
types to work.

There is an unpleasant interaction with ``-XRebindableSyntax``: ``if u
then t else e`` is interpreted as ``ifThenElse u t e``. Unfortunately,
these two constructs have different typing rules when ``t`` and ``e``
have free linear variables. Therefore well-typed linearly typed
programs can stop typing when ``-XRebindableSyntax`` is added.

The meta-theory of linear types in a lazy language fails if we allow
unrestricted ``newtype``-s:

::

  newtype Unrestricted' a where
    Unrestricted' :: a -> Unrestricted' a

Intuitively, this is because forcing a value ``v :: Unrestricted a``
has the consequence of consuming all the resources in the closure of
``v`` making it safe to use the value many times or not at all. But
newtypes convert ``case`` into a cast, hence the closure is never
consumed. So ``newtype`` must not accept non-linear arrow with
``-XLinearTypes``. These are interpreted as linear ``newtype``-s and a
warning is emitted (see Specification_ above).

Lazy pattern-matching is only allowed for unrestricted (multiplicity
``ω``) patterns: lazy patterns are defined in terms of projections
which only exist in the unrestricted case. For instance

::

  swap' :: (a,b) ->. (b,a)
  swap' ~(x,y) = (y,x)

Means

::
  swap' :: (a,b) ->. (b,a)
  swap' xy = (snd xy, fst xy)

Which is not well-typed in particular since fst is not.

::

  fst :: (a,b) -> a -- resp. snd
  fst (a,_) = a

So ``swap'`` must be given the type ``(a,b) -> (b,a)``.

Unresolved questions:

- It is unknown at this point whether view patterns can be linear
- It is unknown at this point whether ``@`` pattern of the form
  ``x@C _ _`` can be considered linear (it is as much a practical
  question of whether there is a reasonable way to implemet such a
  check as a theoretical question of whether we can justify it).
- There is no account yet of linear pattern synonyms.


Costs and Drawbacks
-------------------

Learnability
~~~~~~~~~~~~

This proposal tries hard to make the changes invisible to newcomers,
however, if many libraries start adopting it, the new function types
will appear in APIs. They can often be safely ignored, but they can
still be considered distracting.

Development and maintenance
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The arrow type constructor is constructed and destructed a lot in
GHC's internals. So there are many places where we have to handle
multiplicities. It is most often straightforward as it consists in
getting a multiplicity variable and pass it to a
function. Nevertheless, it is possible to get it wrong. And type
checker developers will have to be aware of multiplicities to modify
most aspects of type checking.

Linear types also affect Core: Core must handle linear types in order
to ensure that core-to-core passes do not break the linearity
guarantees. The flip side is that all core-to-core passes must make
sure that they do not break linearity. It is possible that some of the
pre-linear-type passes actually do break linearity in some cases (this
has not been acertained, yet).

Unification of multiplicity expressions (as for for instance in the
type of ``(.)`` above) requires some flavour of unification module
associativity and commutativity (AC). Unification modulo AC is
well-understood an relatively easy to implement. But would still be a
non-trivial addition to the type-checker. We may decide that a
simplified fragment is better suited for our use-case that the full
generality of AC.


Alternatives
------------

Syntax of multiplicity-parametric arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The proposed mixfix ``a :p-> b`` syntax for the
multiplicity-parametric arrow makes a potentially non-trivial addition
to the parser. So does the proposed type constructor indexed by a
multiplicity ``(:p->)``.

A way to simplify the changes to the parser would be to have the type
constructor be

::

  ARROW :: Multiplicity -> * -> * -- ignoring levity

It would be very inconvenient to use a prefix notation for
multiplicity parametric arrows: we wouldn't want the type of ``map``
to read

::

  map :: ARROW 'U (ARROW p a b) (ARROW p [a] [b])

So we introduce a binary type construction ``WithMult`` (or some
operator syntax). It is a syntax error to use ``WithMult`` anywhere
except to the left of an arrow. And ``WithMult a p -> b`` means
``ARROW p a b``. So that the type of ``map`` becomes:

::

  map :: (a `WithMult` p -> b) -> [a] `WithMult` p -> [b]

Lexical token of the linear arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We propose ``(->.)`` as a notation for the linear arrow. An
alternative, based on the resemblance with the Unicode notation
``(⊸)`` would be ``(-o)``.

We chose ``(->.)`` because it does not change the lexer (``-o`` is not
a token in current GHC, and ``a-o`` is currently interpreted as ``(-)
a o``), and because it is less intrusive, and more easily ignored by
newcomers who don't want to think about linear types.

.. _`Binders with multiplicity`

Binders with multiplicity
~~~~~~~~~~~~~~~~~~~~~~~~~

In the paper, we wrote ``λ x :₁ A, u`` for (unannotated) linear
functions. We don't currently provide a corresponding syntax, by lack
of good syntax.

If a syntax is provided, we could also use this syntax to have records
with different multiplicities.

::

  data R = R { unrestrictedField ::(ω) A, linearField ::(1) B }

.. _`Affine types`

Affine types rather than linear types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In presence of exceptions it may seem that linear functions do not
necessarily consume their arguments. For instance, an ``RIO a`` may
abort before closing its file handles. And because of ``catch`` we are
able to be observe this effect.

Since exceptions are an integral part of Haskell, and since guarantees
of linear functions are different in case of normal return or
exceptional return, it is appealing to call for less guarantees in all
cases.

A function is called *affine* if it guarantees that if its returned
value is consumed at most once, then its argument is consumed at most
once.

There are three possible system which we can consider:

1. A system with linear functions (as we are proposing)
2. A system with affine functions
3. A system with both linear and affine functions

All three system are consistent and can be easily accommodated in our
formalism. In fact the formalism has been designed with extensibility
in mind, and the proposed implementation is easy to change in order to
cope with affine functions. Therefore the choice between these three
systems is not a fundamental issue of this proposal. We are arguing
for system (1), but it can easily be changed.

We argue against system (2) because linearity guarantees still matter,
even if they are made more complex by exceptions. There are use-cases
where exceptions don't matter (such as @gelisam's `3D-printable models
<https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable>`_),
it would arbitrary to prevent them from using the linear types that
they need. Plus even in ``RIO`` code, where exceptions do matter,
linear types are useful: they allow prompt deallocation as argued in
the section on Exceptions_, it can be much harder to reason on the
lifetime of resources with explicit scopes like with ``bracket`` (see
the `inline-java use-case
<http://www.tweag.io/posts/2017-11-29-linear-jvm.html>`_ for an
example where scopes have proved to be unsatisfactory).

There is, nonetheless, value to affine types. There are some
applications where affine types are enough to enforce invariants (such
as in-place mutation of garbage-collected structure, like mutable
arrays). And they can presumably benefit from the additional
flexibility. For instance, ``catch`` can get a more fine-grained type
(writing ``'A`` for the affine multiplicity):

::

  catch :: Exception e => RIO a :'A-> (e -> RIO a) :'A-> RIO a

So affine mutable arrays could be free variables in the body of a
``catch``. It's not clear yet that this finer type for ``catch`` would
actually be useful: the same affine free variable could not appear
both in the body and the handler. The only instance of such a pattern
which we've found documented so far, is in the Alms programming
language, and the ``catch`` is merely used to perform clean-up and
re-raise (TODO check that it reraises + ref. in Jesse Tov's thesis),
we have abstracted this pattern away in the purely linear case. We
invite the community to come up with good examples of such use of
affine types.

While it is easy to make system (3), and we believe it would have
benefits, we haven't included it in the proposal, and rather propose
to stage it for a later proposal (see also `More multiplicities`_
below), and keep, in this proposal, the minimal system which addresses
the motivations.

TODO

- Discuss Roman's encoding?

Subtyping instead of polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since ``A ->. B`` is a strengthening of ``A -> B``, it is tempting to
make ``A ->. B`` a subtype of ``A -> B``. But subtyping and polymorphism
don't mesh very well, and would yield a significantly more complex
solution.

In general, subtyping and polymorphism are not comparable, and some
examples will work better with one or the other. Therefore it makes
sense to go for the simplest one.

In this proposal

::

  f :: A ->. B

  g :: A -> B
  g = f

is, in theory, ill-typed. But it would be a problem to reject this
program (especially with all the constructors which have been
converted to linear types). So the type inference mechanism elaborates
this program to the well-typed η-expansion

::

  f :: A ->. B

  g :: A -> B
  g x = f x

This also work at higher arity, including mixed of linear and
non-linear arguments:

::

  f' :: A ->. B -> C ->. D

  g :: A -> B -> C -> D
  g = f
  -- is interpreted as:
  -- g x y z = f x y z

Zero as a multiplicity
~~~~~~~~~~~~~~~~~~~~~~

The implementation, and the usage-based definition of linearity in the
Multiplicities_ section, use a ``0``. It is currently kept out of the
actual multiplicities because we have no use case for this. But it
would not be hard to provide. Additionally, ``0`` has been used by
`Conor McBride
<https://link.springer.com/chapter/10.1007/978-3-319-30936-1_12>`_ to
handle dependent types, which may matter for Dependent Haskell.

An alternative which we may consider, or which we may take into account
when Dependent Haskell progresses, would be to have the multiplicity
``0`` as an additional multiplicity.

The definitions of sum, product and order would have to be modified as
follows:

::

   0 + x = x
   x + 0 = x
   _ + _ = ω

   0 * _ = 0
   _ * 0 = 0
   1 * x = x
   x * 1 = 1
   ω * ω = ω

   _ ⩽ ω = True
   x ⩽ y = x == y

Note in particular that ``0 ≰ 1``.

An important point to note, however, is that ``case_0`` is
meaningless: it makes it possible to create values dependending on a
value which may not exist at runtime. For instance the length of a
list argument with multiplicity ``0``.

::

  -- Wrong!
  badLength :: [a] :'0-> Int
  badLength [] = 0
  badLength (_:l) = 1 + badLength l

  -- Not linear! But well-typed if the above is accepted
  f :: [a] ->. (Int, [a])
  f l = (badLength l, l)

Because we want to allow ``case_p`` for a variable ``p``, this
creates a small complication. Which can be solved in a number of way:

- Make it so that multiplicity variables are never instantiated by
  ``0``, in particular type-application of multiplicity variables must
  prohibit ``0``.
- Instead of restricting variables and type applications so that
  ``case_p`` is allowed for a variable ``p``, we can allow arbitrary
  variables and disallow, in particular, ``case_p``.

  In this case, we would have:

  ::

     map :: (a :(p+1)-> b) -> [a] :(p+1)-> [b]
     map f [] = []
     map f (a:l) = f a : (map f l)

  In practice, under this situation, the type of ``map`` is probably better
  written as

  ::

     map :: forall p a b q. (p ~ q + 1) => (a :p-> b) -> [a] :p-> [b]

  In order to play more nicely, for instance, with explicit type
  applications.

  A benefit is that higher-order functions with no ``case`` such as
  ``(.)`` are now capable of taking functions with multiplicity ``0`` as
  argument.
- A variation on the same idea is to introduce a constraint

  ::

    CaseCompatible :: Multiplicity -> Constraint

  which is discharged automatically by the compiler. Variables
  implementing this are acceptable in ``case``. So ``map`` would be of
  type.

  ::

    map :: (CaseCompatible p) => (a :p-> b) -> [a] :p-> [b]

  This is harder to implement than just reusing ``p~q+1`` as a
  constraint, but is more resistant to having more multiplicities than
  just 0, 1, and ω, as is currently proposed.
- Another option is to have a type of multiplicities *excluding* ``0``
  and have another type of extended mulitplicities for multiplicities
  with ``0``. Note that a different ``(+)`` and ``(*)`` would have to
  act on extended multiplicities.

.. _`No annotation on case`

No annotation on case
~~~~~~~~~~~~~~~~~~~~~

Instead of having ``case_p`` (see Multiplicities_) we could just have the
regular ``case`` (which would correspond to ``case_1`` in this
proposal's formalism). This would simplify the addition of ``0``.

On the other hand, doing this loses the principle that linear data
types and unrestricted data types are one and the same. And sacrifices
much code reuse.

Unicity instead of linearity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Languages like Clean and Rust have a variant of linear types called
uniqueness, or ownership, typing. This is a dual notion: instead of
functions guaranteeing that they use their argument exactly once, and
no restriction being imposed on the caller, with uniqueness type, the
caller must guarantee that it has a non-aliased reference to a value,
and the function has no restriction.

Where unicity really shines, is for in-place mutation: the ``write``
function can take a regular ``Array`` as an argument, it just needs to
require that it is unique. Freezing is really easy: just drop the
constraint that the ``Array`` is unique, it will never be writable
again.

With linear types, we need to have two types ``MArray`` (guaranteed
unique) and ``Array``, just like in Haskell today. This is fine when
we are freezing one array: just call ``freeze``. But what if we are
freezing a list of arrays? Do we need to ``map freeze``? This is
unfortunate (the problem is even more complicated if we start
considering ``MArray (MArray a)``). It has a feel of ``Coercible``,
but it does feel harder.

On the other hand, other examples work better with linear types, such
as fork-join parallelism. This is why Rust has a notion of so-called
mutable borrowed reference, on which constraints are more akin to
linear types (or rather, affine types, technically).

Overall, uniqueness type system are significantly more complex to
specify and implement than linear types systems such as this
proposal's.

Linearity-in-kinds
~~~~~~~~~~~~~~~~~~

Instead of adding a type for linear function, we could classify types
in two kinds: one of unrestricted types and one of linear
types. A value of a linear type must be used in a linear fashion.

This would get rid of the continuation of ``newMArray`` in the
motivating ``MArray`` interface.

The most natural way to do this, in Haskell, is to add a second
parameter to ``TYPE`` (the first one is for levity polymorphism). So,
ignoring the levity polymorphism, we would have ``TYPE '1`` for linear
types and ``TYPE 'U`` for unrestricted type. We get polymorphism by
abstracting over the multiplicity.

As interesting as it is, there is quite some complication associated
to it. First, because of laziness, you can't have a function of type
``(A :: TYPE '1) -> (B :: TYPE 'U)`` (because you don't need to
consume the result, hence you may not consume an argument that you
have to consume). So what would be the type of the arrow? Something
like ``forall (p :: Multiplicity) (q ⩽ p). p -> q -> q``. So we're
introducing some kind of bounded polymorphism in our story. This is
quite a bit harder than our proposal.

Most types will live in both kinds, but that would have to be
explicit:

::

  data List (p :: Multiplicity) (a :: TYPE p) :: TYPE p where
    [] :: List p a
    (:) :: a -> List p a -> List p a

Mixing non-linear and linear lists (*e.g.* with ``(++)``) would
require either some subtyping from ``List 'U a`` to ``List '1 a`` (but
as discussed above, subptyping in presence of polymorphism quickly
becomes hairy) or some conversion function.

It it worth taking into account that the issues with ``MArray`` and
``Array`` (which may be ``Array '1`` and ``Array 'U`` in this case)
above are not solved by such a situation. Unless there is a subptyping
relation from ``Array 'U`` from ``Array '1``, which cannot be performed
by an explicit function since this would be equivalent to the
proposal's situation.

On the other hand, the CPS interface to ``newMArray`` delimits a scope
in which the array lives. This gives a perfect opportunity to put
clean-up code to react to exceptions. So it may not be such a bad thing
after all.

So linearity in kind seem to add a lot of complication for very little
gain.

On the matter of dependent Haskell, to the best our knowledge, the only
presentations of dependent types with linearity-in-kinds disallow
linear types as arguments of dependent functions.

Additive conjunction
~~~~~~~~~~~~~~~~~~~~

There is a connective of linear logic which is not included in this
proposal: the additive conjunction, typically written ``A&B``. It
differs from the multiplicative conjunction (written ``A⊗B`` in linear
logic, and ``(A, B)`` in Linear Haskell) in that it has two *linear*
projections ``π₁ :: A&B ->. A`` and ``π₂ :: A&B ->. B`` but, contrary
to the multiplicative conjunction, only one of the two conjuncts of a
linear ``A&B`` will be consumed (that is: consuming a value ``u`` of
type ``A&B`` exactly once, means consuming ``π₁ u`` exactly once, or,
*exclusively*, consuming ``π₂ u`` exactly once).

It is not part of the proposal because it can be encoded:

::

  type a & b = forall k. Either (a ->. k) (b ->. k) ->. k

What could be a benefit of having a primitive support for ``A & B``?
Values of type ``A&B`` could be implemented as a lazy thunk rather
than a function. But this only really matters for unrestricted values,
but in this case, the role of lazy pair is already played by
``Unrestricted (A, B)`` (due to our treatment of ``case``, see `No
annotation on case`_).

On the other hand we believe additive pairs of effectful computations
to be more useful in effectful context. In which case we would use:

::

  type a & b = Either (a ->. ⊥) (b ->. ⊥) ->. ⊥

For some effect type ``⊥`` (it could be ``type ⊥ = RIO ()`` for
instance).

So on balance, we didn't consider additive pairs to be useful enough
to justify a dedicated implementation and syntax.

Future extensions (not part of this proposal)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Toplevel-linear binders
+++++++++++++++++++++++

Something that hasn't been touched up by this proposal is the idea of
declaring toplevel linear binders

::

  module Foo where
  token ::('1) A  -- made up syntax

Here ``token`` would have be consumed exactly once by the program,
this property is a link-time property. This generalised the
``RealWorld`` token which is currently magically inserted in the
``main`` function (the existence of which is checked at link time).

This would allow libraries to abstract on ``main`` or to provide their
own linearly-threaded token.

.. _`More multiplicities`

More multiplicities
+++++++++++++++++++

One central aspect of the proposed system is that it is very easy to
extend with new multiplicities: add a multiplicity to the
``Multiplicity`` data-type, extend the sum, product, ordering, and
join functions.

As discussed in the `Affine types`_ section, one such extra
multiplicity is the multiplicity of affine functions (which is both
the join of ``0`` and ``1``). The `paper
<https://arxiv.org/abs/1710.09756>`_ also suggests a "borrowing"
multiplicity which would allow for arbitrary usage, but be strictly
smaller than ``ω``.

It is not clear what the eventual list of multiplicity should be. The
literature teaches us that multiplicities classify co-effects, of
which there are many.

Instead of trying to come up with a definite list of multiplicities
which ought to be built in, we hope to be able to propose a solution
to make it possible for libraries to define new multiplicities.

.. _Core

The Core corner
---------------

*This section is an appendix to the proposal describing the changes
to GHC's Core intermediate language in order to accommodate the new
feature of this proposal*

TODO

.. _`Unresolved questions`

Unresolved questions
--------------------

Inference
~~~~~~~~~

- There is no systematic account of type inference. Can it be made
  predictable when a type annotation is required? For compatibility
  reasons, we want to infer unrestricted arrows conservatively, but
  experience shows that it can result in very surprising type errors.

- In the formalism, case expressions are indexed by a multiplicity:
  ``case_p`` (and similarly ``let_p``). In the surface language, we
  can deduce the multiplicity in equations when their is a type
  annotation.

  ::

    fst :: (a,b) -> a
    fst (a,_) = a    -- this is inferred as a case_ω

    swap :: (a,b) ->. (b,a)
    swap (a,b) = (b,a)   -- this is inferred as a case_1

  But what of explicit ``case`` and ``let`` in the surface language? We
  can annotate them with a multiplicity, but it is generally clear from
  the context which multiplicity is meant. So the multiplicity
  annotation really ought to be inferred. The general idea is: if
  their is any linear variable in the scrutinee, then the case must be
  linear, and if there are only unrestricted variables, it can be
  unrestricted. Is it sound to always pick the highest possible value ?
  What if there are multiplicities with variable multiplicity ?

Patterns
~~~~~~~~

It is not clear yet how the following should be handled:

- View patterns: linear view patterns should not be a problem as long
  as there is only one view and that the patterns are grouped into a
  single call to the view (otherwise the patterns would translate, in
  Core, to several calls using the same linear variable, which is not
  allowed). It is not clear yet that we can have a predictable
  criterion which would allow programmers to use linear view
  patterns without generating faulty Core. On the other hand, it would
  be unfortunate not to have linear view patterns at all, as views
  matter more in linear types as there are usually no projections.
- ``@``-patterns: The pattern ``x@(Just _) -> …`` could be seen as
  linear. After all, it is equivalent to ``Just y -> let x = Just y in
  …``. It is not clear that we can make the linearity checking in Core
  accept this sort of patterns (see also the Core_ section above).
- Pattern synonym: linear pattern synonyms have not been studied
  yet. In particular, how they ought to be type checked, when they are
  defined. It is still unknown whether this problem is hard or easy.

Syntax
~~~~~~

Linear monads, like ``RIO`` in the socket motivating example will
require the ``do`` notation to feel native and be comfortable to
use. There is a facility to do this ``-XRebindableSyntax`` but,
besides the problem with ``itThenElse`` mentionned above, this has a
much too coarse grain behaviour: realistically, the same file will
want to mention regular monads and linear monads (there is also
another useful type of monads where multiplicity can change), but
``-XRebindableSyntax`` changes the meaning of ``do`` globally. A
solution would be to have a locally-rebindable ``do`` syntax such as
is attempted in `this proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/78>`_.

Core
~~~~

In Core, ``case`` is of the form ``case u as x of { <alternatives> }``
where ``x`` represents the head normal form of ``u``. It is used by
the compiler in some Core to Core passes. It is also how default
alternatives of a case are implemented:

::

  fmap' :: (a -> a) -> Maybe a -> Maybe a
  fmap' (Just x) = Just (f x)
  fmap' y = y

is elaborated into

::

  \f o -> case_ω o as y of { Just x -> Just (f x) ; WILDCARD -> y }

But it is not obvious what to do for linear cases. The following is a
linearity violation as ``y`` in a sense contains ``x`` (basically, you
could define a function ``a ->. (a,a)`` generically with this).

::

  case_1 o as y of { Just x -> Just (x,y) }

So we need a simple story (Core needs to stay fairly simple) for the
``as`` clause of linear cases.

The easiest thing to do would be to type ``case_p u as y of { … }`` as
``let_p y = u in case y of { … }``. But this may not be a good idea:
it would prevent default cases, or legitimate patterns such as
``x@(Just _)`` from being considered linear. It may also make some
compiler passes harder than they ought to.

After all, there is a transformation for ``x@(Just _) -> u`` which
makes is a linear program: ``Just y -> let_1 x = Just y in u``. And
the latter program has precisely the same behaviour.

It is not known at this point whether the simpler typing rule would be
an obstacle or whether it is worth it to have a more fine-grained
typing.

Solving this will have user-facing implications, in particular regarding
which view patterns and ``@``-patterns are available in linear
functions.

Implementation Plan
-------------------

- @aspiwack will implement the proposal
- @aspiwack will implement and release a library exporting standard
  functions and types for linearly typed programs.
