Local import
==============

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

This proposal allows the use of ``import`` statements in any ``let``, ``where`` or ``do`` block. The scope of those imports is limited to the corresponding block and (for ``let`` and ``where`` blocks) to the defined expression. Moreover, it defines a shortcut syntax, ``Qualifier.{ … }``, to allow scoping a local import over a single expression.

For example, the following snippets are considered valid:

::

  module MyModule where

  import Data.Set (Set)
  import qualified Data.Set as Set

  foo s t u = Set.{ s \\ (t \\ u) }

::

  module MyModule where

  import Data.Set (Set)
  import qualified Data.Set as Set
  
  foo :: (Set Int, Set String)
  foo = (s1, fromList ["foo", "bar", "baz"])
    where
      import Set
      s1 = fromList [1, 2, 3]

::

  module Main where
  import System.Environment as Env
   
  main :: IO ()
  main = do
    args <- Env.getArgs
    case args of
      x:y:_ -> …
      _ -> do
        import qualified System.Exit as Exit
        Exit.{ exitWith $ ExitFailure (-1) }

This proposal is strongly inspired by OCaml's *local open* feature, which introduces two constructions: ``let open Module in …`` and ``Module.( … )``, both with the semantics of importing the namespace ``Module`` (unqualified) in the subsequent expression. It was introduced in 2011 with version 3.12; in less than 10 years, local open has gained widespread adoption throughout the ecosystem, and brought significant benefits in terms of readability and maintainability.

Motivation
------------

As witnessed by a recent proliferation of proposals (1_, 2_, 3_), there is a growing desire, in the community, to improve module imports.

This is yet another of these proposal, with the specific aim of shrinking the trade-off between qualified and unqualified imports:

- Unqualified imports
  - Obscure the provenance of identifiers
  - Have a tendency to conflict with each other
  - Unless, in both cases, one uses import lists, but import list are costly to maintain
- Qualified imports
  - Make for long, repetitive identifiers
  - Make it unpleasant to use binary operators (to borrow an example from the introduction, we may not want to write ``s Set.\\ (t Set.\\ u)``)

Workarounds for the shortcomings qualified imports include
- Using one-letter qualified import. It certainly makes for short enough names, but, even ignoring the paucity of one-letter (or even two-letter) identifier, it can quickly become an exercise in obfuscation: what letters should one choose to import modules ``Set``, ``Selectors``, ``Settings`` in the same place?
- One-letter qualification also doesn't address the case of operators. Operators can be extremely helpful for readability, but qualified operators don't seem to register nearly as well. An option, here, is to only define operators in (lawless) type classes, so that these type classes can be imported unqualified. Individual module can instantiate these type classes instead of exporting operators. This has the cost of obscuring the origin of the operator.

The solution introduced by this proposal is, instead, to import modules qualified, *but to locally unqualify them where it's relevant*.

So that instead of

::

  let x = s Set.\\ (t Set.\\ u)

we can write

::

  let x = Set.{ s \\ (t \\u) }

or

::

  let
    import Set
    x = s \\ (t \\ u)

.. _1: https://github.com/ghc-proposals/ghc-proposals/pull/190
.. _2: https://github.com/ghc-proposals/ghc-proposals/pull/205
.. _3: https://github.com/ghc-proposals/ghc-proposals/pull/220

Proposed Change Specification
-----------------------------

Qualified and non-qualified ``import`` statements are allowed at the **beginning** of any ``let``/``where``/``do``-block.

For all constructs, the effect of such imports is limited to the scope of the enclosing block. For ``let`` and ``where``, the imports also affect the corresponding expression. For example:
::

  let import Foo in <expr>
allows the use of symbols from ``Foo`` in the ``<expr>``. Similary, the same thing is possible with:
::

  f = <expr>
    where import Foo
The set of visible typeclass instances at any given point is the union of instances defined in modules imported in all enclosing scopes.  

Moreover, ``import`` statements are allowed to refer to any module qualifier specified outside that block (that is, local ``import`` statements are not limited to full module names). This means that the following is allowed:
::

  import qualified Data.Set as Set
  foo = …
    where import Set
          …
As well as:
::

  foo = …
    where import qualified Data.Set as Set
          bar = …
            where import Set
                  …
But the following is not valid:
::

  foo = …
    where import qualified Data.Set as Set
          import Set
This is consistent with today's semantics for ``import``, which does not allow for:
::

  module MyModule where

  import qualified Data.Set as Set
  import Set

As is already the case, module qualifiers can be re-used. For example, in:
::

  import Foo as A

  main :: IO ()
  main = do
    import Bar as A
    A.x

The symbol ``x`` in ``A.x`` is searched in both ``Bar`` and ``Foo``.

Finally, the syntactic shortcut ``Qualifier.{ <expression> }`` is introduced, which simply desugars to:
::

  let import Qualifier in <expression>
The OCaml syntax, ``Qualifier.( … )``, was not chosen as it would steal syntax (this is currently parsed as a value constructor composed with the enclosed expression).
   
The following changes in the Haskell 2010 grammar are required:

::

  decls → { impdecls ; decl_1 ; … ; decl_n }    (n ≥ 0)
        | { decl_1 ; … ; decl_n }
  lexp  → …
        | modid.{ lexp }
        | do { stmts }
        | do { impdecls ; stmts }


Effect and Interactions
-----------------------
This proposal strictly extends the language, without affecting the behavior of existing code. It does not interact with any existing language extension.

The changes give programmers various ways to reduce the number of toplevel imports, to limit their effect to specifics parts of the code and to convey intent about their uses. The shortcut syntax can be especially useful for scoping module imports over expressions with operators in DSLs.

Furthermore, it encourages the use of qualified imports, as those can be locally “de-qualified” in order to improve readability.

Costs and Drawbacks
-------------------
These changes should be easy to grasp by beginners. Moreover, they could greatly improve learnability of Haskell libraries, by expliciting the module each function comes from in examples and tutorials.

Some existing tooling (e.g., ``snack``) assume that imports are only found at the toplevel and might be broken by this change. Perhaps more importantly, this change would make it harder for IDE-like tools such as ``hie`` to determine the set of valid completions ; such tools would need to be made context-sensitive, like OCaml's merlin.

This change would make it harder to determine at first glance inter-dependencies between modules.

Finally, some library writers might choose to design their library around this extension. Using such libraries without this extension enabled might be inconvenient, which could be perceived as a drawback by some users.

Alternatives
------------
To our knowledge, there is no other language feature or extension providing similar benefits.

It might be valuable to also allow some form of typelevel local import ; the shortcut syntax, in particular, could be used to simplify type signatures.

The syntactic shortcut syntax is orthogonal to the rest of the proposal and could be entirely removed. The same functionality could potentially be achieved with QuasiQuoters.

As an extension to the proposed behavior, local imports could be used to shadow globally-defined symbols. As an example, the ``blaze-html`` library provides symbols for ``head``, ``div`` and ``id`` ; for this reason,  the relevant modules are frequently imported qualified, or those symbols are explicitly hidden with ``-XNoImplicitPrelude`` and an explicit import. This is necessary for preventing uses of those symbols to be reported as ambiguous by the compiler. Without type-driven disambiguation, this is the only sane behavior in current Haskell, which only allows a single, unordered list of module imports ; however, local imports could be seen as defining nested scopes, such that:
::

  {-# LANGUAGE OverloadedStrings #-}
  import Text.Blaze.Html4.Strict as Blaze
  import Text.Blaze.Html4.Strict.Attributes as Blaze

  markup :: Html
  markup = head $ div ! id "foo"
    where import Blaze
compiles without error. Similarly, in the following example:
::

  import Foo as A

  main :: IO ()
  main = do
    import Bar as A
    A.x
If ``x`` is defined in both ``Foo`` and ``Bar``, the import from ``Bar`` could take precedence over the one from ``Foo``. Finally, DSLs could benefit from this change to override arithmetic operators without implementing bogus ``Num`` instances.

In order to still allow programmers to easily determine the set of imported modules by looking at the top of the file, local imports could be restricted to qualified imports, and possibly allowed to rename already imported modules. Here is an example of both:
::

  import qualified Data.Map as Map
  import Foo

  foo = …
    where import Map
          import Foo as Bar
The obvious drawback of this solution is that it goes against one of the stated motivations of this proposal: to reduce the size of import lists. It merely makes it easier to work with qualified imports.

Finally, the effects of local imports in ``let`` and ``where`` blocks could be restricted to the set of underlying definitions, and not scope over the defined expression. This design was considered and rejected, as it would likely give rise to the following idiom:
::

  foo x = y
     where import Bar
           y = Bar.z x
instead of the lighter:
::

  foo x = Bar.z x
    where import Bar

Unresolved Questions
--------------------
None at this point.

Implementation Plan
-------------------
To be determined.
