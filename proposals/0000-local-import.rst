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

We propose to allow the use of ``import`` statements in any ``let``, ``where`` or ``do`` block. The effect of those imports is limited to the corresponding block and (for ``let`` and ``where`` blocks) to the defined expression. Moreover, we define a shortcut syntax, ``Qualifier.{ … }``, to allow scoping a local import over a single expression.

For example, the following snippets are considered valid:

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
        import System.Exit as Exit
        Exit.{ exitWith $ ExitFailure (-1) }

Motivation
------------

The motivations are the following:

- Making import lists smaller and easier to maintain.
- Making it easier to understand why a module is imported.
- Making it easier to refactor a definition, along with its imports.
- Making it more practical to use qualified imports, especially with operators.

Import lists in Haskell can quickly become unwieldly. The recent proliferation of GHC proposals intending to simplify module imports is good evidence that this feeling is widely shared.

Oftentimes, a module is only imported for a single symbol or two. The purpose of such imports is not obvious at first glance and can significantly clutter the import list. Moving those imports closer to the use-site would solve both of these problems by making the intent clearer and removing them from the toplevel list. Moreover, definitions would be easier to refactor, as they would be more self-contained ; for example, it would be easy to remove a definition along with its specific imports.

To avoid polluting the namespace with conflicting identifiers, Haskell programmers can choose between explicit and qualified imports ; however, they are often reluctant to do so as:
- Explicit import lists are inconvenient (they must be edited whenever the code changes). 
- Module qualifiers can severely affect readability.

The latter problem is often addressed by choosing one-letter qualifiers, which can only be understood by looking up their definition at the top of the file. It is an imperfect solution at best, and does not work well for operators ; for this reason, operators are frequently overloaded via lawless typeclasses for mere syntactic convenience.

OCaml 3.12, released in 2011, introduced two syntactic contructs for locally opening modules: ``let open Module in …`` and ``Module.( … )``. In less than 10 years, this feature has gained widespread adoption throughout the ecosystem, bringing significant benefits in terms of readability and maintainability. For the reasons stated above, we believe Haskell should follow its example.

Proposed Change Specification
-----------------------------
We allow qualified and non-qualified ``import`` statements at the beginning of any ``let``/``where``/``do``-block.

For all constructs, the effect of such imports is limited to the scope the enclosing block. For ``let`` and ``where``, the imports also affect the corresponding expression. For example:
::

  let import Foo in <expr>
allows the use of symbols from ``Foo`` in the ``<expr>``. Similary, the same thing is possible with:
::

  f = <expr>
    where import Foo

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

Finally, we allow the syntactic shortcut ``Qualifier.{ <expression> }``, which simply desugars to:
::

  let import Qualifier in <expression>

The following changes in the Haskell 2010 grammar are required:

::

  decls → { impdecls ; decl_1 ; … ; decl_n }    (n ≥ 0)
       | { decl_1 ; … ; decl_n }
  lexp → …
       | modid.{ lexp }
       | do { stmts }
       | do { impdecls ; stmts }


Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features. 


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
To our knowledge, there is no other language feature or extension providing similar benefits.


Unresolved Questions
--------------------
TODO: Talk about type-level local import ?

Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
