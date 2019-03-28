Import qualified by default
===========================

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

We propose a new extension to GHC ``QualifiedImports`` which switch the default behavior of import from unqualified to qualified. Unqualified modifier will appear after the module name. We also propose to use the module name in a type context to reference the "main" type of the module.

In short::

  import qualified Data.HashMap as HashMap
  import           Data.Ord
  import Data.HashMap (HashMap)


  foo :: HashMap.HashMap -> HashMap.HashMap
  foo = ...

will become::

  import Data.HashMap as HashMap
  import Data.Ord unqualified

  foo :: HashMap -> HashMap
  foo = ...


Motivation
------------

We'd like to ease programmer usage of module by simplifying the import syntax with the following changes:

- Import should be qualified by default.
- Unqualified imports should be specified after the module name.

Qualified as default
~~~~~~~~~~~~~~~~~~~~

A lot of haskell modules are designed to be imported qualified, (e.g. ``vector``, ``containers`` have conflicting bindings), however, by default the haskell language import unqualified.

By being the solution of less effort, developers are usually importing unqualified first, implying that:

* Without tooling, it is difficult to know the origin of a function
* Symbol conflicts can appear when a new module is imported unqualified or when a module introduces a new symbol. In context of the Haskell package versioning policy (PVP), a new binding is not considered a breaking change, but it can lead to symbol conflict if it appears in a module which is imported unqualified.
* Developers usually switch to qualified import later and need to qualify all their name. Which is painful without tooling and generate important diffs.

We propose to fix this issues by importing qualified by default::

  import Module

will have the same meaning as the current syntax::

  import qualified Module

Unqualified appears after the module name
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For alignment consistency, many authors are aligning module name, thus leaving indentation spaces for the possible ``qualified`` modifier, such as::

  import qualified Data.ByteString as ByteString
  import           Data.Ord

This leads to sorting considerations (are we sorting by qualified/unqualified status) and surprising presence of spaces. Without tooling, this spacing must be inserted manually.

We propose to move the ``unqualified`` modifier after the name, such as::


  import Data.ByteString as ByteString
  import Data.Ord unqualified

The part of the proposal is similar to this one: https://github.com/ghc-proposals/ghc-proposals/pull/190, but they only focus on changing the modifier position and not the default qualification strategy.


Module name as type
~~~~~~~~~~~~~~~~~~~

We observed that a lot of modules have a "main" type which is usually named as the last component of the module (e.g. ``Data.HashMap`` exports ``HashMap``). A related common idiom is to import qualified a module and then import unqualified a type from that module::

  import Data.ByteString (ByteString)
  import qualified Data.ByteString as ByteString

This way developers get ``ByteString`` the module and ``ByteString`` the type in scope. This is usually done to avoid the repetitive `ByteString.ByteString` reference.

This pattern is so common in Haskell libraries that we want to reduce the associated boilerplate. We propose to import unqualified the type which have the same name as name given to the imported module. For example, the following qualified import::

  import Data.ByteString as ByteString -- This is a qualified import due to this proposal change

will also import ``Data.ByteString.Bytestring`` as ``ByteString``.

Note that the imported type will share the same name as the imported module, but they live in two different namespace, module are not used directly at the type level.

Also, there is no actual consensus on the alias to use when importing, for example we observes the followings:

- ``import Data.ByteString as B``
- ``import Data.ByteString as BS``
- ``import Data.ByteString as ByteString``
- ``import Data.ByteString as StrictByteString``


This makes source code reading usually difficult. We think that automatically importing the "main type" of a module based on module alias name will give developers a default choice.

Proposed Change Specification
-----------------------------

A new language ``QualifiedImports`` is introduced. When enabled, it:

* remove the ``qualified`` modifier from ``import`` syntax
* introduce the ``unqualified`` modifier to the ``import`` syntax, after the module name, but before the optional ``as`` and binding import list.

This can be summarized by this change to the import grammar. From::

  importdecl :: { LImportDecl GhcPs }
     : 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec

we change it to::

  importdecl :: { LImportDecl GhcPs }
     : 'import' maybe_src maybe_safe maybe_pkg modid optunqualified maybeas maybeimpspec

We'll then also:
     
* changes the default behavior of ``import`` to qualified imports. "Naked" import statement will be qualified, and unqualified imports will be done using the ``unqualified` modifier.
* automatically import the "main type" with the same name as the qualified module if it exits.


Effect and Interactions
-----------------------

This proposal changes the default behavior of the import statement and slightly changes its syntax. Incidently, this will simplify the import list and will orient new developments into qualified import by default.

The automatic import of the "main type" will reduce boilerplate. We also hope that it will introduce a preferred name for import alias.

Other than that, we don't see any other interactions with the language. The new syntax will however have an impact on tools which parses haskell for import statement which have to be updated.

Costs and Drawbacks
-------------------

The implementation cost is a few lines of changes in the parser and in the import behavior. The automatic import of the "main type" needs limited changes to the import mechanism.

The automatic import of type using the same name as the module alias may disallow future extension of the language where module may be used in a type context.

This extension changes the semantic of Haskell import statement, so it cannot be switched on without changes to all the import statements of a module, but this operation is straightforward and won't surprise module authors.

Alternatives
------------

1. We may not implement the "main type" import feature
2. The syntax for ``unqualified`` import can be different. The proposed syntax is ``import ModuleName unqualified (as Foo)``, the following alternatives are possible:

   - ``unqualified`` before the module name: ``import unqualified ModuleName``
   - ``unqualified`` is `as`z; ``import ModuleName as unqualified``.
3. The definition of the main type. We choose to type named as the module alias, so ``import Data.ByteString as Foo`` will try (and fail) to import unqualified ``Data.ByteString.Foo``. However we may also:
   - import the type with the same name as the last component of the fully qualified module name. ``import Data.Container as Storage`` will import `Data.Container.Container`.
   - introduce a new syntax on module export list to specify the main type, for example::

       module Data.HashMap.Strict (
         main HashMap,
	 ...
       )

Unresolved Questions
--------------------

None yet.


Implementation Plan
-------------------

I, @guibou, volunteer to do this job with a bit of mentoring from tweag collegues. I don't think that's difficult, we need to:

- introduce the extension, that's fairly straightforward.
- change the grammar to introduce ``unqualified`` and remove ``qualified``. It is a minor change in the parser.
- change the import logic to import qualified by default. This is also a minor change.
- import the "main type" logic. This may be the most difficult part of this implementation, but nothing dramatic.
