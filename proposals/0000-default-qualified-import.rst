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

We propose a new extension to GHC ``QualifiedImports`` which switchs the default behavior of import from unqualified to qualified. Unqualified modifier will appear after the module name. We also propose to use the module name in a type context to reference the "main" type of the module.

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

We'd like to ease programmer usage of module by simplifying the import syntax with three changes:

- Import should be qualified by default.
- Unqualified imports should be specified after the module name.
- Module "given" name (using ``as``) should be usable as a type, representing the "main" type provided by the module.

Qualified as default
~~~~~~~~~~~~~~~~~~~~

A lot of haskell modules are designed to be imported qualified, ``vector``, ``containers``, ``...``, however, by default the haskell language import unqualified.

By being the solution on less effort, developers are usually importing unqualified first, implying that:

* Without tooling, it is difficult to know the origin of a function
* Symbol conflict can appear when a new module is imported unqualified or when a module introduces a new symbol. In context of the Haskell package versioning policy (PVP), a new binding is not considered a breaking change, but it can lead to symbol conflict if it appears in a module which is imported unqualified.
* Developers usually switch to qualified import later and need to qualify all their name. Which is painful without tooling and generate important diffs.

By doing this switch, we forces users to make the right choice by default.

.. TODO: survey of how it's done in many other languages
  - python: qualified by default
  - c++: namespace are not flattened by default
  - ....


Unqualified appears after the module name
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For alignment consistency, many authors are aligning module name leaving space for the ``qualified`` modifier, such as::

  import qualified Data.ByteString as ByteString
  import           Data.Ord

This leads to sorting considerations (are we sorting by qualified/unqualified status) and surprising presence of spaces. Without tooling, this spacing must be inserted manually.

We propose to move the ``unqualified`` modifier after the name. The part of the proposal is similar to this one: https://github.com/ghc-proposals/ghc-proposals/pull/190, but they only focus on changing the modifier position and not the default qualification strategy.


[Optional] Module name as type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
We also want to change the way types are imported. We are seeing a lot of::

  import Data.ByteString (ByteString)
  import qualified Data.ByteString as ByteString

This way developers get ``ByteString`` the module and ``ByteString`` the type in scope. Note how they live in two different namespaces.

We observed that a lot of modules have a "main" type which is usually named as the last component of the module:

* ``Data.HashMap``: ``HashMap``
* ``Data.Sequence``: ``Sequence``
* ...

We also propose to automatically import the "main type" with the same name as the module.
  

Proposed Change Specification
-----------------------------

A new language ``QualifiedImports`` is introduced. When enabled it:

* remove the ``qualified`` modifier from ``import`` syntax
* changes the default behavior of ``import`` to qualified imports.
* introduce the ``unqualified`` modifier to the ``import`` syntax, after the module name, but before the optional ``as`` and binding import list.
* automatically import the "main type" with the same name as the qualified module.


Effect and Interactions
-----------------------

This proposal changes the default behavior of the import statement and slightly changes its syntax. Other than that, we don't see any other interactions with the language. The new syntax will however have an impact on all tools which parses haskell for import statement which will have to update their parser.


Costs and Drawbacks
-------------------

The implementation cost is a few lines of changes in the parser and in the import behavior. The usage of module name as a type when used in a type context may disallow future extension of the language with first class module.

This extension changes the semantic of haskell import statement, so it cannot be switched on without changes to all the import statements of a module, but this operation is straightforward and won't surprise module authors.

Alternatives
------------

1. Allowing the cohabitation of ``qualified`` and ``unqualified`` modifiers does not seem to bring any advantage, so we discarded this alternative.
2. We may not implement the "main type" import feature
3. The syntax for ``unqualified`` import can be different. For example, python uses ``from ModuleName import *`` for unqualified import, we may use something similar such as ``import Module as *`` or ``import module as unqualified``, but theses solutions conflicts with the ``as`` keyword.

Unresolved Questions
--------------------

1. The definition of the main type is complicated, we have a few options:

  * Using the type with the same name as the last component of the module name. For example ``Data.Container`` will use ``Data.Container.Container`` as main type. But it won't work for modules such as ``Data.HashMap.Strict``.
  * Using the type with the same name as the ``as`` clause. For example, ``import Data.HashMap.Strict as HashMap`` will use ``HashMap`` as the main type.
  * Introduce a new syntax in module export list to specify the main type. For example::
     module Data.HashMap.Strict (
        main HashMap,
        ...
     )

Implementation Plan
-------------------

I, @guibou, volunteer to do this job with a bit of mentoring from tweag collegues. I don't think that's difficult, we need to:

- change the grammar to introduce ``unqualified`` and remove ``qualified``. It is a minor change in the parser.
- change the import logic to import qualified by default. This is also a minor change.
- (Optional) import the "main type" as well. This may or may not be simple depending on the solution used to select the "main-type".
