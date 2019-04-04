Qualified import by default
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

This proposal adds a new extension to GHC ``-XQualifiedImports`` which switches the default behavior of ``import`` from importing modules unqualified to importing module qualified. This proposal also introduces a notion of *main type* of a module import: the main type of a module import is automatically imported unqualified.

That is::

  import qualified Data.HashMap as HashMap
  import           Data.Ord
  import Data.HashMap (HashMap)


  foo :: HashMap.HashMap -> HashMap.HashMap
  foo = ...

becomes::

  {-# LANGUAGE QualifiedImports #-}

  import Data.HashMap as HashMap
  import Data.Ord unqualified

  foo :: HashMap -> HashMap
  foo = ...


Motivation
------------

Qualified as default
~~~~~~~~~~~~~~~~~~~~

A lot of Haskell modules are designed to be imported qualified. For instance ``vector`` and ``containers`` have conflicting bindings.

However, the default of the Haskell language is to import module unqualified. In consequence, developers tend to import module unqualified first, and design their internal modules for unqualified imports as well. This implies that:

* Without tooling, it is difficult to know the origin of a function
* Symbol conflict can appear when a new module is imported unqualified or when a module introduces a new symbol. In context of the Haskell package versioning policy (PVP), a new binding is not considered a breaking change, but it can lead to symbol conflict if it appears in a module which is imported unqualified.
* Developers who want to switch to qualified import later in their project, then need to qualify all their name. Which is painful without tooling and generate important diffs.

To address these issues this proposal makes module imports qualified by default::

  import Module

will have the same meaning as the current syntax::

  import qualified Module

Main type
~~~~~~~~~

A practical limitation on qualified imports is that you can end up with referring to ``ByteString.ByteString``, which needlessly occupies screen space. As a consequence, the following idiom has been adopted by many::

  import Data.ByteString (ByteString)
  import qualified Data.ByteString as ByteString

Now one can both refer to ``ByteString`` (the type), while still requiring the name of definitions from the ``ByteString`` module to be qualified (*e.g.* ``ByteString.empty``).

This proposal codifies this idiom by implicitly importing the main type of a module unqualified (see `Proposed Change Specification`_ for the definition of the main type). For example

  import Data.ByteString as ByteString -- This is a qualified import due to this proposal change

will also import ``Data.ByteString.Bytestring`` unqualified as ``ByteString``.


Proposed Change Specification
-----------------------------

A new language extension, ``-XQualifiedImports`` is introduced.

When ``-XQualifiedImports`` is enabled

* The qualified-import syntax entry

  ::

    import qualified modid [as modid] [impspec]

  becomes a syntax error. In other words, one mustn't use the ``qualified`` keyword.
* In


  ::

    import modid1 [as modid2] [impsec]

  The *main type* is defined as the type, if it exists, exported by ``modid1`` whose name coincide with ``modid2``.

  For instance, in ``import Data.Container.Map as Map``, ``Map`` is the main type. But, in ``import Data.Container.Map as M`` there is no main type.

  If ``as modid2`` is omitted, then there is no main type.
* The meaning of

  ::

    import modid [as modid] [impspec]

  is changed to importing the importing the module ``modid`` qualified. In addition the main type, if it exists and is imported, is also imported unqualified. For instance, if ``impsec`` is specified and doesn't mention the main type, then the main type is not imported.

  Note that only the main type is imported unqualified, not his constructors or fields.
* A new syntax entry is added

  ::

    import modid unqualified [impspec]

  It imports ``modid`` unqualified (restricted, as usual, to the ``impsec`` if it is specified)


Effect and Interactions
-----------------------

By making qualified import a convenient default, this proposal lets software designers make their code base explicitly designed for qualified import first. Helping nudge new developments towards qualified import by default, and push for a simplification of module import bureaucracy.

The changes are contained to the import declarations, and there is no known

Costs and Drawbacks
-------------------

The parser changes only affect module imports. Which are a fairly simple and self-contained part of the parser. Therefore the parser changes are expected to be easy and non-intrusive.

The implementation cost of retrieving the main type of a module import is not yet known, but should not have a significant effect on code complexity.

Alternatives
------------

No implicit unqualified import
++++++++++++++++++++++++++++++

We may choose not to implement the implicit unqualified import of the main type. If we did so, we would expect to see a lot of

::

  import Data.Map unqualified (Map)
  import Data.Map as Map

Since it is already a common idiom. This is not a lot of boilerplate to cope with, and this would take away the only non-trivial feature to implement from the proposal.

On the other hand, it does feel awkward to repeat this idiom all over. Therefore, the implicit unqualified import of main types is likely to be a big driver for adoption of the qualified-by-default style.

Alternative definition of the main type
+++++++++++++++++++++++++++++++++++++++

The main type could be defined differently.

- The main type of a module import is the type, if it exists, whose name is the same as the last component of the module's name. For example, ``import Data.Container as Storage`` would import the ``Data.Container.Container`` type unqualified.
- A heavier-weight approach would be to let module specify their main type with a syntax such as

  ::

    module Data.HashMap.Strict (
      main HashMap,
      ...
    )


The reason why we chose to bind the main type to the named with which the import is qualified are

- It works with existing libraries.
- ``Foo.Foo`` looks very repetitive, ``FooBar.Foo`` feels much less awkward. So really, the former is the one to be avoided.
- It makes visual sense that ``Foo.frobnicate`` is the ``frobnicate`` function which applies to type ``Foo``.
- It is not hard to find examples of modules where the indented main type does not share a name with the module. For instance, in the ``dependent-map`` package, the intended main type of the module ``Data.Dependent.Map`` is ``DMap``. Using the main type convention of this proposal, one would expect the programmers to write

  ::

    import Data.Dependent.Map as DMap

   hence import ``DMap`` unqualified, and the ``member`` function as ``DMap.member``.

Syntax of unqualified imports
+++++++++++++++++++++++++++++

Here are alternative syntax proposals for explicit unqualified imports

- More symmetric with the Haskell 98 syntax, ``unqualified`` could be specified in before the module name: ``import unqualified ModuleName``. However, considering the positive responses to https://github.com/ghc-proposals/ghc-proposals/pull/190 , it really does not seem like a good option.
- Yet another option is to consider, conceptually, and represent visually that unqualified imports are qualified imports in a zero-length namespace. Example syntax could be:
    - ``import ModuleName as unqualified``
    - ``import ModuleName as *``
    - ``import ModuleName as .``

    Each time, the right-hand side of the ``as`` is a keyword, which signifies unqualified import.


Unresolved Questions
--------------------

1. User may want to disable implicitly unqualified type import. We thought about the following scheme:

- `import Module`: qualified import + unqualified import of the main type
- `import Module unqualified`: unqualified import
- `import Module qualified`: fully qualified import: no import of the main type

However the use case is rare and user always have the possibility to disable the implicit import of the main type by naming the imported module with a different name. For example ``import Data.ByteString as ByteString`` will implicitely import the ``ByteString`` type unqualified. However ``import Data.ByteString as LibByteString`` won't import ``ByteString`` unqualified.

Implementation Plan
-------------------

@guibou will implement this proposal with mentoring from Tweag I/O's GHC contributors.
