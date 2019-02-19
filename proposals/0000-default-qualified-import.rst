Import qualified by default
========================

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

We propose a new extension to GHC `QualifiedImports` which switch the behavior of import from unqualified by default to qualified by default. We also propose to use the module name in a type context to reference the main type of the module.

In short ::

  import qualified Data.HashMap as HashMap
  import Data.Ord


  foo :: HashMap.HashMap -> HashMap.HashMap
  foo = ...

will become ::

  import Data.HashMap as HashMap
  import unqualified Data.Ord

  foo :: HashMap -> HashMap
  foo = ...


Motivation
------------

A lot of haskell modules are designed to be imported qualified, `vector`, `containers`, `...`, TODO: insert here an hackage survey on which add facts to this sentence!, however, by default the haskell language import unqualified.

By being the solution on less effort, developers are usually importing unqualified first and this leads to many issues in the development process:

* Without tooling, it is difficult to know the origin of a function
* The introduction of a new function in a module may lead to broken builds
* Developers usually switch to qualified import later and need to requalifiy all their function. Which is painful without tooling.

By doing this switch, we forces users to make the right choice by default (TODO: who am I to think that this is the right choice? ;)

-- TODO: survey of how it's done in many other languages:

- python: qualified by default
- c++: namespace are not flattened by default
- ....


We also want to change the way types are imported. We are seeing a lot of ::

  import Data.ByteString (ByteString)
  import qualified Data.ByteString as ByteString

This way developers get `ByteString` the module and `ByteString` the type in scope. Note how they live in two different namespaces.

We observed that a lot of modules have a "main" type which is usually named as the last component of the module:

* ``Data.HashMap``: ``HashMap``
* ``Data.Sequence``: ``Sequence``
* ...

We propose to use the fact that both namespaces are already separated to implicitly import the "main type" with the same name as the module.
  

Proposed Change Specification
-----------------------------

* change the syntax of ``import`` to introduce a new ``qualified`` modifier
* change the default (i.e. when there is no label) to ``qualified``.
* automatically import the "main type" with the same name as the qualified module.


Effect and Interactions
-----------------------

It certainly reduces the length of the import list.


TBD

Costs and Drawbacks
-------------------

TBD

Alternatives
------------

TBD

- keep the current solution
- only implement the `qualified` / `unqualified` switch, without "main type" export

Unresolved Questions
--------------------

The definition of the main type is complicated, we have a few options:

* Using the type with the same name as the last component of the module name. For example ``Data.Container`` will use ``Data.Container.Container`` as main type. But it won't work for modules such as `Data.HashMap.Strict`.
* Using the type with the same name as the `as` clause. For example, ``import Data.HashMap.Strict as HashMap`` will use ``HashMap`` as the main type.
* Introduce a new syntax in module export list to specify the main type. For example ::
    module Data.HashMap.Strict (
       main HashMap,
       ...
    )

Implementation Plan
-------------------

I, @guibou, volunteer to do this job with a bit of mentoring from tweag collegues. I don't think that's difficult, we need to:

- change the grammar to introduce `unqualified` 
- change the import logic to import qualified by default
- (Optional) import the "main type" as well.
