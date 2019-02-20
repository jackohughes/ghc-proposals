Haskell top level module
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

This is a revival of the proposal discussed in https://github.com/ghc-proposals/ghc-proposals/pull/92 which proposed a solution to the long-list-of-language-pragma problem.

In this document, we propose to introduce a new import statement which is able to only import top level informations from another module, i.e. `LANGUAGE`, `OPTIONS`, ... pragmas and `import` statement.

Contrary to the previous proposal by @nomeata, this proposal does not need any change in the GHC ecosysteme. Especially, no changes to cabal, stack, ghc database. All the behavior of top level modules is implemented in GHC.
  
Motivation
------------

(This section could be extended with @nomeata proposal which clearly states the problem)

But especially, the current situation here is that a project developer have two options to manage pragmas:

- listing them on all the files of the projet. This is a lot of boilerplate and is not composable.
- listing the extensions in a building tool, such as cabal, or stack. This leads to duplicated setup when projects must be built with different projects.

One example of use case for this is the following. Given a project which uses many haskell extensions, all of these extensions can be bundled in a "top level module" and imported easily on each module of the project.

The syntax should still be discussed, but for example, in the module `MyProject.Header` ::

  {-# LANGUAGE DerivingVia #-}
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE FunctionalDependencies #-}
  {-# LANGUAGE StandaloneDeriving #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE AllowAmbiguousTypes #-}
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE KindSignatures #-}
  {-# LANGUAGE TupleSections #-}
  {-# LANGUAGE MultiParamTypeClasses #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE UndecidableInstances #-}

  import qualified Text.Megaparsec as Megaparsec

Later, in another module, this will be used as ::

  import toplevel MyProject.Header

Proposed Change Specification
-----------------------------

* GHC grammar is extended to introduce ``import toplevel Module.Name`` syntax.
* GHC is extended so that a module keep its "top level" informations, so that they can be used by the ``import toplevel`` syntax.


Effect and Interactions
-----------------------

This proposal will allow a reduction of boilerplate present in all haskell module.

TODO: insert here a small hackage survey on which we compute the LANGUAGE overhead of known packages.

In a short future, we may imagine hackage package which only contains top level modules. We may also imagine that library authors will provide top level module in their library to help users bootstrap their usage of the library.


Costs and Drawbacks
-------------------

To be developed

Alternatives
------------

- cabal / stack / bazel / name-your-tool extensions field: it enables extensions for all packages at once, the source code is not independent.

Unresolved Questions
--------------------

- What happen when we import two top level modules with conflicting extensions? In his proposal, @nomeata proposed to enable / disable extension based on the order of import. This is not satisfying because haskell developers are used to the fact that module import can be reordered.

Implementation Plan
-------------------

To be discussed.
