AutoNomic
=========

A Nomic game in Haskell

The original game: www.nomic.net

Nomic is a fabulous and strange game where you have the right to change the rules in the middle of the game!

In fact, changing the rules is the goal of the game. Changing a rule is considered as a move. Of course even that could be changed!

In this game, the player can enter new rules in a dedicated language, modify existing ones, thus changing completely the behaviour of the game!


Installation
============

To install, follow this procedure:
- download the source tar.gz
- extract it somewhere

$ cd <name>/Nomic-Rules
$ cabal install
$ cd ../Nomic 
$ cabal install


Execution
=========

Launch with the command:

$ Nomic

and follow the instructions. You may connect using a web browser or a command line remote terminal like telnet.



Troubleshooting
===============

compiling from the sources, if you have this kind of problem:

$ cabal install
Resolving dependencies...
cabal: cannot configure digestive-functors-0.1.0.1. It requires mtl >=2.0.0.0
&& <3
For the dependency on mtl >=2.0.0.0 && <3 there are these packages:
mtl-2.0.0.0 and mtl-2.0.1.0. However none of them are available.
mtl-2.0.0.0 was excluded because mtl-1.1.0.2 was selected instead
mtl-2.0.0.0 was excluded because hslogger-1.0.12 requires mtl ==1.1.0.2
(...)

Then try to unregister all incriminated packages.
This could include:
$ghc-pkg unregister blaze-html
$ghc-pkg unregister happstack-state
$ghc-pkg unregister happstack-data
$ghc-pkg unregister happstack-util
$ghc-pkg unregister happstack-server
$ghc-pkg unregister web-routes
$ghc-pkg unregister web-routes-happstack
$ghc-pkg unregister digestive-functors
$ghc-pkg unregister digestive-functors-blaze
$ghc-pkg unregister digestive-functors-happstack
$ghc-pkg unregister monads-fd
$ghc-pkg unregister MaybeT
$ghc-pkg unregister Nomic-Rules
$ghc-pkg unregister mtl
$ghc-pkg unregister hslogger

Then re-run install command:
$ cabal install

