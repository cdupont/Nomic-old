Nomyx
=========

A Nomic game in Haskell

Nomyx is a fabulous and strange game where you have the right to change the rules in the middle of the game!
In fact, changing the rules is the goal of the game. Changing a rule is considered as a move. Of course even that could be changed!
In this game, the player can enter new rules in a dedicated language, modify existing ones, thus changing completely the behaviour of the game!

The web site: www.nomyx.net

Installation
============

You need a [Haskell platform](www.haskell.org/platform) running on your system. 
To install from the Hackage release, follow this procedure:

    cabal install Nomyx-Language --enable-documentation --haddock-hyperlink-source
    cabal install Nomyx

To install from the GitHub repo:

    git clone git@github.com:cdupont/Nomic.git
    cd Nomyx-Language
    cabal install  --enable-documentation --haddock-hyperlink-source
    cd ../Nomyx 
    cabal install


Execution
=========

Launch with the command:

    Nomyx

and follow the instructions. You may connect using a web browser to the provided address.

