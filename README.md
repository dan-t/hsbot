Robots with "superior" AI moving in a grid
------------------------------------------

![hsbot screenshot](hsbot.png)


Make and Install
----------------

    cabal install cabal-dev
 
    git clone https://github.com/dan-t/Gamgine
    git clone https://github.com/dan-t/hsbot
 
    cd hsbot
    cabal-dev add-source ../Gamgine
    cabal-dev install


Usage
-----

    hsbot


Control
-------

    left mouse button pressed  -> place a block
    right mouse button pressed -> remove a block
