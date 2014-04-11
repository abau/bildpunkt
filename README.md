# Weekend project: bildpunkt - a GPU-based distance field renderer

bildpunkt renders distance fields on the GPU.
It is written in Haskell using the [accelerate](http://hackage.haskell.org/package/accelerate)
library.

    $ git clone https://github.com/apunktbau/bildpunkt
    $ cd bildpunkt
    $ cabal configure && cabal build
    $ ./dist/build/bildpunkt/bildpunkt simple simple.png
