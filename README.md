# Weekend project: bildpunkt

Bildpunkt is a simple GPU-based distance field renderer.
It is written in Haskell using the [accelerate](http://hackage.haskell.org/package/accelerate)
library.

The following sample images are rendered in around 4 seconds on a GeForce GTX 650 Ti Boost.

![sample image 1](https://raw.githubusercontent.com/apunktbau/bildpunkt/master/image/simple1.png)
![sample image 2](https://raw.githubusercontent.com/apunktbau/bildpunkt/master/image/simple2.png)

## Building

    $ git clone https://github.com/apunktbau/bildpunkt
    $ cd bildpunkt
    $ cabal configure && cabal build
    $ ./dist/build/bildpunkt/bildpunkt simple1 simple1.png

## Rendering

Currently there is no textual input, so scenes must be built by writing Haskell code.
Check [Bildpunkt.Scene](Bildpunkt/Scene.hs) for some simple pre-built scenes.
To render your scene, add a new match

    "yourSceneName" -> Scene.yourScene

to [Bildpunkt.Main.renderScene.config](Bildpunkt/Main.hs#L24) and run

    $ bildpunkt yourSceneName image.png

[Bildpunkt.DistanceField](Bildpunkt/DistanceField.hs) provides all distance fields you can use
to build a scene.

For more information on distance fields, see 
[Iñigo Quílez' website](http://www.iquilezles.org/www/articles/raymarchingdf/raymarchingdf.htm).
