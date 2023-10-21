This is Mike Warot's bending of Oberon System 3 (also known as ETH Oberon) to fit with his odd tastes.

It's no longer compatible with the [Oberon+ compiler, IDE and runtimes](https://github.com/rochus-keller/Oberon/blob/master/README.md) and the [OBX Platform Abstraction Layer (PAL)](https://github.com/rochus-keller/Oberon/tree/master/runtime/Pal), and thus truly cross-platform (runs on all platforms where [LeanQt](https://github.com/rochus-keller/LeanQt) is available). 

The migration is just starting, and highly broken. Nothing works yet at all. I'm just taking stock of the pieces.

Anything with a .pas extension has at least recieved some attention. I've learned a few things

* If a declaration has a * after it, it's meant to be exported from a unit/module/etc.
* The automatic begin of a block to match the expected END is annoying when converting back to pascal
* There's a lot of code to go through
