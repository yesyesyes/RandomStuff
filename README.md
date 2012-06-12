[sbt]: https://github.com/harrah/xsbt/wiki/Getting-Started-Setup
[link]: http://aperiodic.net/phil/scala/s-99/

# Just some random stuff

## Layout:

		src
         |-main/
             |.../Sorting.scala — some simple sorting algorithms
             |.../nnprblms — part of 99 scala problems solutions
         |-test/
             |.../SortingTests.scala


99 Scala problems [link].

## To test it locally:

1. Install the current stable binary release of [sbt]
2. Get the source code.

		$ git clone git://github.com/yesyesyes/RandomStuff.git

2. run 


		$ sbt test
