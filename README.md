Dependent types Embedded in Programs (DEP)
==========================================

Dependently typed programming in Scala

*DEP* is a small collection of experiments illustrating how general
dependent types can be embedded in Scala's type system using macros.


Installation
------------

1. Scala runs on the JVM, so get yourself an up-to-date
   [JRE/JDK](http://openjdk.java.net/).  If you're on a Debian/Ubuntu
   system, this should do the trick:

        $ sudo apt-get install openjdk-7-jre

2. Install the [SBT](http://www.scala-sbt.org/) build tool.

3. Clone the [GitHub repository](https://github.com/heathermiller/dep):

        $ git clone https://github.com/heathermiller/dep.git

4. Go into the root directory and compile the project:

        $ cd dep
        $ sbt compile

5. Run the test suite

        $ sbt test

6. Have fun!
