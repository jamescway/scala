set -xe
scalac par.scala
scala org.scalatest.run ParSpec
