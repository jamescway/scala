set -xe
scalac Par.scala
scala org.scalatest.run ParSpec
