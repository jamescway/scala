set -xe
scalac gen.scala
scala org.scalatest.run GenSpec
