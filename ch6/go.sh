set -xe
scalac state.scala
scala org.scalatest.run StateSpec
