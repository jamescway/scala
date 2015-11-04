set -xe
scalac State.scala
scala org.scalatest.run StateSpec
