import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._

lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(const(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)

genMap.sample
genMap.sample
genMap.sample

val myGen = for { // generate tuples of 2 numbers
  n <- Gen.choose(10,20)
  m <- Gen.choose(2*n, 500)
} yield (n,m)

myGen.sample
myGen.sample

lazy val genList: Gen[List[Int]] = for {
  x <- arbitrary[Int]
  l <- oneOf(const(Nil), genList)
} yield x::l

genList.sample
genList.sample
genList.sample
genList.sample
genList.sample
