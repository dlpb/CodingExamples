package dna.dnatest

import org.scalatest._
import dna.DNA

class dnaTester extends FlatSpec with Matchers {

  it should "pass the example" in {
    val dna = "CAGCCTA"
    val p = List(2,5,0)
    val q = List(4,5,6)
    val m = 3

    val result = DNA.minimalImpact(dna, p.toArray, q.toArray, m)

    result.toList should be(List(2,4,1))
  }
}