package dna

/**
 * Created by daniel on 01/03/2015.
 */
object DNA {
  def minimalImpact(dna: String, p: Array[Int], q: Array[Int], m: Int): Array[Int] = {

    println(s"Args: $dna, ${p.toList}, ${q.toList}, $m")

    val result = new Array[Int](m)
    (0 until m) foreach {
      i => result(i) = Int.MaxValue
    }

    val lookup = Map('A' -> 1, 'C' -> 2, 'G' -> 3, 'T' -> 4)

    //go over every element
    (0 until dna.length) foreach {
      index =>
      //get the test case we want to chec
        (0 until m) foreach {
          testCase =>
          //if this index is within the test case
            val start = p(testCase)
            val end = q(testCase)

            val check = (index >= start && index <= end)

            //if we should check, get the char and compare and swap
            if (check) {
              val char: Char = dna.charAt(index)
              val value = lookup(char)

              println(s"Start=$start, End=$end, index=$index, char=$char value=$value testCase=$testCase")
              if (value < result(testCase))
                result(testCase) = value
            }
        }
    }

    result
  }
}
