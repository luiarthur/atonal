case class Atonal(scale: Vector[Note]) {
}

object Atonal {
  import scala.util.Random.shuffle

  def twelvePitches = Vector.range(0,12)
  def prettyString(s:String, n:Int) = if (s.size < n) (" " * (n-s.size)) + s else s

  def matApply[T,S](m:Vector[Vector[T]], f:T => S) = {
    m.map(row => row.map(f))
  }

  def printMatrix[T](m: Vector[Vector[T]], spaces:Int=1) {
    val maxSize = m.flatten.map(_.toString.size).max
    m.foreach(row => {
      row.foreach{x => print(prettyString(x.toString, maxSize) + " "*spaces)}
      println
    })
  }

  def bound(x: Int) = x match {
    case y if y > 11 => y - 12
    case y if y <  0 => y + 12
    case _ => x
  }

  def canonical(row: Vector[Int]): Vector[Int] = {
    require(row.max == 11 && row.min == 0 && row.distinct.length == row.length )
    if (row.head == 0) row else {
      val base0 = row.map(_ - row.head)
      base0.map(bound)
    }
  }

  def genMatrix(pitch: Vector[Int]):Vector[Vector[Int]] = {
    val prime = canonical(pitch) // row1
    val inversion = prime.map(prime.head - _).map(bound) // col1
    inversion.map( start => prime.map(_ + start).map(bound) )
  }

  def genRandomMatrix = genMatrix(shuffle(twelvePitches))

  def matFromLetterPrime(prime:Vector[String]) = {
    require(Note.notes.toSet == prime.toSet)
    val pitches = prime.map(Note.notes.indexOf)
    val numMat = genMatrix(pitches)
    matApply(numMat, Note.notes)
  }
}
