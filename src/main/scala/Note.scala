object Note {
  val notes = 
    Vector("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
}

case class Note(n:String) {
  import Note._
  require(notes.contains(n))

  val Intervals = Map(
    0 -> "Unity",
    1 -> "min2",
    2 -> "maj2",
    3 -> "min3",
    4 -> "maj3",
    5 -> "p4",
    6 -> "tritone",
    7 -> "p5",
    8 -> "min6",
    9 -> "maj6",
    10 -> "min7",
    11 -> "maj7",
    12 -> "octave"
  )

  override def toString() = this.n
  
  // Distance in halfsteps
  def dist(that:Note):Int = {
    val d = notes.indexOf(that.n) - notes.indexOf(this.n)
    d match {
      case s if s < -6 => s + 12
      case s if s > 6 => s - 12
      case _ => d
    }
  }

  private def sign(x:Int) = if (x < 0 && math.abs(x) != 6) "-" else ""

  def interval(that:Note) = {
    val d = this.dist(that)
    val name =  Intervals(math.abs(this.dist(that)))
    sign(d) + name
  }

  def shift(halfSteps:Int) = {
    require(halfSteps >= 0 && halfSteps <= 12)
    val newIdxRaw = notes.indexOf(this.n) + halfSteps
    val newIdx = if (newIdxRaw >= 12) newIdxRaw - 12 else newIdxRaw
    notes(newIdx)
  }
}
