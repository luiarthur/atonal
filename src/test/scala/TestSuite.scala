import org.scalatest.FunSuite
class TestSuite extends FunSuite {
  test("Stupid") {
    assert(true)
  }

  test("Relations") {

    val c = Note("C")
    val a = Note("A")

    assert(Note("E").interval(Note("A")) == "p4")
    assert(Note("A").interval(Note("E")) == "-p4")
    assert(Note("A").interval(Note("C#")) == "maj3")

    assert(Note("A").interval(Note("D#")) == "tritone")
    assert(Note("D#").interval(Note("C")) == "-min3")
    assert(Note("C").interval(Note("D#")) == "min3")

    assert(Note("A").shift(4) == "C#")
    assert(Note("B").shift(6) == "F")
    assert(Note("C").shift(12) == "C")
    assert(Note("C").shift(9) == "A")
  }
}
