/** Datatype for "a letter from A to G"
  */
enum NoteName derives CanEqual:
  case A, B, C, D, E, F, G

  lazy val nextNoteName: NoteName =
    this match
      case A => B
      case B => C
      case C => D
      case D => E
      case E => F
      case F => G
      case G => A

  def *(degree: Degree): NoteName =
    degree.fold(this)(_.nextNoteName)

  def /(degree: Degree): NoteName =
    this * degree.inverted

  def /(that: NoteName): Degree =
    if this == that
    then Degree.Unison
    else (this / that.nextNoteName).nextDegree

object NoteName:
  val allNoteNames: List[NoteName] = List(A, B, C, D, E, F, G)

/** A pitch class, i.e. a musical note with octave equivalence, so C4 == C5
  */
case class Note(noteName: NoteName, accidentalCount: Int) derives CanEqual:

  def sharpened = Note(noteName, accidentalCount + 1)

  def flattened = Note(noteName, accidentalCount - 1)

  def shiftedBy(accidentalDistance: Int): Note =
    Note(noteName, accidentalCount + accidentalDistance)

  def *(interval: Interval): Note =
    // perform the shift in C major, ignoring accidentalCount and interval.semitones
    //   and keep track of how many semitones we move
    val (resultingNoteName, totalSemitonesMoved) =
      interval.degree.fold(noteName, 0): (currentNoteName, semitonesMovedSoFar) =>
        (
          currentNoteName.nextNoteName,
          currentNoteName match
            case NoteName.E | NoteName.B => semitonesMovedSoFar + 1
            case _                       => semitonesMovedSoFar + 2
        )
    // Correct for the difference between the required (interval.semitones) and actual (totalSemitonesMoved)
    //   by changing the accidental of the resulting note
    Note(resultingNoteName, accidentalCount + (interval.semitones - totalSemitonesMoved))

  def /(interval: Interval): Note =
    this * interval.inverted

  def /(that: Note): Interval =
    val degree = noteName / that.noteName
    val offsetThis = that * Interval(degree, 0)
    Interval(degree, -offsetThis.accidentalCount)

object Note:
  val A = Note(NoteName.A, 0)
  val B = Note(NoteName.B, 0)
  val C = Note(NoteName.C, 0)
  val D = Note(NoteName.D, 0)
  val E = Note(NoteName.E, 0)
  val F = Note(NoteName.F, 0)
  val G = Note(NoteName.G, 0)
