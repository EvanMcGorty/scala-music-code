case class Harmony(intervals: Set[Interval]) derives CanEqual:

  def *(otherInterval: Interval): Harmony = Harmony(intervals.map(_ * otherInterval))

  def /(otherInterval: Interval): Harmony = Harmony(intervals.map(_ / otherInterval))

  def inverted: Harmony = Harmony(intervals.map(_.inverted))

  def +(otherInterval: Interval): Harmony = Harmony(intervals + otherInterval)

  def ++(that: Harmony): Harmony = Harmony(intervals ++ that.intervals)

  def nextInversion: Harmony =
    intervals.toSeq.sorted
      .find(_.degree != Degree.Unison)
      .map: nextInterval =>
        Harmony(intervals.map(_ / nextInterval))
      .getOrElse(this)

object Harmony:

  import Interval.*

  val Empty = Harmony(Set.empty)

  val Unison = Empty + Interval.PerfectUnison

  val Five = Unison + PerfectFifth
  val DiminishedFive = Unison + DiminishedFifth
  val AugmentedFive = Unison + AugmentedFifth

  val Major = Five + MajorThird
  val Minor = Five + MinorThird
  val Diminished = DiminishedFive + MinorThird
  val Augmented = AugmentedFive + MajorThird

  val MajorSeven = Major + MajorSeventh
  val Seven = Major + MinorSeventh
  val MinorSeven = Minor + MinorSeventh
  val MinorMajorSeven = Minor + MajorSeventh
  val HalfDiminishedSeven = Diminished + MinorSeventh
  val FullyDiminishedSeven = Diminished + DiminishedSeventh
  val AugmentedMajorSeven = Augmented + MajorSeventh

  // Badly named chord qualities
  object Enharmonic:
    val AugmentedSeven = Harmony(Set(Interval.PerfectUnison, MajorThird, MinorSixth, MinorSeventh))
    val DiminishedMajorSeven = Harmony(Set(Interval.PerfectUnison, AugmentedSecond, AugmentedFourth, MajorSeventh))

  val SuspendedSecond = Five + MajorSecond
  val SuspendedFour = Five + PerfectFourth

  val MajorSix = Major + MajorSixth
  val MinorSix = Major + MinorSixth
  val MinorFlatSix = Minor + MinorSixth

  val Ionian = Harmony(
    Set(Interval.PerfectUnison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth, MajorSixth, MajorSeventh)
  )
  val Dorian = Ionian.nextInversion
  val Phyrgian = Ionian.nextInversion
  val Lydian = Ionian.nextInversion
  val Mixolydian = Ionian.nextInversion
  val Aeolian = Ionian.nextInversion
  val Locrian = Ionian.nextInversion
