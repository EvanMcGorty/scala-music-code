/** A scale degree with octave equivalence i.e. 2nd == 9th
  */
enum Degree derives CanEqual:
  case Unison, Second, Third, Fourth, Fifth, Sixth, Seventh

  /** Classic 'applyNTimes' style function, where n is the number of scale degrees above the first
    */
  lazy val fold: [A] => A => (A => A) => A =
    this match
      case Unison   => [A] => (x: A) => (f: A => A) => x
      case Second  => [A] => (x: A) => (f: A => A) => f(x)
      case Third   => [A] => (x: A) => (f: A => A) => f(f(x))
      case Fourth  => [A] => (x: A) => (f: A => A) => f(f(f(x)))
      case Fifth   => [A] => (x: A) => (f: A => A) => f(f(f(f(x))))
      case Sixth   => [A] => (x: A) => (f: A => A) => f(f(f(f(f(x)))))
      case Seventh => [A] => (x: A) => (f: A => A) => f(f(f(f(f(f(x))))))

  lazy val nextDegree: Degree =
    this match
      case Unison   => Second
      case Second  => Third
      case Third   => Fourth
      case Fourth  => Fifth
      case Fifth   => Sixth
      case Sixth   => Seventh
      case Seventh => Unison

  def *(that: Degree): Degree =
    that.fold(this)(_.nextDegree)

  lazy val previousDegree: Degree =
    this * Seventh

  def /(that: Degree): Degree =
    that.fold(this)(_.previousDegree)

  lazy val inverted: Degree =
    Unison / this

  lazy val asIndex: Int =
    fold(0)(_ + 1)

object Degree:

  val allDegrees: List[Degree] = List(Unison, Second, Third, Fourth, Fifth, Sixth, Seventh)

  given Ordering[Degree] = summon[Ordering[Int]].on[Degree](_.asIndex)

/** An interval class, i.e. an interval quotiented by octave equivalence, so Major 2nd == Major 9th.
  * @param degree
  *   The scale degree which this interval shifts up by
  * @param semitones
  *   The total number of semitones encompassed by this interval
  *
  * Capable of representing nonsense intervals that are 'theoretically' valid. For example, a "doubly-augmented third"
  * would be Interval(Third, 6). Note that Interval(First, 12) is not an octave, but rather a 12-times-augmented unison.
  */
case class Interval(degree: Degree, semitones: Int) derives CanEqual:

  def *(that: Interval): Interval =
    val semitonesSum =
      if degree.asIndex + that.degree.asIndex >= 7
      then semitones + that.semitones - 12
      else semitones + that.semitones
    Interval(degree * that.degree, semitonesSum)

  def inverted: Interval =
    val invertedSemitones =
      if degree == Degree.Unison
      then -semitones
      else 12 - semitones
    Interval(degree.inverted, invertedSemitones)

  def /(that: Interval): Interval =
    this * that.inverted

object Interval:

  given Ordering[Interval] = summon[Ordering[Degree]].on[Interval](_.degree).orElseBy[Int](_.semitones)

  import Degree.*

  // All intervals that occur in the modes of Natural, Melodic, and Harmonic Minor/Minor
  val PerfectUnison = Interval(Unison, 0)
  val MinorSecond = Interval(Second, 1)
  val MajorSecond = Interval(Second, 2)
  val AugmentedSecond = Interval(Second, 3)
  val MinorThird = Interval(Third, 3)
  val MajorThird = Interval(Third, 4)
  val DiminishedFourth = Interval(Fourth, 4)
  val PerfectFourth = Interval(Fourth, 5)
  val AugmentedFourth = Interval(Fourth, 6)
  val DiminishedFifth = Interval(Fifth, 6)
  val PerfectFifth = Interval(Fifth, 7)
  val AugmentedFifth = Interval(Fifth, 8)
  val MinorSixth = Interval(Sixth, 8)
  val MajorSixth = Interval(Sixth, 9)
  val DiminishedSeventh = Interval(Seventh, 9)
  val MinorSeventh = Interval(Seventh, 10)
  val MajorSeventh = Interval(Seventh, 11)

  // Unnatural intervals that still sometimes (arguably) come up or that are just conceptually interesting
  // Some of these occur in double harmonic major/minor or in an 8 note scale
  object Strange:
    val DiminishedUnison = Interval(Unison, -1)
    val AugmentedUnison = Interval(Unison, 1)
    val DiminishedSecond = Interval(Second, 0)
    val DoublyAugmentedSecond = Interval(Second, 4)
    val DiminishedThird = Interval(Third, 2)
    val AugmentedThird = Interval(Third, 5)
    val DoublyDiminishedFourth = Interval(Fourth, 3)
    val DoublyAugmentedFourth = Interval(Fourth, 7)
    val DoublyDiminishedFifth = Interval(Fourth, 5)
    val DoublyAugmentedFifth = Interval(Fourth, 9)
    val DiminishedSixth = Interval(Sixth, 7)
    val AugmentedSixth = Interval(Sixth, 10)
    val DoublyDiminishedSeventh = Interval(Seventh, 8)
    val AugmentedSeventh = Interval(Seventh, 12)

  import Strange.*

  val toNameString: PartialFunction[Interval, String] =
    case PerfectUnison            => "unison"
    case MinorSecond       => "minor second"
    case MajorSecond       => "major second"
    case AugmentedSecond   => "augmented second"
    case MinorThird        => "minor third"
    case MajorThird        => "major third"
    case DiminishedFourth  => "diminished fourth"
    case PerfectFourth     => "perfect fourth"
    case AugmentedFourth   => "augmented fourth"
    case DiminishedFifth   => "diminished fifth"
    case AugmentedFifth    => "augmented fifth"
    case MinorSixth        => "minor sixth"
    case MajorSixth        => "major sixth"
    case DiminishedSeventh => "diminished seventh"
    case MinorSeventh      => "minor seventh"
    case MajorSeventh      => "major seventh"

    case DiminishedUnison        => "diminished unison"
    case AugmentedUnison         => "augmented unison"
    case DiminishedSecond        => "diminished second"
    case DoublyAugmentedSecond   => "doubly augmented second"
    case DiminishedThird         => "diminished third"
    case AugmentedThird          => "augmented third"
    case DoublyDiminishedFourth  => "doubly diminished fourth"
    case DoublyAugmentedFourth   => "doubly augmented fourth"
    case DoublyDiminishedFifth   => "doubly diminished fifth"
    case DoublyAugmentedFifth    => "doubly augmented fifth"
    case DiminishedSixth         => "diminished sixth"
    case AugmentedSixth          => "augmented sixth"
    case DoublyDiminishedSeventh => "doubly diminished seventh"
    case AugmentedSeventh        => "augmented seventh"
