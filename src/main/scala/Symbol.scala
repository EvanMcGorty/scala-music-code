import cats.parse.*
import Degree.*

def log(str: String) =
  System.err.nn.println(str)

// Populated by Symbol.fromText exclusively for error logging
val originalStrings = scala.collection.mutable.Map.empty[Symbol, String]

// A music symbol which injects into String
// E.g. as a "Quality" Am7 == Cmaj6, but as Symbols these two are distinct
trait Symbol derives CanEqual:
  protected lazy val text: String
  final lazy val asText =
    val maybeReparsed = Symbol.fromText(text)
    if (!maybeReparsed.contains(this)) {
      log(s"WARN: $this.text == \"$text\" and parses to $maybeReparsed")
    }
    originalStrings.get(this).foreach { originalString =>
      if (originalString != text) {
        log(s"WARN: $this.text == \"$text\" but originally parsed string was \"$originalString\"")
      }
    }
    text

// A neutral element for addition and subtraction with symbols
case object ZeroSymbol extends Symbol:
  override protected lazy val text = "0"

val groupingDelimiters = Map(
  "(" -> ")",
  "[" -> "]",
  "{" -> "}",
)
// e.g. "(...)"
case class GroupingSymbol(opening: String, grouped: Symbol, closing: String) extends Symbol:
  override protected lazy val text = opening ++ grouped.asText ++ closing

// todo: allow strings as separators which can include arbitrary characters (including groupingDelimiters)
val listSeparators = List(
  " ",
  "\t",
  "\n",
  ",",
  ";",
  ":",
  "|",
  "->",
)
// e.g. "A B C" or "A, B, C" or "| A | B | C |"
// the first and last separators can be empty strings
case class ListSymbol(startSeparator: String, symbolsAndSeparators: List[(Symbol, String)]) extends Symbol:
  override protected lazy val text = startSeparator + symbolsAndSeparators.map {
    case (symbol, separator) => symbol.asText ++ separator
  }.mkString
  def simplified: Symbol = this match {
    case ListSymbol("", List((symbol, ""))) => symbol
    case _ => this
  }

val sumSeparators = Map(
  "+ " -> false,
  "- " -> true,
)
// A bunch of symbols added or subtracted sequentially
// Should have at least two elements
case class SumSymbol(symbolsWithNegativity: List[(Symbol, Boolean)]) extends Symbol:
  override protected lazy val text =
    symbolsWithNegativity.map { case (symbol, isNegative) =>
      (if isNegative then "- " else "+ ") ++ symbol.asText
    }.mkString(" ")
  def simplified: Symbol = this match {
    case SumSymbol(List((symbol, false))) => symbol
    case _ => this
  }

val noteNameToString = Map(
  NoteName.A -> "A",
  NoteName.B -> "B",
  NoteName.C -> "C",
  NoteName.D -> "D",
  NoteName.E -> "E",
  NoteName.F -> "F",
  NoteName.G -> "G",
)

case class NoteSymbol(note: Note) extends Symbol:
  override protected lazy val text =
    val accidental =
      if note.accidentalCount < 0 then
        "b" * (-note.accidentalCount)
      else
        "#" * note.accidentalCount
    noteNameToString(note.noteName) ++ accidental

val degreeToString = Map(
  Unison  -> 0,
  Second  -> 1,
  Third   -> 2,
  Fourth  -> 3,
  Fifth   -> 4,
  Sixth   -> 5,
  Seventh -> 6,
)

def perfectQualityToString(count: Int): String =
  if count == 0 then
    "Per"
  else if count < 0 then
    "Dim" + perfectQualityToString(count + 1)
  else
    "Aug" + perfectQualityToString(count - 1)

def imperfectQualityToString(count: Int): String =
  if count == 0 then
    "Maj"
  else if count == -1 then
    "Min"
  else if count < -1 then
    "Dim" + imperfectQualityToString(count + 1)
  else
    "Aug" + imperfectQualityToString(count - 1)

def isPerfectIntervalCount(positiveDegreeCount: Int) = ((positiveDegreeCount % 7): @unchecked) match {
  case 0 | 3 | 4 => true
  case 1 | 2 | 5 | 6 => false
}

// if qualityCount == 0, how many semitones does a degreeCount correspond to (mod 7)
val defaultSemitones = List(0, 2, 4, 5, 7, 9, 11)

case class IntervalSymbol(degreeCount: Int, qualityCount: Int) extends Symbol:
  override protected lazy val text =
    val (positiveDegreeCount, signStr) =
      if degreeCount < 0 then
        (-degreeCount, "-")
      else
        (degreeCount, "")
    val qualityStr =
      if isPerfectIntervalCount(positiveDegreeCount) then
        perfectQualityToString(qualityCount)
      else
        imperfectQualityToString(qualityCount)
    signStr ++ qualityStr ++ (positiveDegreeCount + 1).toString
  lazy val degreeCountMod7 = math.floorMod(degreeCount, 7)
  lazy val quotientedInterval = Interval(
    degree = Degree.allDegrees(degreeCountMod7),
    semitones = defaultSemitones(degreeCountMod7) + qualityCount
  )

object IntervalSymbol:
  def fromInterval(interval: Interval): IntervalSymbol =
    val degreeCount = Degree.allDegrees.indexOf(interval.degree)
    val degreeSemitones = defaultSemitones(degreeCount)
    IntervalSymbol(degreeCount, interval.semitones - degreeSemitones)

// I.e. 'maj' in 'Cmaj' or 'dor' in "Cdor'
trait NoteSuffix

trait ModeSymbol extends NoteSuffix with Symbol:
  override protected lazy val text = modeSymbolToString(this)
case object Locrian extends ModeSymbol
case object Phrygian extends ModeSymbol
case object Aeolian extends ModeSymbol
case object Dorian extends ModeSymbol
case object Mixolydian extends ModeSymbol
case object Ionian extends ModeSymbol
case object Lydian extends ModeSymbol

val modeSymbolToString = Map(
  Locrian -> "loc",
  Phrygian -> "phr",
  Aeolian -> "aeo",
  Dorian -> "dor",
  Mixolydian -> "mix",
  Ionian -> "ion",
  Lydian -> "lyd",
)

object Symbol:
  def parseZero = Parser.char('0').as(ZeroSymbol)
  def inverseParser[A](map: Map[A, String]): Parser[A] =
    val inverted = map.map { case (x, str) => (str, x) }
    assert(inverted.size == map.size)
    Parser.fromStringMap(inverted)
  val parseAccidentalCount =
    Parser.string("#").rep.map(_.length)
      .backtrack.orElse(Parser.string("b").rep.map(-_.length))
      .backtrack.orElse(Parser.pure(0))
  val parseNoteName = inverseParser(noteNameToString)
  val parseNote: Parser[Note] =
    (parseNoteName ~ parseAccidentalCount).map {
      case (noteName, accidentalCount) => Note(noteName, accidentalCount)
    }
  val parseNoteSymbol = parseNote.map(NoteSymbol(_))
  val parseNegativeSign: Parser0[Boolean] =
    Parser.fromStringMap0(Map("-" -> true, "" -> false))
  val parseDims: Parser[Int] =
    Parser.string("Dim").rep.map(_.length)
  val parseAugs: Parser[Int] =
    Parser.string("Aug").rep.map(_.length)
  val parsePerfectInterval: Parser[(Int, Int)] =
    Parser.string("Per").as(0)
      .backtrack.orElse(parseDims.map(-_))
      .backtrack.orElse(parseAugs) ~
      Numbers.digits.mapFilter(_.toIntOption).map(_-1)
        .filter(intervalCount => intervalCount >= 0 && isPerfectIntervalCount(intervalCount))
  val parseImperfectInterval: Parser[(Int, Int)] =
    Parser.fromStringMap(Map("Min" -> -1, "Maj" -> 0))
      .backtrack.orElse(parseDims.map(-1-_))
      .backtrack.orElse(parseAugs) ~
      Numbers.digits.mapFilter(_.toIntOption).map(_-1)
        .filter(intervalCount => intervalCount >= 0 && !isPerfectIntervalCount(intervalCount))
  val parseIntervalSymbol: Parser[IntervalSymbol] =
    parsePerfectInterval.backtrack.orElse(parseImperfectInterval).map {
      case (qualityCount, degreeCount) => IntervalSymbol(degreeCount, qualityCount)
    }
  val parseModeSymbol: Parser[ModeSymbol] =
    inverseParser(modeSymbolToString)
  val peekGroupingDelimiter =
    Parser.stringIn(groupingDelimiters.values)
      .backtrack.orElse(Parser.end)
  val peekAtomicDelimiter =
    Parser.stringIn(groupingDelimiters.values ++ listSeparators ++ sumSeparators.keys).peek
      .backtrack.orElse(Parser.end)
  val parseGroupingSymbol: Parser[GroupingSymbol] =
    Parser.fromStringMap(groupingDelimiters).withString.flatMap { (endOfGroup, startOfGroup) =>
      (parseSymbol <* Parser.string(endOfGroup)).map { parsedSymbol =>
        GroupingSymbol(startOfGroup, parsedSymbol, endOfGroup)
      }
    }
  def parseSumSeparator: Parser[Boolean] =
    Parser.fromStringMap(sumSeparators)
  def parseAtomicSymbol: Parser[Symbol] =
    Parser.oneOf(List(
      parseZero,
      parseGroupingSymbol,
      parseIntervalSymbol,
      parseModeSymbol,
      parseNoteSymbol
    ).map(parser => (parser <* peekAtomicDelimiter).backtrack))
  def parseSumSymbols: Parser[Symbol] =
    (parseSumSeparator.backtrack.orElse(Parser.pure(false)).with1 ~ parseAtomicSymbol ~
      (Parser.char(' ') *> parseSumSeparator ~ parseAtomicSymbol).backtrack.rep0).map {
        case (x, xs) => SumSymbol((x :: xs).map(_.swap)).simplified
      }
  def parseListSeparator: Parser0[String] =
    Parser.stringIn(listSeparators).backtrack.rep.map(_.toList.mkString)
      .backtrack.orElse(peekGroupingDelimiter.as(""))
  def parseSymbol: Parser0[Symbol] =
    (parseListSeparator.? ~ (parseSumSymbols ~ parseListSeparator).backtrack.rep0).map {
      case (maybeStartSeparator, sumSymbolsAndSeparators) =>
        ListSymbol(maybeStartSeparator.getOrElse(""), sumSymbolsAndSeparators).simplified
    }
  def fromText(text: String): Option[Symbol] =
    val maybeResult = parseSymbol.parseAll(text).toOption
    maybeResult.foreach { result =>
      originalStrings += (result -> text)
    }
    maybeResult
