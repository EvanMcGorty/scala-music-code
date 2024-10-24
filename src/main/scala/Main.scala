@main def hello: Unit =
  println("Hello world!")
  println(msg)

  val x = Note.A / Note.C
  println(Interval.toNameString.lift(x).getOrElse(x.toString))
  println(Symbol.fromText("A#"))

def msg = "I was compiled by Scala 3. :)"
