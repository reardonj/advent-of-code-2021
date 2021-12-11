object Day10 extends Day:
  def a(stream: Iterator[String]): Any =
    stream.map(parseLine(_).fold(errorScore, _ => 0)).sum

  def b(stream: Iterator[String]): Any = {
    val results = stream
      .flatMap(parseLine(_).toOption)
      .map(_.map(pairs).foldLeft(0L)(5 * _ + completionScore(_)))
      .toSeq
      .sorted
    results(results.length / 2)
  }

  private def parseLine(line: String): Either[Char, List[Char]] =
    line.foldLeft(Right[Char, List[Char]](Nil))(parseToken)

  private val pairs = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  private val errorScore = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  private val completionScore = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  private def parseToken(parser: Either[Char, List[Char]], token: Char) =
    parser.flatMap { stack =>
      if pairs.contains(token) then Right(token :: stack)
      else if stack.headOption
          .flatMap(pairs.get)
          .fold(false)(_ == token)
      then Right(stack.tail)
      else Left(token)
    }
