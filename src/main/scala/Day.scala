trait Day {
  def a(stream: Iterator[String]): Any
  def b(stream: Iterator[String]): Any

  val challenges: Seq[(String, Iterator[String] => Any)] =
    Seq("a" -> a, "b" -> b)
}
