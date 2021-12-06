object Day6 extends Day:
  def a(stream: Iterator[String]): Any = simulateDays(stream, 80)

  def b(stream: Iterator[String]): Any = simulateDays(stream, 256)

  private def simulateDays(stream: Iterator[String], days: Int) = {
    val fish = stream
      .flatMap(_.split(","))
      .toSeq
      .groupMapReduce(_.toInt)(_ => BigInt(1))(_ + _)

    (1 to days).foldLeft(fish)((f, day) => simulateOneDay(f)).values.sum
  }

  private def simulateOneDay(
      fishByDaysToReproduction: Map[Int, BigInt]
  ): Map[Int, BigInt] = {
    val shiftedDays = fishByDaysToReproduction.map { case (k, v) =>
      ((k - 1), v)
    }
    val toReproduce = shiftedDays.getOrElse(-1, BigInt(0))
    shiftedDays
      .removed(-1)
      .updated(8, toReproduce)
      .updatedWith(6)(fish => Some(fish.getOrElse(BigInt(0)) + toReproduce))
  }
