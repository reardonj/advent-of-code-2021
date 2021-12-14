import scala.annotation.tailrec
object Day12 extends Day:

  def a(stream: Iterator[String]): Any = {
    val graph = parseGraph(stream)
    // println(graph)
    explore(
      graph,
      Seq(PathState("start" :: Nil, Set("start"), true)),
      Seq()
    ).length
  }

  def b(stream: Iterator[String]): Any = {
    val graph = parseGraph(stream)
    // println(graph)
    explore(
      graph,
      Seq(PathState("start" :: Nil, Set("start"), false)),
      Seq()
    ).length
  }

  private type Graph = Map[String, Set[String]]

  private class PathState(
      route: List[String],
      visited: Set[String],
      hasVisitedTwice: Boolean
  ):
    this.ensuring(route.nonEmpty)

    val done = route.head == "end"

    override def toString() =
      s"PathState(smallVisited = ${hasVisitedTwice}, route = ${route.reverse.mkString("-")})"

    def visitNeighbours(graph: Graph): Seq[PathState] = {
      val toVisit =
        if hasVisitedTwice then graph(route.head).diff(visited)
        else graph(route.head) - "start"

      // println(s"to visit from ${this}: ${toVisit}")
      toVisit.toSeq.map(visit)
    }

    private def visit(node: String): PathState =
      PathState(
        node :: route,
        if node.forall(_.isLower) then visited + node else visited,
        hasVisitedTwice || visited.contains(node)
      )

  @tailrec
  private def explore(
      graph: Graph,
      paths: Seq[PathState],
      foundRoutes: Seq[PathState]
  ): Seq[PathState] =
    if paths.isEmpty then foundRoutes
    else {
      val (newRoutes, rest) =
        paths.flatMap(_.visitNeighbours(graph)).partition(_.done)
      explore(graph, rest, foundRoutes ++ newRoutes)
    }

  private def parseGraph(stream: Iterator[String]): Graph =
    stream
      .map(_.split("-"))
      .foldLeft(Map[String, Set[String]]()) { case (graph, Array(a, b)) =>
        graph.updatedWith(a)(addEdge(b)).updatedWith(b)(addEdge(a))
      }

  private def addEdge(b: String)(
      value: Option[Set[String]]
  ): Option[Set[String]] = value.fold(Some(Set(b)))(edges => Some(edges + b))
