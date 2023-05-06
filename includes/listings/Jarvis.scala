package utilities.geometry.convexhull.algorithms

import utilities.geometry.{Point, PointsUtils}
import utilities.geometry.PointsUtils.Points

case class Jarvis() extends ConvexHullAlgorithm {

  private val FIRST_POINT_ORDERING = new Ordering[Point] {
    override def compare(x: Point, y: Point): Int = {
      val byX = Ordering.Double.TotalOrdering.compare(x.x, y.x)
      lazy val byY = Ordering.Double.TotalOrdering.compare(x.y, y.y)
      if (byX == 0) byY
      else byX
    }
  }


  override protected def nonEmptyCalculate(points: Points): Points = {
    val firstPoint = points.max(FIRST_POINT_ORDERING)
    val pZero = Point(firstPoint.x - 1, firstPoint.y)

    var pI = firstPoint
    var pIMinusOne = pZero

    val pointsBuilder = List.newBuilder[Point]
    pointsBuilder += firstPoint

    while (true) {
      points
        .filterNot(_ == pI)
        .maxByOption(PointsUtils.angleBetweenThreePoints(pIMinusOne, pI, _)) match {
        case Some(`firstPoint`) => return pointsBuilder.result()
        case Some(pIPlusOne) =>
          pointsBuilder += pIPlusOne
          pIMinusOne = pI
          pI = pIPlusOne
        case _ => return pointsBuilder.result()
      }
    }
    Nil
  }

}
