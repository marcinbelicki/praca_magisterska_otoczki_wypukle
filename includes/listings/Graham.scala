package utilities.geometry.convexhull.algorithms
import utilities.geometry.PointsUtils.Points
import utilities.geometry.ordering.OrientationOrdering
import utilities.geometry.{PointsCycle, PointsUtils}


case object Graham extends ConvexHullAlgorithm {

  override def calculate(points: Points)(implicit ordering: OrientationOrdering): Points = {
    require(points.nonEmpty, "You can not calculate convex hull of an empty points set.")
    val centroid = PointsUtils.calculateCentroid(points)
    val pointsSorted = points.map(_ - centroid).sorted
    val pointsCycle = new PointsCycle(pointsSorted)

    pointsCycle.getHull.map(_ + centroid)
  }
}
