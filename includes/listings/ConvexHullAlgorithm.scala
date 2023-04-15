package utilities.geometry.convexhull.algorithms

import utilities.geometry.PointsUtils.Points
import utilities.geometry.ordering.OrientationOrdering

trait ConvexHullAlgorithm extends Product {
  def calculate(points: Points)(implicit ordering: OrientationOrdering): Points
}
