package utilities.geometry.convex.hull.algorithms

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import utilities.geometry.Point
import utilities.geometry.PointsUtils.Points
import AlgorithmTest.{EXPECTED_CONVEX_HULL, INPUT_POINTS, ONE_INPUT_POINT}
import utilities.geometry.convexhull.algorithms.ConvexHullAlgorithm
import utilities.geometry.ordering.OrientationOrdering

abstract class AlgorithmTest(convexHullAlgorithm: ConvexHullAlgorithm)(implicit ordering: OrientationOrdering) extends AnyWordSpec {
  private def convexHullAlgorithmName: String = convexHullAlgorithm.productPrefix

  private def assertExpectedOutput(input: Points, expectedOutput: Points): Assertion = {
    val actualConvexHull = convexHullAlgorithm.calculate(input)
    assert(actualConvexHull == expectedOutput)
  }

  s"$convexHullAlgorithmName" must {
    "throw the IllegalArgumentException for empty" in {
      assertThrows[IllegalArgumentException](convexHullAlgorithm.calculate(Nil))
    }

    "return the same thing for one input point" in {
      assertExpectedOutput(ONE_INPUT_POINT, ONE_INPUT_POINT)
    }

    "properly calculate convex hull" in {
      assertExpectedOutput(INPUT_POINTS, EXPECTED_CONVEX_HULL)
    }
  }
}


object AlgorithmTest {
  val INPUT_POINTS: Points = List(
    Point(1, 3),
    Point(2, 4),
    Point(2, 2),
    Point(4, 6),
    Point(3, 4),
    Point(4, 4),
    Point(4, 3),
    Point(2, 6),
    Point(5, 2),
    Point(6, 4),
    Point(5, 5)
  )

  val EXPECTED_CONVEX_HULL: Points = List(
    Point(6, 4),
    Point(4, 6),
    Point(2, 6),
    Point(1, 3),
    Point(2, 2),
    Point(5, 2)
  )

  val ONE_INPUT_POINT: Points = List(
    Point(1, 2)
  )
}

