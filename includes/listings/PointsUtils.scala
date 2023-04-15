package utilities.geometry

import java.lang.Math._

object PointsUtils {

  type Points = List[Point]

  private lazy val HALF_PI = PI * .5

  def calculateCentroid(points: Points): Point = points.reduceLeft(_ + _) / points.length

  def distanceSquared(a: Point, b: Point): Double = distanceFromCenterSquared(a - b)

  def distance(a: Point, b: Point): Double = distanceFromCenter(a - b)

  def distanceFromCenterSquared(p: Point): Double = pow(p.x, 2) + pow(p.y, 2)

  def distanceFromCenter(p: Point): Double = sqrt(distanceFromCenterSquared(p))

  def phase(p: Point): Double = {
    import p.{x, y}
    (x.sign, y.sign) match {
      case (1, _) => atan(y / x)
      case (-1, 0 | 1) => atan(y / x) + PI
      case (-1, -1) => atan(y / x) - PI
      case (0, -1) => HALF_PI
      case (0, 1) => -HALF_PI
    }
  }

  def alpha(p: Point): Double = {
    import p.{x, y}
    val absX = abs(x)
    val absY = abs(y)
    val d = absX + absY

    (x.sign, y.sign) match {
      case (0 | 1, 0 | 1) => y / d
      case (-1, 0 | 1) => 2 - y / d
      case (-1, -1) => 2 + absY / d
      case (0 | 1, -1) => 4 - absY / d
    }
  }
}
