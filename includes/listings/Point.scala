package utilities.geometry

case class Point(
                  x: Double,
                  y: Double
                ) {
  def -(that: Point): Point = Point(x - that.x, y - that.y)
  def +(that: Point): Point = Point(x + that.x, y + that.y)
  def /[T](number: T)(implicit numeric: Numeric[T]): Point = {
    val double = numeric.toDouble(number)
    Point(x / double, y / double)
  }
}
