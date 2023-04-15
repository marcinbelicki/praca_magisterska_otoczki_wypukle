package utilities.geometry

import utilities.geometry.PointsUtils.Points

class PointsCycle(points: Points) extends Iterable[Point] {

  private lazy val first = NoPointRef.next

  override def iterator: Iterator[Point] = new Iterator[Point] {
    private var currentPointRef: AbstractPointRef = NoPointRef

    override def hasNext: Boolean = currentPointRef.isNotLast

    override def next(): Point = {
      val next = currentPointRef.next
      currentPointRef = next
      next.point
    }
  }

  private def createPointRefs(points: Points): AbstractPointRef =
    points.foldLeft[AbstractPointRef](NoPointRef)(_.addPoint(_)).addLastPointAndReturn()

  createPointRefs(points)

  trait AbstractPointRef {
    var next: PointRef = _
    var prev: PointRef = _

    def isNotFirst: Boolean
    def isNotLast: Boolean

    def addPoint(point: Point): PointRef

    protected def addLastPoint(): Unit

    final def addLastPointAndReturn(): AbstractPointRef = {
      addLastPoint()
      this
    }
  }

  class PointRef(val point: Point) extends AbstractPointRef {

    final def isNotFirst: Boolean = this ne first
    final def isNotLast: Boolean = next ne first

    override def addPoint(point: Point): PointRef = {
      val pointRef = new PointRef(point)
      pointRef.prev = this
      next = pointRef
      pointRef
    }

    def isInTriangle(t2: PointRef, t3: PointRef): Boolean =
      Orientation.calculateThreePointsOrientation(t2.point, t3.point, point) == Orientation.Left

    override protected def addLastPoint(): Unit = {
      next = first
      first.prev = this
    }
  }

  object NoPointRef extends AbstractPointRef {

    override def addPoint(point: Point): PointRef = {
      val pointRef = new PointRef(point)
      next = pointRef
      pointRef
    }

    override def isNotFirst: Boolean = first ne null
    override def isNotLast: Boolean = first ne null

    override protected def addLastPoint(): Unit = ()
  }

  def getHull: Points = {

    if (NoPointRef.isNotLast) {
      var q = NoPointRef.next
      while (q.isNotLast) {
        if (q.next.isInTriangle(q, q.next.next)) {
          q.next = q.next.next
          q.next.prev = q
          if (q.isNotFirst) q = q.prev
        } else q = q.next
      }
    }
    toList
  }

}
