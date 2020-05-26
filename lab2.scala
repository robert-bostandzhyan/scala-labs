object SetOperation extends Enumeration {
  val Union, Intersection = Value
}

object Interval {
  val Empty: Interval = new Interval(0, 0)
}

class Interval(val a: Double, val b: Double) {
  def has(x: Double): Boolean = a < x && x < b
}

class IntervalSet private(
                           val op: SetOperation.Value,
                           val left: Option[IntervalSet],
                           val right: Option[IntervalSet],
                           val interval: Option[Interval]
                         ) {

  def this() = this(SetOperation.Union, None, None, Option(Interval.Empty))

  def this(interval: Interval) = this(SetOperation.Union, None, None, Option(interval))

  def has(x: Double): Boolean = if (isLeaf) interval.get.has(x) else
  if (op == SetOperation.Union) left.get.has(x) || right.get.has(x) else left.get.has(x) && right.get.has(x)


  def +(iSet: IntervalSet): IntervalSet
  = new IntervalSet(SetOperation.Union, Option(this), Option(iSet), None)

  def +(i: Interval) : IntervalSet = this + new IntervalSet(i)

  def *(iSet: IntervalSet): IntervalSet
  = new IntervalSet(SetOperation.Intersection, Option(this), Option(iSet), None)

  def *(i: Interval) : IntervalSet = this * new IntervalSet(i)

  private def isLeaf: Boolean = left.isEmpty && right.isEmpty && interval.isDefined
}


object Main extends App {
  val set1 = new IntervalSet +
    new Interval(0, 1) +
    new Interval(2, 5) +
    new Interval(7, 8)

  val set2 = (new IntervalSet +
    new Interval(0, 2) +
    new Interval(2.4, 5)) *
    new Interval(1.4, 8)

  val set = set1 * set2

  List(-1, 1.3, 2, 4, 7).foreach(x => println(x + " " + set.has(x)))
}
