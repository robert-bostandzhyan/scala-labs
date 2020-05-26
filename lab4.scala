//---------------------------------------------Equality-----------------------------------------------------------

object EqType extends Enumeration {
  val Const, Assign, Var = Value
}

object Equality {
  private def construct[T](a: EqPart[T], b: EqPart[T]): (EqPart[T], EqPart[T]) = if (a.isVar) (a, b) else (b, a)
}

class Equality[T] private(private val eq: (EqPart[T], EqPart[T])) {
  def left: EqPart[T] = eq._1
  def right: EqPart[T] = eq._2

  def this(a: EqPart[T], b: EqPart[T]) = this(Equality.construct[T](a, b))

  def this(a: String, b: String) = this(new EqPart[T](a), new EqPart[T](b))
  def this(a: String, b: T)      = this(new EqPart[T](a), new EqPart[T](b))
  def this(a: T, b: String)      = this(new EqPart[T](a), new EqPart[T](b))
  def this(a: T, b: T)           = this(new EqPart[T](a), new EqPart[T](b))

  def eqType: EqType.Value = {
    if (left.isValue && right.isValue) {
      EqType.Const
    } else if (left.isVar && right.isValue) {
      EqType.Assign
    } else {
      EqType.Var
    }
  }
}

class EqPart[T] private(val name: Option[String], val value: Option[T]) {

  def this(name: String) = this(Option(name), None)

  def this(value: T) = this(None, Option(value))

  def isVar: Boolean = name.isDefined

  def isValue: Boolean = !isVar
}

//---------------------------------------------EqSystem-----------------------------------------------------------

object EqSystem {
  private def construct[T](eqList: List[Equality[T]]): (List[Equality[T]], Map[String, T], List[Equality[T]]) = {
    val groups = eqList.groupBy(_.eqType)
    (groups.getOrElse(EqType.Const, List.empty[Equality[T]]),
      groups.getOrElse(EqType.Assign, List.empty[Equality[T]])
        .map(eq => (eq.left.name.get, eq.right.value.get))
        .toMap,
      groups.getOrElse(EqType.Var, List.empty[Equality[T]]))
  }

  implicit class EqSystemOps[T:Numeric](s: EqSystem[T]) {
    def solve(name: String): Option[T] = {
      val res = s.assignEqs.get(name)
      if (res.isDefined) {
        res
      } else if (s.constEqs.isEmpty && s.assignEqs.isEmpty) {
        None
      } else {
        s.reduceSystem().solve(name)
      }
    }
  }
}

class EqSystem[T] private(val system: (List[Equality[T]], Map[String, T], List[Equality[T]])) {
  def constEqs: List[Equality[T]] = system._1
  def assignEqs: Map[String, T] = system._2
  def varEqs: List[Equality[T]] = system._3

  def this() = this(List(), Map(), List())

  def this(eqList: List[Equality[T]]) = this(EqSystem.construct(eqList))

  def +(eq: Equality[T]): EqSystem[T] = {
    if (eq.eqType == EqType.Const) {
      new EqSystem((eq :: constEqs, assignEqs, varEqs))
    } else if (eq.eqType == EqType.Assign) {
      new EqSystem((constEqs, assignEqs + (eq.left.name.get -> eq.right.value.get), varEqs))
    } else {
      new EqSystem((constEqs, assignEqs, eq :: varEqs))
    }
  }

  def isSolvable: Boolean = {
    if (constEqs.exists(eq => eq.left.value.get != eq.right.value.get)) {
      false
    } else if (constEqs.isEmpty && assignEqs.isEmpty) {
      true
    } else {
      reduceSystem().isSolvable
    }
  }

  private def reduceSystem(): EqSystem[T] = new EqSystem(
    varEqs.map(eq => {
      val l = assignEqs.get(eq.left.name.get)
      val r = assignEqs.get(eq.right.name.get)
      if (l.isDefined && r.isDefined) {
        new Equality[T](l.get, r.get)
      } else if (l.isDefined && r.isEmpty) {
        new Equality[T](l.get, eq.right.name.get)
      } else if (l.isEmpty && r.isDefined) {
        new Equality[T](eq.left.name.get, r.get)
      } else {
        new Equality[T](eq.left.name.get, eq.right.name.get)
      }
    })
  )
}

//----------------------------------------------------------------------------------------------------------------

object Main extends App {
  val system = new EqSystem[Int](List(
    new Equality(1, 1),
    new Equality(2, 2),
    new Equality("a", 2),
    new Equality("a", "b"),
    new Equality("b", "c"),
    new Equality("c", "e"),
    new Equality("d", "f"),
    new Equality("d", 1),
    new Equality("i", "j")
  ))

  println(system.isSolvable)
  println("a=" + system.solve("a"))
  println("c=" + system.solve("c"))
  println("f=" + system.solve("i"))
}
