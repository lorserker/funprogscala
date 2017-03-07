package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (name, sig) => 
      (name, Signal(eval(cycleFreeExpr(name, sig(), namedExpressions), namedExpressions))) 
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  private def cycleFreeExpr(name: String, expr: Expr, refs: Map[String, Signal[Expr]]): Expr = {
    expr match {
      case Literal(v) => expr
      case Ref(refname) => {
        if (name == refname) Literal(Double.NaN) 
        else cycleFreeExpr(name, getReferenceExpr(refname, refs), refs)
      }
      case Plus(a, b) => Plus(cycleFreeExpr(name, a, refs), cycleFreeExpr(name, b, refs))
      case Minus(a, b) => Minus(cycleFreeExpr(name, a, refs), cycleFreeExpr(name, b, refs))
      case Times(a, b) => Times(cycleFreeExpr(name, a, refs), cycleFreeExpr(name, b, refs))
      case Divide(a, b) => Divide(cycleFreeExpr(name, a, refs), cycleFreeExpr(name, b, refs))
    }

  }
}
