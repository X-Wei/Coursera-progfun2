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
    for((name,exprSig)<-namedExpressions)
      yield (name -> Signal{eval(exprSig(), namedExpressions )})
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def eval_rec(expr:Expr, usedRef: Set[String]): Double = {// resolve the pb of cyclic definition by introducing `usedRef`
      expr match{
        case Ref(name) => {
          if(usedRef.contains(name)) Double.NaN
          else eval_rec(getReferenceExpr(name, references), usedRef + name)
        }
        case Literal(v) => v
        case Plus(a,b) => eval_rec(a, usedRef) + eval_rec(b, usedRef)
        case Minus(a,b) => eval_rec(a, usedRef) - eval_rec(b, usedRef)
        case Times(a,b) => eval_rec(a,usedRef) * eval_rec(b, usedRef)
        case Divide(a,b) => eval_rec(a, usedRef) / eval_rec(b, usedRef)
      }
    }

    eval_rec(expr, Set[String]())
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal => exprSignal()}
  }
}
