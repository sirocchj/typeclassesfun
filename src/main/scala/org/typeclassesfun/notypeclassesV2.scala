package org.typeclassesfun

object notypeclassesV2 {

  trait Json { def json: String }
  trait Evaluator { def evaluate: Int }
  sealed trait Expression extends Json with Evaluator

  case class Value(value: Int) extends Expression {
    override def json: String = s"$value"
    override def evaluate: Int = value
  }

  case class Add(expr1: Expression, expr2: Expression) extends Expression {
    override def json: String =
      s"""{ op: "+", expr1: ${expr1.json}, expr2: ${expr2.json} }"""
    override def evaluate: Int = expr1.evaluate + expr2.evaluate
  }

  case class Subtract(expr1: Expression, expr2: Expression) extends Expression {
    override def json: String =
      s"""{ op: "-", expr1: ${expr1.json}, expr2: ${expr2.json} }"""
    override def evaluate: Int = expr1.evaluate - expr2.evaluate
  }

  def main(args: Array[String]): Unit = {
    val expression =
      Add(
        Subtract(
          Value(10),
          Value(2)
        ),
        Add(
          Value(1),
          Value(2)
        )
      )

    val jsonString = expression.json

    val evaluated = expression.evaluate

    import org.typeclassesfun.ReflectionUtils._

    println(
      s"""expression             : $expression
          |inferred type of above : ${getTypeTag(expression).tpe}
          |
          |jsonString             : $jsonString
          |inferred type of above : ${getTypeTag(jsonString).tpe}
          |
          |evaluated              : $evaluated
          |inferred type of above : ${getTypeTag(evaluated).tpe}
        """.stripMargin
    )
  }
}
