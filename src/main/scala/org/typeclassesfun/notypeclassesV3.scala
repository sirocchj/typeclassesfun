package org.typeclassesfun

object notypeclassesV3 {

  sealed trait Expression

  case class Value(value: Int) extends Expression
  case class Add(expr1: Expression, expr2: Expression) extends Expression
  case class Subtract(expr1: Expression, expr2: Expression) extends Expression

  object Json {
    def json(e: Expression): String = e match {
      case Value(value) ⇒ s"$value"
      case Add(e1, e2) ⇒ s"""{ op: "+", expr1: ${json(e1)}, expr2: ${json(e2)} }"""
      case Subtract(e1, e2) ⇒ s"""{ op: "-", expr1: ${json(e1)}, expr2: ${json(e2)} }"""
    }
  }

  object Evaluator {
    def evaluate(e: Expression): Int = e match {
      case Value(value) ⇒ value
      case Add(e1, e2) ⇒ evaluate(e1) + evaluate(e2)
      case Subtract(e1, e2) ⇒ evaluate(e1) - evaluate(e2)
    }
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

    import org.typeclassesfun.notypeclassesV3.Json._

    val jsonString = json(expression)

    import org.typeclassesfun.notypeclassesV3.Evaluator._

    val evaluated = evaluate(expression)

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
