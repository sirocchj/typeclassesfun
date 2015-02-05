package org.typeclassesfun

object notypeclassesV4 {

  sealed trait Expression

  case class Value(value: Int) extends Expression

  case class Add(expression1: Expression, expression2: Expression) extends Expression

  case class Subtract(expression1: Expression, expression2: Expression) extends Expression

  sealed trait JsValue

  case object JsNull extends JsValue

  case class JsString(s: String) extends JsValue

  case class JsNumber(i: BigDecimal) extends JsValue

  case class JsBoolean(b: Boolean) extends JsValue

  case class JsArray(a: Seq[JsValue]) extends JsValue

  case class JsObject(m: Map[String, JsValue]) extends JsValue

  object Json {
    def stringify(jsValue: JsValue): String = jsValue match {
      case JsNull ⇒ "null"
      case JsString(s) ⇒ s""""$s""""
      case JsNumber(i) ⇒ s"$i"
      case JsBoolean(b) ⇒ s"$b"
      case JsArray(a) ⇒ (a map stringify).mkString("[ ", ", ", " ]")
      case JsObject(m) ⇒ (for ((key, value) ← m) yield s"$key: ${stringify(value)}").mkString("{ ", ", ", " }")
    }

    def toJson(e: Expression): JsValue = e match {
      case Value(value) ⇒ JsNumber(value)
      case Add(e1, e2) ⇒ JsObject {
        Map(
          "operation" → JsString("+"),
          "expression1" → toJson(e1),
          "expression2" → toJson(e2)
        )
      }
      case Subtract(e1, e2) ⇒ JsObject {
        Map(
          "operation" → JsString("-"),
          "expression1" → toJson(e1),
          "expression2" → toJson(e2)
        )
      }
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

    import org.typeclassesfun.notypeclassesV4.Json._

    val json = toJson(expression)
    val jsonString = stringify(json)

    import org.typeclassesfun.notypeclassesV4.Evaluator._

    val evaluated = evaluate(expression)

    import org.typeclassesfun.ReflectionUtils._

    println(
      s"""expression             : $expression
          |inferred type of above : ${getTypeTag(expression).tpe}
          |
          |json                   : $json
          |inferred type of above : ${getTypeTag(json).tpe}
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
