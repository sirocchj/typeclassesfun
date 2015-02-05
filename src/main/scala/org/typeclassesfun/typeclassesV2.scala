package org.typeclassesfun

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

object typeclassesV2 {

  sealed trait Expression[A]

  type IntE = Expression[Int]
  type BoolE = Expression[Boolean]

  case class IntValue(value: Int) extends IntE

  case class BooleanValue(value: Boolean) extends BoolE

  case class And[E1 <: BoolE, E2 <: BoolE](expr1: E1, expr2: E2) extends BoolE

  case class Or[E1 <: BoolE, E2 <: BoolE](expr1: E1, expr2: E2) extends BoolE

  case class Not[E <: BoolE](expression: E) extends BoolE

  case class Add[E1 <: IntE, E2 <: IntE](expr1: E1, expr2: E2) extends IntE

  case class LessThan[E1 <: IntE, E2 <: IntE](expr1: E1, expr2: E2) extends BoolE

  case class GreaterThan[E1 <: IntE, E2 <: IntE](expr1: E1, expr2: E2) extends BoolE

  case class If[P <: BoolE, B1 <: IntE, B2 <: IntE](pred: P, branch1: B1, branch2: B2) extends IntE

  sealed trait JsValue

  case class JsString(s: String) extends JsValue

  case class JsNumber(bd: BigDecimal) extends JsValue

  case class JsBoolean(b: Boolean) extends JsValue

  case class JsObject(m: Map[String, JsValue]) extends JsValue

  @implicitNotFound("Please define an implicit JsWrite[${A}]")
  trait JsWrite[A] {
    def write(a: A): JsValue
  }

  object JsWrite {
    def apply[T: JsWrite]: JsWrite[T] = implicitly[JsWrite[T]]
  }

  object Json {
    def stringify(jsValue: JsValue): String = jsValue match {
      case JsString(s) ⇒ s""""$s""""
      case JsNumber(bd) ⇒ bd.toString()
      case JsBoolean(b) ⇒ b.toString
      case JsObject(m) ⇒ (for ((key, value) ← m) yield s"$key: ${stringify(value)}").mkString("{", ", ", "}")
    }

    def toJson[A: JsWrite](a: A): JsValue = JsWrite[A] write a
  }

  object JsonWriters {
    implicit val jsWriteIntValue = new JsWrite[IntValue] {
      override def write(intValue: IntValue): JsValue = JsNumber(intValue.value)
    }

    implicit val jsWriteBooleanValue = new JsWrite[BooleanValue] {
      override def write(booleanValue: BooleanValue): JsValue = JsBoolean(booleanValue.value)
    }

    implicit def jsWriteAnd[E1 <: BoolE : JsWrite, E2 <: BoolE : JsWrite]: JsWrite[E1 And E2] = new JsWrite[E1 And E2] {
      override def write(and: E1 And E2): JsValue = JsObject {
        Map(
          "op" → JsString("&"),
          "expr1" → (JsWrite[E1] write and.expr1),
          "expr2" → (JsWrite[E2] write and.expr2)
        )
      }
    }

    implicit def jsWriteOr[E1 <: BoolE : JsWrite, E2 <: BoolE : JsWrite]: JsWrite[E1 Or E2] = new JsWrite[E1 Or E2] {
      override def write(or: E1 Or E2): JsValue = JsObject {
        Map(
          "op" → JsString("|"),
          "expr1" → (JsWrite[E1] write or.expr1),
          "expr2" → (JsWrite[E2] write or.expr2)
        )
      }
    }

    implicit def jsWriteNot[E <: BoolE : JsWrite]: JsWrite[Not[E]] = new JsWrite[Not[E]] {
      override def write(not: Not[E]): JsValue = JsObject {
        Map(
          "op" → JsString("¬"),
          "expression" → (JsWrite[E] write not.expression)
        )
      }
    }

    implicit def jsWriteAdd[E1 <: IntE : JsWrite, E2 <: IntE : JsWrite]: JsWrite[E1 Add E2] = new JsWrite[E1 Add E2] {
      override def write(add: E1 Add E2): JsValue = JsObject {
        Map(
          "op" → JsString("+"),
          "expr1" → (JsWrite[E1] write add.expr1),
          "expr2" → (JsWrite[E2] write add.expr2)
        )
      }
    }

    implicit def jsWriteLessThan[E1 <: IntE : JsWrite, E2 <: IntE : JsWrite]: JsWrite[E1 LessThan E2] = new JsWrite[E1 LessThan E2] {
      override def write(lessThan: E1 LessThan E2): JsValue = JsObject {
        Map(
          "op" → JsString("<"),
          "expr1" → (JsWrite[E1] write lessThan.expr1),
          "expr2" → (JsWrite[E2] write lessThan.expr2)
        )
      }
    }

    implicit def jsWriteGreaterThan[E1 <: IntE : JsWrite, E2 <: IntE : JsWrite]: JsWrite[E1 GreaterThan E2] = new JsWrite[E1 GreaterThan E2] {
      override def write(greaterThan: E1 GreaterThan E2): JsValue = JsObject {
        Map(
          "op" → JsString(">"),
          "expr1" → (JsWrite[E1] write greaterThan.expr1),
          "expr2" → (JsWrite[E2] write greaterThan.expr2)
        )
      }
    }

    implicit def jsWriteIf[P <: BoolE : JsWrite, B1 <: IntE : JsWrite, B2 <: IntE : JsWrite]: JsWrite[If[P, B1, B2]] = new JsWrite[If[P, B1, B2]] {
      override def write(`if`: If[P, B1, B2]): JsValue = JsObject {
        Map(
          "op" → JsString("if"),
          "pred" → (JsWrite[P] write `if`.pred),
          "branch1" → (JsWrite[B1] write `if`.branch1),
          "branch2" → (JsWrite[B2] write `if`.branch2)
        )
      }
    }
  }

  @implicitNotFound("Please define an implicit Eval[${A}, ${B}]")
  trait Eval[A, B] {
    def eval(a: A): B
  }

  object Eval {
    def evaluate[A, B](a: A)(implicit ev: Eval[A, B]): B = ev eval a
  }

  object Evaluators {
    implicit val evalIntValue = new Eval[IntValue, Int] {
      override def eval(intValue: IntValue): Int = intValue.value
    }
    implicit val evalBooleanValue = new Eval[BooleanValue, Boolean] {
      override def eval(booleanValue: BooleanValue): Boolean = booleanValue.value
    }

    implicit def evalAnd[E1 <: BoolE, E2 <: BoolE](implicit evE1: E1 Eval Boolean,
                                                   evE2: E2 Eval Boolean): E1 And E2 Eval Boolean = new Eval[E1 And E2, Boolean] {
      override def eval(and: E1 And E2): Boolean =
        (evE1 eval and.expr1) && (evE2 eval and.expr2)
    }

    implicit def evalOr[E1 <: BoolE, E2 <: BoolE](implicit evE1: E1 Eval Boolean,
                                                  evE2: E2 Eval Boolean): E1 Or E2 Eval Boolean = new Eval[E1 Or E2, Boolean] {
      override def eval(or: E1 Or E2): Boolean =
        (evE1 eval or.expr1) || (evE2 eval or.expr2)
    }

    implicit def evalNot[E <: BoolE](implicit evE: E Eval Boolean): Not[E] Eval Boolean = new Eval[Not[E], Boolean] {
      override def eval(not: Not[E]): Boolean = !(evE eval not.expression)
    }

    implicit def evalAdd[E1 <: IntE, E2 <: IntE](implicit evE1: E1 Eval Int,
                                                 evE2: E2 Eval Int): E1 Add E2 Eval Int = new Eval[E1 Add E2, Int] {
      override def eval(add: E1 Add E2): Int =
        (evE1 eval add.expr1) + (evE2 eval add.expr2)
    }

    implicit def evalLessThan[E1 <: IntE, E2 <: IntE](implicit evE1: E1 Eval Int,
                                                      evE2: E2 Eval Int): E1 LessThan E2 Eval Boolean = new Eval[E1 LessThan E2, Boolean] {
      override def eval(lessThan: E1 LessThan E2): Boolean =
        (evE1 eval lessThan.expr1) < (evE2 eval lessThan.expr2)
    }

    implicit def evalGreaterThan[E1 <: IntE, E2 <: IntE](implicit evE1: E1 Eval Int,
                                                         evE2: E2 Eval Int): E1 GreaterThan E2 Eval Boolean = new Eval[E1 GreaterThan E2, Boolean] {
      override def eval(greaterThan: E1 GreaterThan E2): Boolean =
        (evE1 eval greaterThan.expr1) > (evE2 eval greaterThan.expr2)
    }

    implicit def evalIf[P <: BoolE, B1 <: IntE, B2 <: IntE](implicit evP: P Eval Boolean,
                                                            evB1: B1 Eval Int,
                                                            evB2: B2 Eval Int): If[P, B1, B2] Eval Int = new Eval[If[P, B1, B2], Int] {
      override def eval(`if`: If[P, B1, B2]): Int =
        if (evP eval `if`.pred)
          evB1 eval `if`.branch1
        else
          evB2 eval `if`.branch2
    }
  }

  def main(args: Array[String]): Unit = {
    val expression =
      If(
        And(
          LessThan(
            Add(
              Add(
                IntValue(2),
                IntValue(3)
              ),
              IntValue(4)
            ),
            IntValue(23)
          ),
          GreaterThan(
            IntValue(45),
            IntValue(12)
          )
        ),
        Add(
          IntValue(2),
          IntValue(2)
        ),
        IntValue(5)
      )

    // pseudo-code:
    // val e = if (((2 + 3) + 4 < 23) && (45 > 12)) {
    //   2 + 2
    // } else {
    //   5
    // }

    import org.typeclassesfun.typeclassesV2.Json._
    import org.typeclassesfun.typeclassesV2.JsonWriters._

    val json = toJson(expression)
    val jsonString = stringify(json)

    import org.typeclassesfun.typeclassesV2.Eval._
    import org.typeclassesfun.typeclassesV2.Evaluators._

    val result = evaluate(expression)

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
         |result                 : $result
         |inferred type of above : ${getTypeTag(result).tpe}
       """.stripMargin
    )
  }
}
