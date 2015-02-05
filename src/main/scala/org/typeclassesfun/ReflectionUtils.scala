package org.typeclassesfun

import scala.reflect.runtime.{universe => ru}

object ReflectionUtils {
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
}
