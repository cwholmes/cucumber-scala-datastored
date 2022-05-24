package com.github.cwholmes.cucumber

import scala.reflect.ClassTag

final class TypedReturn private (private val returns: List[(AnyRef, ClassTag[AnyRef])])

object TypedReturn {

  def unapply(typedReturn: TypedReturn): Option[List[(AnyRef, ClassTag[AnyRef])]] = { Option(typedReturn.returns) }

  // only supporting up to 3 since one step shouldn't need to return more than 3 things
  def apply[R1 <: AnyRef](r1: R1)(implicit tag1: ClassTag[R1]): TypedReturn = { new TypedReturn(List((r1, tag1.asInstanceOf[ClassTag[AnyRef]]))) }

  def apply[R1 <: AnyRef, R2 <: AnyRef](r1: R1, r2: R2)(implicit tag1: ClassTag[R1], tag2: ClassTag[R2]): TypedReturn = {
    new TypedReturn(List((r1, tag1.asInstanceOf[ClassTag[AnyRef]]), (r2, tag2.asInstanceOf[ClassTag[AnyRef]])))
  }

  def apply[R1 <: AnyRef, R2 <: AnyRef, R3 <: AnyRef](r1: R1, r2: R2, r3: R3)(implicit
                                                                              tag1: ClassTag[R1],
                                                                              tag2: ClassTag[R2],
                                                                              tag3: ClassTag[R3]): TypedReturn = {
    new TypedReturn(
      List((r1, tag1.asInstanceOf[ClassTag[AnyRef]]), (r2, tag2.asInstanceOf[ClassTag[AnyRef]]), (r3, tag3.asInstanceOf[ClassTag[AnyRef]])))
  }

  def apply[R1 <: AnyRef, R2 <: AnyRef, R3 <: AnyRef, R4 <: AnyRef](r1: R1, r2: R2, r3: R3, r4: R4)(implicit
                                                                                                    tag1: ClassTag[R1],
                                                                                                    tag2: ClassTag[R2],
                                                                                                    tag3: ClassTag[R3],
                                                                                                    tag4: ClassTag[R4]): TypedReturn = {
    new TypedReturn(List(
      (r1, tag1.asInstanceOf[ClassTag[AnyRef]]),
      (r2, tag2.asInstanceOf[ClassTag[AnyRef]]),
      (r3, tag3.asInstanceOf[ClassTag[AnyRef]]),
      (r4, tag4.asInstanceOf[ClassTag[AnyRef]])
    ))
  }

}
