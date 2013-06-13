package fr.enst.plnc2013

sealed abstract class Option[+A] {
  def isEmpty: Boolean
  def get: A
  def getOrElse[B >: A](default: B): B = 
    if (isEmpty)
      default
    else
      get
}

class Some[+A](val get: A) extends Option[A] {
  val isEmpty = false
}

case object None extends Option[Nothing] {
  val isEmpty = true
  def get = error("no get for None")
}
