package fr.enst.plnc2013

abstract class List[+A] {
  def ::[B >: A](elem: B) = Cons(elem, this)

  def head: A
  def tail: List[A]
  def size: Int

  def buildRev[B >: A](newList: List[B]): List[B]

  def reverse: List[A]

  def map[B](f: A => B) : List[B]

  def foreach(f: A => Unit): Unit
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def size = 1 + tail.size

  def buildRev[B >: A](newList: List[B]) : List[B] = tail.buildRev(head :: newList)

  def reverse = buildRev(Nil)

  def map[B](f: A => B) = f(head) :: tail.map(f)

  def foreach(f: A => Unit) {
    f(head)
    tail.foreach(f)
  }
}

case object Nil extends List[Nothing] {
  def head = sys.error("no head for Nil")
  def tail = sys.error("no tail for Nil")
  def size = 0

  def buildRev[A](newList: List[A]) = newList

  val reverse = this

  def map[B](f: Nothing => B) = Nil

  def foreach(f: Nothing => Unit) {}
}


