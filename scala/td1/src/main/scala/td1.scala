package fr.enst.plnc2013.td1

object TD1 {

  // Placer ici le code de isOdd & friends
  def isOdd(a: Int) = if(a % 2 == 1) true else false
  def isEven(a: Int) = !isOdd(a)

  def myWhile(cond: => Boolean,f: => Unit): Unit =
    if(cond){
      f
      myWhile(cond,f)
    }

  def solveQueens(numberOfQueens: Int, f: List[(Int, Int)] => Unit) : Unit =

}

class ExtCond(cond: => Boolean){
   def doWhile(f: => Unit): Unit =
     if(cond){
       f
       doWhile(f)
     }
}

object ExtCond {
  implicit def toExtCond(cond: => Boolean) = new ExtCond(cond)
}

class ExtSeq[T](seq: Seq[T]){
  def any(f: T => Boolean) = seq.map(f).contains(true)
  def all(f: T => Boolean) = seq.map(f).count({e => e == true}) == seq.length
}

object ExtSeq{
  implicit def toExtSeq[T](seq: Seq[T]): ExtSeq[T] = new ExtSeq(seq)
}

case class Complex(x: Double, y: Double){
  override def toString: String =
    if (y == 0)
      x.toString
    else if (x == 0)
      y.toString ++ "i"
    else if (x == 0 && y == 0)
      "0.0"
    else
      x.toString ++ { if(y>=0) "+" else "" } ++y.toString ++ "i"

  def reciprocal: Complex = new Complex(x,-y)
  def +(c: Complex): Complex = new Complex(x+c.x, y+c.y)
  def +(i: Int): Complex = new Complex(x+i.toDouble, y)
}

object Complex {
  implicit def toComplex(i: Int) = new Complex(i,0)
}

object Main extends App {

  import TD1._

  // Placer ici le code à exécuter
}
