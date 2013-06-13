package fr.enst.plnc2013

/**
 * Created with IntelliJ IDEA.
 * User: jeremiefourbil
 * Date: 13/06/13
 * Time: 16:47
 * To change this template use File | Settings | File Templates.
 */
case class Rational (n: Int, d: Int = 1){
  import Rational._

  require(d != 0)

  private[this] val g = gcd(n, d)

  val numerator = n / g
  val denominator = d / g

  override lazy val toString = if (denominator == 1) numerator.toString
  else numerator + "/" + denominator

  def +(other: Rational) = Rational(numerator * other.denominator +
    other.numerator * denominator,
    denominator*other.denominator)
  def *(other: Rational) = Rational(numerator * other.numerator,
    denominator * other.denominator)
}

object Rational {
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  implicit def toRational(n: Int) = new Rational(n, 1)

  implicit def toDouble(r: Rational) = (r.numerator : Double) / r.denominator
}