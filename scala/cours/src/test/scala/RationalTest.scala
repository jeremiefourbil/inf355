/**
 * Created with IntelliJ IDEA.
 * User: jeremiefourbil
 * Date: 13/06/13
 * Time: 16:49
 * To change this template use File | Settings | File Templates.
 */

import fr.enst.plnc2013._
import org.scalacheck._
import Prop._

object RationalTest extends Properties ("Rational") {

  val smallInt = Gen.choose(-1000, 1000)

  import Rational._

  property("gcd") = {
    forAll(smallInt, smallInt) { (a, b) =>
      (a > 0 && b > 0) ==> {
        val g = gcd(a, b)
        (g >= 1) :| "gcd greater than 0"
        (g <= a) :| "gcd smaller or equal to a"
        (g <= b) :| "gcd smaller or equal to b"
        (a % g == 0) :| "gcd divides a"
        (b % g == 0) :| "gcd divides b"
      }
    } && {
      gcd(1, 0) == 1 &&
        gcd(0, 1) == 1 &&
        gcd(0, 0) == 0
    }
  }

  property("simplified") = {
    forAll(smallInt, smallInt) { (a, b) =>
      b != 0 ==> {
        val r = Rational(a, b)
        gcd(r.numerator, r.denominator) == 1
      }

    }
  }
}