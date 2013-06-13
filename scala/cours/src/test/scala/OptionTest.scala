import org.specs2.mutable._
import fr.enst.plnc2013._

class OptionSpec extends Specification {
  "The option" should {
    "be able to return an Int" in {
      Some(3).get must_== 3
    }
  }

  "isEmpty" should {
    "be true for None" in {
      None must be empty
    }
    "be false for Some" in {
      Some(42) must not be empty
    }
  }

}
