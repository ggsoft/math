package app.number

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import app.number.Frac._

class FracTest extends FunSuite with BeforeAndAfter {
  var f1,f2: Frac = _

  before {
    f1 = Frac(27,41)
    f2 = Frac(BigInt("100250657902956342367209564"), BigInt("152232480519304075446503412"))
  }

  test("Fractions equality") {
    assert(f1==f2)
  }

  test("Fractions reduction") {
    val f = f2.reduce
    assert((f1.a==f.a)&&(f1.b==f.b))
  }

  test("Addition of fractions") {
    val f = f1+f2
    assert(f==f1*2)
  }

  test("Subtraction of fractions") {
    val f = f1-f2
    assert(f==zero)
  }

  test("Multiplying fractions and power") {
    val f = f1*f2
    assert(f==f1.pow(2))
  }

  test("Dividing of fractions") {
    val f = f1/f2
    assert(f==one)
  }

}
