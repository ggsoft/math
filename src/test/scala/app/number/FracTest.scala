package app.number

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class FracTest extends FunSuite with BeforeAndAfter {
  var f1,f2,f3,f4,f5: Frac = _

  before {
    f1 = Frac(27,41)
    f2 = Frac(BigInt("100250657902956342367209564"), BigInt("152232480519304075446503412"))
    f3 = Frac(2,1)
    f4 = Frac(1,2)
    f5 = Frac(0,1)
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
    assert(f==f3*f1)
  }

  test("Subtraction of fractions") {
    val f = f1-f2
    assert(f==f5)
  }

  test("Multiplying fractions and power") {
    val f = f1*f2
    assert(f==f1.pow(2))
  }

  test("Dividing of fractions") {
    val f = f1/f2
    assert(f==f3*f4)
  }

}
