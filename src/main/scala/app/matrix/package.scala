package app

import app.number.{Number => Num}

package object matrix {

  def det[A](qm: QMatrix[A])(implicit f: A => Num[A]): A = { // determinant calculation
    val a = qm.a
    val one = a(0)(0).one
    def sign(k: Int) = if (k%2==0) one else -one
    if (qm.n == 1) a(0)(0)
    else a.head.indices.map(j => sign(j)*a(0)(j)*det(qm.minor(0,j))).reduceLeft(_ + _)
  }

}
