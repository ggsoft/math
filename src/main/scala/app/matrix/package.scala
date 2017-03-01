package app

import app.number.{Number => Num}

package object matrix {

  def det[A](qm: QMatrix[A])(implicit f: A => Num[A]): A = { // determinant calculation
    if (qm.n == 1) qm.a(0)(0)
    else qm.a.head.indices.map(j => qm.sign(j) * qm.a(0)(j) * det(qm.minor(0, j))).reduceLeft(_ + _)
  }

}
