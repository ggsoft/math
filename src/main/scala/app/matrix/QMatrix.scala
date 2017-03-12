package app.matrix

import app.number.{Number => Num}

class QMatrix[A <% Num[A]](override val a: Seq[Seq[A]]) extends Matrix[A](a) { // square matrix
val n = rows
  if (rows != cols) throw new Exception("Square matrix must have rows = cols")

  def one =
    QMatrix(
      for (i <- 0 to n - 1) yield
        for(j <- 0 to n - 1) yield if (i == j) a(0)(0).one else a(0)(0).zero
    )


  def minor(i: Int, j: Int) = {
    def drop[T](a: Seq[T], i: Int) = a.indices.filter(_ != i).map(k => a(k))
    QMatrix(drop(a, i).map(b => drop(b, j)))
  }

  def pow(n: Int): QMatrix[A] = { // power of the square matrix
  def pw(k: Int): Matrix[A] = if (k > 1) this ** pow(n-1) else this
    if (n < 0) throw new Exception("Power of matrix < 0")
    else if (n == 0) one
    else QMatrix(pw(n).a)
  }

  def inverse = { // inverse matrix
  val d = det(this)
    if (d == a(0)(0).zero) throw new Exception("Inverse matrix does not exist")
    QMatrix(a.indices.map(i => a(i).indices.map(j => sign(i+j) * det(minor(i,j))))).tr / d
  }

  def sign(k: Int) = if (k%2==0) a(0)(0).one else -a(0)(0).one

}

object QMatrix {
  def apply[A <% Num[A]](a: Seq[Seq[A]]) = new QMatrix(a)
}
