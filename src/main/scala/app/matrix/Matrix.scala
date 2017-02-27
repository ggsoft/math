package app.matrix

import app.number.{Number => Num}

abstract class Matrix[+A] {
  val a: Seq[Seq[A]]
}

case class QMatrix[A](a: Seq[Seq[A]]) extends Matrix[A] { // square matrix
  val n = a.size
  if (n == 0) throw new Exception("Empty matrix is not allowed")
  if (a.exists(_.size != n)) throw new Exception("Not suitable data for square matrix")
  def minor(i: Int, j: Int) = {
    def drop[T](a: Seq[T], i: Int) = a.indices.filter(_ != i).map(k => a(k))
    QMatrix(drop(a, i).map(b => drop(b, j)))
  }

  def sign(k: Int)(implicit f: A => Num[A]) = if (k%2==0) a(0)(0).one else -a(0)(0).one

  override def toString = a.map(_.mkString("\t")).mkString("\n")
}
