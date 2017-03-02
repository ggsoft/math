package app.matrix

import app.number.{Number => Num}
import app.space.Vector

abstract class Table[+A] {
  val a: Seq[Seq[A]]
}

class Matrix[A](val a: Seq[Seq[A]]) extends  Table[A] {
  val rows = a.size
  if (rows == 0) throw new Exception("Empty matrix is not allowed (rows = 0)")
  val cols = a(0).size
  if (cols == 0) throw new Exception("Empty matrix is not allowed (cols = 0)")
  if (a.exists(_.size != cols)) throw new Exception("Not rectangular matrix is not allowed")

  def addOk(m: Matrix[A]) = (rows == m.rows) && (cols == m.cols)
  def multOk(m: Matrix[A]) = (rows == m.cols) && (cols == m.rows)

  def +(m: Matrix[A])(implicit f: A => Num[A]) = {
    if (addOk(m)) {
      Matrix((a zip m.a).map(x => (x._1 zip x._2).map(y => y._1 + y._2 )))
    } else throw new Exception("Matrices by addition should have the same sizes")
  }

  def -(m: Matrix[A])(implicit f: A => Num[A]) = {
    if (addOk(m)) {
      Matrix((a zip m.a).map(x => (x._1 zip x._2).map(y => y._1 - y._2 )))
    } else throw new Exception("Matrices by substraction should have the same sizes")
  }

  def *(k: A)(implicit f: A => Num[A]) = Matrix(a.map(_.map(_ * k)))
  def /(k: A)(implicit f: A => Num[A]) = Matrix(a.map(_.map(_ / k)))

  // transposed matrix
  lazy val tr = Matrix[A](for(j <- 0 to cols - 1) yield (for (i <- 0 to rows - 1) yield a(i)(j)))

  def **(m: Matrix[A])(implicit f: A => Num[A]) = { // multiplication of matrices
    if (multOk(m)) {
      Matrix(a.map(x => m.tr.a.map(y => Vector(x:_*) ** Vector(y:_*))))
    } else throw new Exception("Not suitable dimensions for multiplication")
  }

  override def toString = a.map(_.mkString("\t")).mkString("\n")
}

object Matrix {
  def apply[A](a: Seq[Seq[A]]) = new Matrix(a)
}

class QMatrix[A](override val a: Seq[Seq[A]]) extends Matrix[A](a) { // square matrix
  val n = rows
  if (rows != cols) throw new Exception("Square matrix must have rows = cols")
  def minor(i: Int, j: Int) = {
    def drop[T](a: Seq[T], i: Int) = a.indices.filter(_ != i).map(k => a(k))
    QMatrix(drop(a, i).map(b => drop(b, j)))
  }

  def sign(k: Int)(implicit f: A => Num[A]) = if (k%2==0) a(0)(0).one else -a(0)(0).one

}

object QMatrix {
  def apply[A](a: Seq[Seq[A]]) = new QMatrix(a)
}
