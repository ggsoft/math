package app.space

import app.number.{Number => Num}

case class Plane[A](a: Point[A], val v: Vector[A])(implicit f: A => Num[A]) { // hyperplane in n-dimensional space
  def in(p: Point[A])(implicit f: A => Num[A]) = (Vector(a,p) ** v) == 0 // is point belongs to the plane
  def ||(p: Plane[A])(implicit f: A => Num[A]) = v || p.v // is parallel to another plane
  def |-(p: Plane[A])(implicit f: A => Num[A]) = v |- p.v // is orthogonal to another plane
  private val d = v**Vector(a) // right part D in equation Ax+By+Cz ... = D
  def dist(p: Point[A])(implicit f: A => Num[A]) = (v**Vector(p) - d).abs/v.mod // distance from point to the plane
  override def toString = v.toString + " = " + d.toString
}
