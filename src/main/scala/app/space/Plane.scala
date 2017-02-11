package app.space

import app.number.{Number => Num}

case class Plane[A](a: Point[A], val v: Vector[A])(implicit f: A => Num[A]) { // hyperplane in n-dimensional space
  if (v.mod == a.c(0).zero) throw new Exception("Zero vectors are not allowed")
  private val d = v**Vector(a) // right part D in equation Ax+By+Cz ... = D
  def in(p: Point[A])(implicit f: A => Num[A]) = (Vector(a,p) ** v) == 0 // is point belongs to the plane
  def ||(p: Plane[A])(implicit f: A => Num[A]) = v || p.v // is parallel to another plane
  def |-(p: Plane[A])(implicit f: A => Num[A]) = v |- p.v // is orthogonal to another plane
  // is points p1 and p2 are in different half-spaces
  def diff(p1: Point[A], p2: Point[A])(implicit f: A => Num[A]) = (v**Vector(p1) - d) * (v**Vector(p2) - d) < p1.c(0).zero
  def dist(p: Point[A])(implicit f: A => Num[A]) = (v**Vector(p) - d).abs/v.mod // distance from point to the plane
  override def toString = v.toString + " = " + d.toString
}
