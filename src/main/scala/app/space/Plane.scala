package app.space

import app.number.{Number => Num}

case class Plane[A](a: Point[A], val v: Vector[A])(implicit f: A => Num[A]) { // hyperplane in n-dimensial space
  def in(p: Point[A])(implicit f: A => Num[A]) = (Vector(a,p) ** v) == 0
  def ||(p: Plane[A])(implicit f: A => Num[A]) = v || p.v
  private val d = v**Vector(a)
  def dist(p: Point[A])(implicit f: A => Num[A]) = (v ** Vector(p) - d).abs/v.mod
  override def toString = v.toString + " = " + d.toString
}
