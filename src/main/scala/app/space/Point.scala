package app.space

import app.number.{Number => Num}

abstract class Space[+A] {
  val n: Int // dimension
}

case class Point[A](c: A*) extends Space[A] { // point of n-dimensional space
  override val n: Int = c.size
  def +(p: Point[A])(implicit f: A => Num[A]) = Point((c zip p.c).map(x => x._1 + x._2):_*)
  def -(p: Point[A])(implicit f: A => Num[A]) = Point((c zip p.c).map(x => x._1 - x._2):_*)
  def *(k: A)(implicit f: A => Num[A]) = Point(c.map(_ * k):_*)
  def /(k: A)(implicit f: A => Num[A]) = Point(c.map(_ / k):_*)
  def dist(p: Point[A])(implicit f: A => Num[A]) = Vector(this, p).mod // distance to another Point
  override def toString = c.mkString("(",",",")")
}

