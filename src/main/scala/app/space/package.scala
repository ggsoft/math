package app

import app.number.{Number => Num}
import app.matrix._

package object space {

  def mix[A](s: Vector[A]*)(implicit f: A => Num[A]): A = { // mixed product of vectors
    if (s.size < 3) throw new Exception("Number of vectors should be > 2")
    if (s.exists(_.n != s.size)) throw new Exception(s"${s.size} vectors expected")
    det(QMatrix(s.map(_.c)))
  }

  def volume[A](s: Vector[A]*)(implicit f: A => Num[A]): A = mix(s:_*).abs // volume of n-dimensional parallelepiped

}
