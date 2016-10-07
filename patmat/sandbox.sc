
object foo {
  val t = 3
  val s = t toString
  val g = t to 8
  val z: String = null
  z.toString
}

abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def succ: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def pred = throw new IllegalArgumentException
  def succ = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new IllegalArgumentException
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def pred = n
  def succ = new Succ(this)
  def + (that: Nat) = this.succ + that.pred
  def - (that: Nat) = this.pred - that.pred
}

val xs = List(2,3,4)
val a::b::c::rest = xs
val d = xs ::: xs
val dd = xs ++ xs
val y = xs.splitAt(1)
