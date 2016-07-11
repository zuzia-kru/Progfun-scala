package Week4

/**
 * Created by Zuzanna on 11/07/2016.
 */
//Peano numbers
abstract class Nat {
  def isZero : Boolean
  def predecessor : Nat
  def successor : Nat
  def + (that: Nat) : Nat
  def - (that: Nat) : Nat

}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new ClassNotFoundException("Negative number")
  def successor = new Succ(Zero)
  def + (that:Nat) = that
  def - (that:Nat) = if (that.isZero) Zero else throw new ClassNotFoundException("Negative Number")


}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def successor = new Succ(this)
  def + (that: Nat) = this.successor + that.predecessor
  def - (that: Nat) = if (that.isZero) this else this.predecessor - that.predecessor

}
