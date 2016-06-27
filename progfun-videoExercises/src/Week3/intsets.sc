object intsets {
  abstract class Intset {
    def incl(x:Int) : Intset
    def contains(x:Int) : Boolean
    def union(other:Intset) : Intset
  }

  object Empty extends Intset {
    def incl(x:Int): Intset = new Nonempty(x, Empty, Empty)
    def contains(x:Int): Boolean = false
    def union(other:Intset): Intset = other
  }

  class Nonempty(elem: Int, left: Intset, right:Intset) extends Intset {
    def contains(x:Int) : Boolean = {
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true
    }
    def incl(x:Int):Intset = {
      if (x<elem) new Nonempty(elem, left incl x, right)
      else if (x>elem) new Nonempty(elem, left, right incl x)
      else this
    }
    def union(other:Intset): Intset = {
      ((left union right) union other) incl elem
    }
  }
}