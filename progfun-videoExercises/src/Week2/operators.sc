object operators {

  val x = new Rational(2, 6)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x<y
  x max z
  x+z
  x + y - z


  class Rational(x: Int, y: Int) {
    require(y !=0, "Denominator must be nonzero")
    private def gcd(a:Int,b:Int): Int = if (b==0) a else gcd(b, a % b)

    private val g = gcd(x,y)
    val numerator = x/g
    val denominator = y/g

    def this(x:Int) = this(x,1)

    /**
     * Introduce an operator for the methods less, add and sub
     */
    def < (that:Rational) = numerator * that.denominator < that.numerator * denominator
    def max(that: Rational) = if (this < that) that else this
    def + (that: Rational) = new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )
    def - (that: Rational) = this + -that

    /**
     * To define a unary operator in Scala we must use the special syntax (remember about the space before the type) but then we can use the operator normally
     */
    def unary_- :Rational = new Rational(-numerator,denominator)
    override def toString = numerator.toString + '/' + denominator.toString
  }

}