object functionsAndData {

  val x = new Rational(2, 6)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.add(y)
  x.neg
  y.sub(x)
  x.sub(y)
  x.sub(y).sub(z)



  class Rational(x: Int, y: Int) {
    /**
     * unsatisfied require will throw the illegalargumentexception whereas
     * unsatisfied 'assert' will throw assertionunsatisfiedexception
     */
    require(y !=0, "Denominator must be nonzero")
    private def gcd(a:Int,b:Int): Int = if (b==0) a else gcd(b, a % b)

      /** Using vals here so that the computations are performed
        * at the construction and not at every call for the class
        * private fields will not be reachable from outside the class
         */
    private val g = gcd(x,y)
    val numerator = x/g
    val denominator = y/g

    //alternative constructor syntax:
    def this(x:Int) = this(x,1)

    def less(that:Rational) = numerator * that.denominator < that.numerator * denominator
    def max(that: Rational) = if (this.less(that)) that else this
    def add(that: Rational) = new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )
    def sub(that: Rational) = add(that.neg)

    def neg = new Rational(-numerator,denominator)
    override def toString = numerator.toString + '/' + denominator.toString
  }

}