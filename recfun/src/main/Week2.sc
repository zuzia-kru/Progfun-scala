//object Exercise {
//  //Exercises in Currying given during video lectures.
//
//  //Function multiplying ints in an interval. Summing was done in the lectures
//  def product(f: Int=>Int)(a:Int, b:Int):Int =
//  if (a>b) 1 else f(a)*product(f)(a+1,b)
//
//  //Implementation of a factorial using the product function
//  def fact(n: Int) : Int = product(x=>x)(1,n)
//
//  //Generalised function summing or multiplying ints in a given interval
//  def general(g:(Int,Int)=>Int)(f:Int=>Int)(a:Int, b:Int):Int =
//    if (a>b && g(1,1)==1) 1  else if (a>b) 0 else g(f(a),general(g)(f)(a+1,b))
//
//  //Generalised reduce map given in lectures
//  def mapReduce(f:Int=>Int, combine: (Int,Int)=>Int, zero:Int)(a:Int, b:Int):Int =
//    if(a>b) zero else combine(f(a),mapReduce(f,combine,zero)(a+1,b))
//
//
//  fact(4)
//  fact(5)
//  fact(0)
//  product(x=>x)(1,5)
//  product(x=>x)(4,6)
//  product(x=>x*(x-1))(3,4)
//  general((x,y)=>x+y)(x=>x)(1,5)
//  general((x,y)=>x*y)(x=>x)(1,4)
//
//
//
//}
object exercise2 {

  import math.abs

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean = abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(next, guess)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(y => 1 + y / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

  sqrt(2)

}