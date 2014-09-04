/**
 * Created by i7 on 04.09.2014.
 */
import scala.math._


object Main {
  def sqrtNewton(x: Double) = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGood(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    def isGood(guess: Double, x: Double) =
      abs(pow(guess, 2) - x) < 0.001

    sqrtIter(1.0, x)
  }

  def factorial(x: Long) = {
    def fact1(n: Long, acc: Long): Long = {
      if (n == 0) acc
      else fact1(n - 1, n * acc)
    }
    fact1(x, 1)
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def main(args: Array[String]) {
    println("Square root of 17 = " + sqrtNewton(17))
    println("25! = " + factorial(25))
    println("GCD 256, 1024 = " + gcd(256, 1024))
  }
}
