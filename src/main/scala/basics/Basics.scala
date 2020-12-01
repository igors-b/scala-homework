package basics

import scala.annotation.tailrec

object Basics extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, (a % b).abs)

  def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)

}
