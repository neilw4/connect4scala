package neilw4.c4scala.util

object Maths {
     def max(a: Int, b: Int) = math.max(a, b)
     def min(a: Int, b: Int) = math.min(a, b)
     def max(a: Int, b: Int, c: Int): Int = max(max(a, b), c)
     def min(a: Int, b: Int, c: Int): Int = min(min(a, b), c)
     def max(a: Int, b: Int, c: Int, d: Int): Int = max(max(a, b), max(c, d))
     def max(a: Int, b: Int, c: Int, d: Int, e: Int): Int = max(max(a, b, c), max(d, e))
     def min(a: Int, b: Int, c: Int, d: Int, e: Int): Int = min(min(a, b, c), min(d, e))
 }
