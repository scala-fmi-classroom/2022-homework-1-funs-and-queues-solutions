package homework1

import scala.annotation.tailrec

def fromDigitsAlt(digits: List[Int], radix: Int = 10): Int =
  @tailrec
  def fromDigits(digits: List[Int], acc: Int): Int =
    if digits.isEmpty then acc
    else fromDigits(digits.tail, acc * radix + digits.head)

  fromDigits(digits, 0)

def parseIntegerAlt(integer: String, radix: Int = 10): Int =
  def toNumericValue(digit: Char) =
    if '0' <= digit && digit <= '9' then digit - '0'
    else if 'A' <= digit && digit <= 'Z' then digit - 'A' + 10
    else throw new IllegalArgumentException("A non-digit found")

  if integer.nonEmpty && integer.head == '-' then -parseIntegerAlt(integer.tail, radix)
  else fromDigitsAlt(integer.toList.map(toNumericValue), radix)

def zipMapAlt[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
  @tailrec
  def zipMap(a: List[A], b: List[B], acc: List[C]): List[C] =
    if a.isEmpty || b.isEmpty then acc.reverse
    else zipMap(a.tail, b.tail, f(a.head, b.head) :: acc)

  zipMap(a, b, List.empty)

def memoize[A, B, R](f: (A, B) => R): (A, B) => R =
  val cache = scala.collection.mutable.Map.empty[(A, B), R]

  (a, b) => cache.getOrElseUpdate((a, b), f(a, b))

def countCoinChangeVariantsAlt(denominations: Set[Int], change: Int): Int =
  lazy val countCoinChangeVariants: (Set[Int], Int) => Int = memoize { (denominations, change) =>
    if change == 0 then 1
    else if change < 0 || denominations.isEmpty then 0
    else countCoinChangeVariants(denominations, change - denominations.head) +
      countCoinChangeVariants(denominations.tail, change)
  }

  countCoinChangeVariants(denominations, change)

def combinationsAlt[A](xs: List[A], n: Int): List[List[A]] =
  if n == 0 then List(Nil)
  else if xs.isEmpty then Nil
  else combinationsAlt(xs.tail, n - 1).map(xs.head :: _) :::
    combinationsAlt(xs.tail, n)
