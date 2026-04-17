package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (z, _) => z + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (z, x) => Cons(x, z))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, z) => Cons(x, z))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List[Int](), (x, z) => Cons(x + 1, z))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (x, z) => Cons(x.toString, z))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, List[B](), (a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (a, z) => if f(a) then Cons(a, z) else z)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List[B](), (a, bs) => append(f(a), bs))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else List())

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
      case _ => Nil

  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] = 
    (as, bs) match
      case (Cons(a, ast), Cons(b, bst)) => Cons(f(a, b), zipWith(ast, bst, f))
      case _ => Nil

  // Obs.: Not working
  // def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] = 
  //   @tailrec
  //   def loop(as: List[A], bs: List[B], cs: List[C], f: (A, B) => C): List[C] = 
  //     (as, bs) match
  //       case (Cons(a, ast), Cons(b, bst)) => 
  //         loop(ast, bst, append(cs, Cons(f(a, b), Nil)), f)
  //       case _ => Nil
  //   loop(as, bs, List(), f)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
