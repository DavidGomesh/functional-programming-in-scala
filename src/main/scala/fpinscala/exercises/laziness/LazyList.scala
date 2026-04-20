package fpinscala.exercises.laziness

import scala.annotation.tailrec
import fpinscala.exercises.laziness.LazyList.cons
import fpinscala.exercises.laziness.LazyList.empty
import fpinscala.exercises.laziness.LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    @tailrec
    def loop(ll: LazyList[A], l: List[A]): List[A] =
      ll match
        case Cons(h, t) => loop(t(), h() :: l)
        case _ => l.reverse

    loop(this, List.empty)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty

  def drop(n: Int): LazyList[A] =
    this match
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, bs) => cons(f(a), bs))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, as) => if p(a) then cons(a, as) else as)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, as) => cons(a, as))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((a, bs) => f(a).append(bs))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _                        => None

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None

  def zipWith[B, C](bs: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, bs)):
      case (Cons(a, ta), Cons(b, tb)) => Some((f(a(), b()), (ta(), tb())))
      case _                          => None

  def zipAll[B](bs: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, bs)):
      case (Cons(a, ta), Cons(b, tb)) => Some(((Some(a()), Some(b())), (ta() , tb() )))
      case (Cons(a, ta), _          ) => Some(((Some(a()), None     ), (ta() , empty)))
      case (_          , Cons(b, tb)) => Some(((None     , Some(b())), (empty, tb() )))
      case _                          => None

  def startsWith[B](s: LazyList[B]): Boolean = ???
    // zipWith(s)((a, b) => a == b).forAll(r => r == true)

  def tails: LazyList[LazyList[A]] =
    unfold(this)(as => as match
      case Cons(h, t) => Some((as, t()))
      case _          => None
    ).append(LazyList(empty))


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val single = cons(a, continually(a))
    single

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def loop(n1: Int, n2: Int): LazyList[Int] =
      cons(n1, loop(n2, n1 + n2))
    loop(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty


  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))(s => s match
      case (n1, n2) => Some((n1, (n2, n1 + n2)))
    )

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)
