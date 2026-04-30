package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match
      case (n, next) if n < 0 => (-(n + 1), next)
      case result             => result

  def double(rng: RNG): (Double, RNG) =
    val (n, next) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), next)

  def doubleUsingMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, next1) = rng.nextInt
    val (d, next2) = double(next1)
    ((i, d), next2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, next1) = double(rng)
    val (d2, next2) = double(next1)
    val (d3, next3) = double(next2)
    ((d1, d2, d3), next3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) then
      (List.empty, rng)
    else
      val (i , next)  = rng.nextInt
      val (is, next2) = ints(count - 1)(next)
      (i :: is, next2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 =>
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))(
      (cr, pr) => map2(pr, cr)((as, a) => a :: as)
    )

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng1 =>
      val (a, rng2) = r(rng1)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s1 =>
        val (a, s2) = underlying(s1)
        f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
      s => (a, s)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List.empty))(
      (cs, ps) => cs.map2(ps)((a, as) => a :: as)
    )

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] = ???

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def update(m: Machine, i: Input): Machine =
    (m, i) match
      case (Machine(_    , 0      , _    ), _         ) => m
      case (Machine(true , _      , _    ), Input.Turn) => m
      case (Machine(false, _      , _    ), Input.Coin) => m
      case (Machine(true , candies, coins), Input.Coin) => Machine(false, candies    , coins + 1)
      case (Machine(false, candies, coins), Input.Turn) => Machine(true , candies - 1, coins    )
