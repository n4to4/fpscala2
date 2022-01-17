package fpinscala.random

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

type Rand[+A] = RNG => (A, RNG)

//type State[S, +A] = S => (A, S)

//case class State[S, +A](run: S => (A, S))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = s =>
      val (a, s2) = underlying(s)
      (f(a), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- underlying
        b <- sb
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
      val (a, s2) = underlying(s)
      f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](Nil))((s, acc) => s.map2(acc)(_ :: _))

type Rand2[A] = State[RNG, A]
