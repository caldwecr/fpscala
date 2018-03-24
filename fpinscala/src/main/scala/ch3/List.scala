package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def main(args: Array[String]): Unit = {
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sumLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)((acc, curr) => curr + acc)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def productLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)((prod, curr) => prod * curr)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(b, bs) => bs
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def setHead[A](list: List[A], h: A): List[A] = {
    if(h == ()) list
    else Cons(h, list)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(n > 0) {
      drop(tail(l), n - 1)
    }
    else l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if(f(h)) dropWhile(t, f)
      else l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)

  }

  def foldLeftUsingRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))

  def foldRightUsingLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => b + 1)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((res, curr) => Cons(curr, res))

  def exerciseOne = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => {
      println("case 0")
      x
    }
    case Nil => {
      println("case 1")
      42
    }
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => {
      println("case 2")
      x + y
    }
    case Cons(h, t) => {
      println("case 4")
      h + sum(t)
    }
    case _ => {
      println("case 5")
      101
    }
  }

  // 3.14
  def append[A](as: List[A], a: A): List[A] = {
    foldLeft(reverse(as), List(a))((acc, x) => Cons[A](x, acc))
  }

  // 3.15a
  def concat[A](front: List[A], back: List[A]): List[A] = {
    foldRightUsingLeft(front, back)((a, acc) => { Cons[A](a, acc) })
  }

  // 3.15
  def flatten[A](lists: List[List[A]]): List[A] = {
    foldRightUsingLeft(lists, Nil: List[A])((a, acc) => concat(a, acc))
  }

  // 3.16
  def increment(as: List[Int]): List[Int] = foldRightUsingLeft(as, Nil: List[Int])((a, acc) => Cons[Int](a + 1, acc))

  // 3.17
  def doubleAsString(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleAsString(xs))
  }

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightUsingLeft(as, Nil: List[B])((a, acc) => Cons[B](f(a), acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightUsingLeft(as, Nil: List[A])((a, acc) => {
      if (f(a)) Cons[A](a, acc)
      else acc
    })
  }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  // 3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as){ a =>
      if (f(a)) List(a)
      else Nil: List[A]
    }
  }
}

