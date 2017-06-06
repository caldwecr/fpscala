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
}

