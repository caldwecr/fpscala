package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def main(args: Array[String]): Unit = {
    println(List.exerciseOne)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): Option[List[A]] = as match {
    case Nil => None
    case Cons(b, bs) => Some(bs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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

