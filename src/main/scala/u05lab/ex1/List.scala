package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] = map(f => (f, getPos(f)))

  def getPos(elem: A, init: Int = 0): Int = this match
    case h :: t =>
      if h == elem then
        init
      else
        t.getPos(elem, init+1)
    case _ => init

  def partition(pred: A => Boolean): (List[A], List[A]) = 
    (filter(pred), filter(!pred(_)))

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case h :: Nil() => h
    case h :: t => op(h, t.reduce(op))
    case _ => throw UnsupportedOperationException()

  def takeRight(n: Int): List[A] = drop(length - n)

  def takeLeft(n: Int): List[A] = reverse().takeRight(n).reverse()

  def drop(n: Int): List[A] = (this, n) match
    case (h :: t, n) =>
      if n.equals(0) then
        this
      else
        t.drop(n - 1)
    case _ => this

  def span(pred: A => Boolean): (List[A], List[A]) =
    val pivotPosition = getPos(filter(!pred(_)).get(0).get)
    (takeLeft(pivotPosition), takeRight(length-pivotPosition))

  def collect[B](fun: PartialFunction[A, B]): List[B] = filter(fun.isDefinedAt).map(fun)

object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
