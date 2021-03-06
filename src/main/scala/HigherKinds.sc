/*
0. Explain map, flatMap, IO[A]. Use map, flatMap on Option, List, Lens...
0.1. Write our own Functor and then Monad and use them in some examples;
0.2. Exercise: Take an Option[String] and always turn it into a value of String.;
0.3. Turn an Option[Int] into None if the integer in odd using only map or flatmap. Compare map and flatMap?
0.3. Turn an Option[Int] into Some(1) using only map or flatMap. Is it possible?
0.3. Turn an empty list into a three element list using map or flatMap.
0.4. Filter the odd valued elements of a list. [0, 2, 1, 3] => [0, 2] (or based on the index);
0.5. Compose two Functors together and try and do the same with Monad;

F[_]: Functor
G[_]: Functor
F[G[_]]: Functor
G[F[_]]: Functor

F[_]: Monad
G[_]: Monad
F[G[_]]: Monad?
G[F[_]]: Monad?



1. Use an optic to save a new record to a database and get an object with id (primary key) added;
2. Extend the prism to give an error when the parsing is not possible;
3. Use optics to decouple code (a method evaluates the cost of electricity given something that can give the degrees in Fahrenheit);
4. Replace extending Sensor without using inheritance.
 */

//OO style?
//trait Functor[A] {
//  def map[B](f: A => B): Functor[B]
//}

trait MyMaybe[+A]
case object MyNone extends MyMaybe[Nothing]
case class MySome[A](value: A) extends MyMaybe[A]

trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]

  def as[A, B](fa: F[A], b: B): F[B] = map(fa, (_: A) => b)
  def lift[A, B](f: A => B): F[A] => F[B] = map(_, f)
  def compose[G[_]](g: MyFunctor[G]): MyFunctor[F[G]] = ???
}


type ListFunctor = MyFunctor[List]
// val ListFunctor = MyFunctor(List)  Looks like function application at the type level.

val listFunctor: ListFunctor = new MyFunctor[List] {
//  override def map[A, B](fa: List[A], f: A => B) = fa.map(f)
  override def map[A, B](fa: List[A], f: A => B) = fa match {
    case Nil => Nil
    case a::as => f(a) :: map(as, f)
  }
}

val myMaybeFunctor: MyFunctor[MyMaybe] = new MyFunctor[MyMaybe] {
  override def map[A, B](fa: MyMaybe[A], f: A => B) = fa match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }
}

myMaybeFunctor.map(MyNone, (a: Int) => MyCons(a * 4, MyNil))
val myMaybeValue: MyMaybe[Int] = MySome(10)
myMaybeFunctor.map(myMaybeValue, (a: Int) => MyCons((a * 4).toString, MyNil))

val myListFunctor = new MyFunctor[MyList] {
  override def map[A, B](fa: MyList[A], f: A => B) = fa match {
    case MyCons(a, as) => MyCons(f(a), map(as, f))
    case MyNil => MyNil
  }
}

val myListValue = MyCons(2, MyCons(1, MyNil))
myListFunctor.as(myListValue, 100)

val r: Option[List[Int]] = Some(List(1, 2, 3))
r.map(_.map(_ + 1))
