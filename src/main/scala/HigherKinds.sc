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
  self =>
  def map[A, B](fa: F[A], f: A => B): F[B]

  def as[A, B](fa: F[A], b: B): F[B] = map(fa, (_: A) => b)
  def lift[A, B](f: A => B): F[A] => F[B] = map(_, f)

  def compose[G[_]](g: MyFunctor[G]): MyFunctor[({ type FG[A] = F[G[A]] })#FG] =
    new MyFunctor[({ type FG[A] = F[G[A]] })#FG] {
      override def map[A, B](fga: F[G[A]], f: A => B): F[G[B]] = self.map(fga, g.lift(f))
    }

  def rev_compose[G[_]](g: MyFunctor[G]): MyFunctor[({ type GF[A] = G[F[A]] })#GF] =
    new MyFunctor[({ type GF[A] = G[F[A]] })#GF] {
      override def map[A, B](gfa: G[F[A]], f: A => B): G[F[B]] = g.map(gfa, self.lift(f))
    }
}

trait MyMonad[F[_]] {
  self =>
  def point[A](a: A): F[A]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]

  def asFunctor: MyFunctor[F] = new MyFunctor[F] {
    def map[A, B](fa: F[A], f: A => B): F[B] = flatMap(fa, (a: A) => point(f(a)))
  }
}

object ListTransformer {
  def compose[F[_]](fMonad: MyMonad[F]): MyMonad[({type FG[A] = F[List[A]]})#FG] =
    new MyMonad[({type FG[A] = F[List[A]]})#FG] {
      def point[A](a: A): F[List[A]] = fMonad.point(List(a))

      def flatMap[A, B](fa: F[List[A]], f: A => F[List[B]]): F[List[B]] = {
        fMonad.flatMap[List[A], List[B]](fa, {
          case Nil => fMonad.point(Nil)
          case a :: as =>
            val head: F[List[B]] = f(a)
            val rest: F[List[B]] = flatMap(fMonad.point(as), f)
            fMonad.flatMap(head, (b: List[B]) => fMonad.flatMap(rest, (bs: List[B]) => fMonad.point(b ::: bs)))
        })
      }
    }
}

object OptionTransformer {
  def compose[F[_]](mf: MyMonad[F]): MyMonad[({type FG[A] = F[Option[A]]})#FG] =
    new MyMonad[({type FG[A] = F[Option[A]]})#FG] {
    override def point[A](a: A): F[Option[A]] = mf.point(Some(a))

    override def flatMap[A, B](fa: F[Option[A]], f: A => F[Option[B]]): F[Option[B]] = {
      mf.flatMap[Option[A], Option[B]](fa, {
        case None => mf.point(None)
        case Some(a) => f(a)
      })
    }
  }
}

// Can it be done?
//object Option2Transformer {
//  def compose[F[_]](mf: MyMonad[F]): MyMonad[({type GF[A] = Option[F[A]]})#GF] =
//    new MyMonad[({type GF[A] = Option[F[A]]})#GF] {
//      override def point[A](a: A): Option[F[A]] = Some(mf.point(a))
//
//      override def flatMap[A, B](fa: Option[F[A]], f: A => Option[F[B]]): Option[F[B]] = {
//        fa match {
//          case None => None
//          case Some(mfa) => mf.flatMap(mfa, f)
//        }
//      }
//    }
//}

//trait MyMonad[F[_]] extends MyFunctor[F] {
//  def point[A](a: A): F[A]
//  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
//
//  def map[A, B](fa: F[A], f: A => B): F[B] = flatMap(fa, (a: A) => point(f(a)))
//}

val myMaybeMonad: MyMonad[MyMaybe] = new MyMonad[MyMaybe] {
  override def point[A](a: A): MyMaybe[A] = MySome(a)

  override def flatMap[A, B](fa: MyMaybe[A], f: A => MyMaybe[B]): MyMaybe[B] = fa match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }
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

val myMaybeFunctor: MyFunctor[MyMaybe] = myMaybeMonad.asFunctor
//val myMaybeFunctor: MyFunctor[MyMaybe] = new MyFunctor[MyMaybe] {
//  override def map[A, B](fa: MyMaybe[A], f: A => B) = fa match {
//    case MyNone => MyNone
//    case MySome(a) => MySome(f(a))
//  }
//}

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
myListFunctor.map(myListValue, (a: Int) => a + 1)
myListFunctor.as(myListValue, 100)

val composed = listFunctor.compose(myMaybeFunctor)
val value: List[MyMaybe[Int]] = List(MySome(10), MySome(4), MyNone)
//val innerMap: MyMaybe[Int] => MyMaybe[Int] = (m: MyMaybe[Int]) => myMaybeFunctor.map(m, (v: Int) => v + 1)
//val innerMap: MyMaybe[Int] => MyMaybe[Int] = myMaybeFunctor.lift(_ + 1)
//myListFunctor.map(value, innerMap)
composed.map(value, (a: Int) => a + 1)
