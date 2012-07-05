package com.appspot.yesopodelkinascale

object Category extends GenericCategory[Function] {
  def id[A]: A => A = a => a
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    g compose f
}

trait GenericCategory[->>[_, _]] {
  def id[A]: A ->> A
  def compose[A, B, C](f: A ->> B, g: B ->> C): A ->> C
}

trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
  def fmap[A, B](f: A ->> B): F[A] ->>> F[B]
}

trait Functor[F[_]] extends GenericFunctor[Function, Function, F] {
  final def fmap[A, B](as: F[A])(f: A => B): F[B] = fmap(f)(as)
}

object Functor {
  def fmap[A, B, F[_]](as: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.fmap(as)(f)

  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] =
      as => as map f
  }
  
  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] =
      ls => ls map f
  }
  
  implicit object Function0Functor extends Functor[Function0] {
    def fmap[A, B](f: A => B): Function0[A] => Function0[B] =
      a => () => f(a())    
  }
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A, B](f: F[A => B]): F[A] => F[B]
  final def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
    apply(f)(fa)
  override def fmap[A, B](f: A => B): F[A] => F[B] =
    apply(pure(f))
}

object Applicative {
  
  def pure[A, F[_]](a: A)(implicit applicative: Applicative[F]): F[A] =
    applicative pure a
    
  def apply[A, B, F[_]](fa: F[A])(f: F[A => B])(implicit applicative: Applicative[F]): F[B] =
    applicative.apply(fa)(f)
    
  implicit object OptionApplicative extends Applicative[Option] {
    override def pure[A](a: A): Option[A] = Option(a)
    override def apply[A, B](f: Option[A => B]): Option[A] => Option[B] =
      o => for { a <- o; p <- f } yield p(a)
  }
  
}