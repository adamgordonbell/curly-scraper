package net.degoes

import net.degoes.applications.exercises.{Console, printLine, readLine}
import scalaz.Monad
import scalaz.zio._
import scalaz.zio.console._

object FunctionalScala extends App {

  //tic tac toe
  case class Board(val board : Vector[Vector[Option[Piece]]])
  object Board{
    def create() : Board = {
      val row = Vector(None,None,None)
      Board(Vector(row,row,row))
    }
  }
  sealed trait Piece
  case class X() extends Piece
  case class O() extends Piece

  def wonHorizontal(p : Piece, b : Board): Boolean ={
    b.board.contains(Vector(p,p,p))
  }

  def wonVertical(p : Piece, b : Board): Boolean ={
    b.board.transpose.contains(Vector(p,p,p))
  }

  def wonDiagonal(p : Piece, b : Board) : Boolean = {
    val center = b.board(1)(1) == p
    val leftDiag = (b.board(0)(0) == p ) && (b.board(2)(2) == p ) && center
    val rightDiag = (b.board(0)(2) == p ) && (b.board(2)(0) == p ) && center
    rightDiag && leftDiag
  }

//  case class Action(x : Int, y : Int, piece : Piece)
//
//  case class State(actions : List[Action])
//
//  trait Query[+A] {
//    type s
//    val intial :State
//    def step(s : S, action : Action) : Either[S,A]
//  }

//  object Query {
//    def unfold[S](s : S, f : (S, Action) => Either[S,A]) : Query[A] =???
//  }


  //refinement types better than dependent types - jdegoes
  //more fp languages

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Tic Tac Toe!")


    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

//  def getChoice[F[_]: Console: Monad]: F[Int] =
//    for {
//      _      <- printLine[F]("Where to play:")
//      line   <- readLine[F]
//      letter <- line.toList match {
//        case char :: Nil if char isDigit => char.point[F]
//        case _ =>
//          printLine[F]("You did not enter a letter.") *>
//            getChoice[F]
//      }
//    } yield letter
}


object jdegoes_tictactoe {
  sealed trait Piece
  case object X extends Piece
  case object O extends Piece

  case class Position private (index: Int)
  object Position {
    def apply(index: Int): Option[Position] =
      if (index < 0 || index >= 3) None
      else Some(new Position(index))
  }

  trait Query[+A] { self =>
    type S

    val initial: S
    def step(s: S, action: Action): S

    def answer(s: S): A

    final def zip[B](that: Query[B]): Query[(A, B)] =
      new Query[(A, B)] {
        type S = (self.S, that.S)
        val initial: S = (self.initial, that.initial)
        def step(s: S, action: Action): S =
          (self.step(s._1, action), that.step(s._2, action))
        def answer(s: S): (A, B) = (self.answer(s._1), that.answer(s._2))
      }

    final def *>[B](that: Query[B]): Query[B] = self.zip(that).map(_._2)

    final def <*[B](that: Query[B]): Query[A] = self.zip(that).map(_._1)

    final def map[B](f: A => B): Query[B] =
      new Query[B] {
        type S = self.S
        val initial: S = self.initial
        def step(s: S, action: Action): S = self.step(s, action)
        def answer(s: S): B = f(self.answer(s))
      }
  }
  object Query {
    final def fold[S0](s0: S0)(f: (S0, Action) => S0): Query[S0] =
      new Query[S0] {
        type S = S0

        val initial: S = s0
        def step(s: S, action: Action): S = f(s, action)
        def answer(s: S): S = s
      }
  }

  case class Action(x: Position, y: Position, piece: Piece)

  case class State(name: String, actions: List[Action]) {
    final def query[A](q: Query[A]): A =
      q.answer(actions.foldLeft[q.S](q.initial)(q.step(_, _)))
  }

  def wonHorizontal(mark: Piece, y0: Int): Query[Boolean] =
    (Query.fold[(Boolean, Boolean, Boolean)]((false, false, false)) {
      case ((b1, b2, b3), Action(x, y, piece)) =>

        (b1 || (piece == mark && x.index == 0 && y.index == y0),
          b2 || (piece == mark && x.index == 1 && y.index == y0),
          b3 || (piece == mark && x.index == 2 && y.index == y0))
    }).map(t => t._1 && t._2 && t._3)

  def wonHorizontal(mark: Piece): Query[Boolean] =
    wonHorizontal(mark, 0).zip(
      wonHorizontal(mark, 1).zip(
        wonHorizontal(mark, 2))).map {
      case (b1, (b2, b3)) => b1 || b2 || b3
    }
}
//import bloomfilter.mutable.BloomFilter
//
//val expectedElements = 1000
//val falsePositiveRate = 0.1
//val bf = BloomFilter[String](expectedElements, falsePositiveRate)
//bf.add("some string")
//bf.mightContain("some string")
//bf.dispose()

object circetest {
  import io.circe._
  import io.circe.generic.auto._
  import io.circe.parser._
  import io.circe.syntax._

  sealed trait Foo
  case class Bar(xs: Vector[String]) extends Foo
  case class Qux(i: Int, d: Option[Double]) extends Foo

  val foo: Foo = Qux(13, Some(14.0))

  val json = foo.asJson.noSpaces
  println(json)

  val decodedFoo = decode[Foo](json)
  println(decodedFoo)
}