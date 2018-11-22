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


