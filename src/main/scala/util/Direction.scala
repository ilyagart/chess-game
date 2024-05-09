package com.chess
package util

import model.{Board, Coordinates, Piece}


sealed trait Direction(val shift: (Int, Int)) {
  def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]]
}

object Direction {
  case object UP_LEFT extends Direction(-1, 1) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = negativeX(coordinates).zip(positiveY(coordinates)).map((x, y) => board.square(x)(y).piece)
  }

  case object UP extends Direction((0, 1)) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = positiveY(coordinates).map(board.square(coordinates.start.x)(_).piece)
  }

  case object UP_RIGHT extends Direction(1, 1) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = positiveX(coordinates).zip(positiveY(coordinates)).map((x, y) => board.square(x)(y).piece)
  }

  case object RIGHT extends Direction(1, 0) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = positiveX(coordinates).map(board.square(_)(coordinates.start.y).piece)
  }

  case object DOWN_RIGHT extends Direction(1, -1) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = positiveX(coordinates).zip(negativeY(coordinates)).map((x, y) => board.square(x)(y).piece)
  }

  case object DOWN extends Direction(0, -1) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = negativeY(coordinates).map(board.square(coordinates.start.x)(_).piece)
  }

  case object DOWN_LEFT extends Direction(-1, -1) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = negativeX(coordinates).zip(negativeY(coordinates)).map((x, y) => board.square(x)(y).piece)
  }

  case object LEFT extends Direction(-1, 0) {
    override def findCollidingPieces(coordinates: Coordinates, board: Board): Seq[Option[Piece]] = negativeX(coordinates).map(board.square(_)(coordinates.start.y).piece)
  }

  val all: Set[Direction] = Set(UP_LEFT, UP, UP_RIGHT, RIGHT, DOWN_RIGHT, DOWN, DOWN_LEFT, LEFT)
  val diagonal: Set[Direction] = Set(UP_LEFT, UP_RIGHT, DOWN_RIGHT, DOWN_LEFT)
  val whitePawn: Set[Direction] = Set(UP, UP_RIGHT, UP_LEFT)
  val blackPawn: Set[Direction] = Set(DOWN, DOWN_LEFT, DOWN_RIGHT)
  val pawn: Set[Direction] = whitePawn ++ blackPawn

  def isDiagonal(direction: Direction, coordinates: Coordinates): Boolean = diagonal.contains(direction) && math.abs(coordinates.start.x - coordinates.end.x) == math.abs(coordinates.start.y - coordinates.end.y)

  def find(coordinates: Coordinates): Option[Direction] = all.find(_.shift == ((coordinates.end.x - coordinates.start.x).sign, (coordinates.end.y - coordinates.start.y).sign))

  private def positiveRange(a: Int, b: Int): Range = a + 1 until b

  private def negativeRange(a: Int, b: Int): Range = a - 1 until b by -1

  private def positiveX(coordinates: Coordinates) = positiveRange(coordinates.start.x, coordinates.end.x)

  private def negativeX(coordinates: Coordinates) = negativeRange(coordinates.start.x, coordinates.end.x)

  private def positiveY(coordinates: Coordinates) = positiveRange(coordinates.start.y, coordinates.end.y)

  private def negativeY(coordinates: Coordinates) = negativeRange(coordinates.start.y, coordinates.end.y)

}