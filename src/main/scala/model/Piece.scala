package com.chess
package model

import util.Direction
import util.Direction.*


sealed trait Color {
  def opposite: Color
}

case object Black extends Color {
  def opposite: Color = White
}

case object White extends Color {
  def opposite: Color = Black
}

sealed trait Piece(val name: Char, val color: Color, directions: Set[Direction] = Set.empty) {
  def isOppositeColor(other: Piece): Boolean = color != other.color

  def containsDirection(direction: Direction): Boolean = directions.contains(direction)

  def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean = containsDirection(direction)
}

object Piece:
  case class Rook(override val color: Color) extends Piece('r', color, Set(LEFT, UP, RIGHT, DOWN))

  case class Knight(override val color: Color) extends Piece('n', color) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean =
      (coordinates.xDelta == 1 && coordinates.yDelta == 2) ||
        (coordinates.xDelta == 2 && coordinates.yDelta == 1)
  }

  case class Bishop(override val color: Color) extends Piece('b', color, diagonal) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean = Direction.isDiagonal(direction, coordinates)
  }

  case class King(override val color: Color) extends Piece('k', color, all) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean =
      containsDirection(direction) && coordinates.xDelta <= 1 && coordinates.yDelta <= 1
  }

  case class Queen(override val color: Color) extends Piece('q', color, all) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean = {
      val isValidDiagonalDirection = Direction.isDiagonal(direction, coordinates)
      isValidDiagonalDirection || (all -- diagonal).contains(direction)
    }
  }

  case class Pawn(override val color: Color) extends Piece('p', color, pawn) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates, board: Board): Boolean = {
      val enemyPieceOpt = board.getPieceAtCoordinates(coordinates.end).find(_.color != color)

      val attackDiagonally = coordinates.xDelta == 1 && coordinates.yDelta == 1 && enemyPieceOpt.nonEmpty
      val moveOneOrTwoVertical = coordinates.xDelta == 0 && coordinates.yDelta > 0 && coordinates.yDelta <= 2 && enemyPieceOpt.isEmpty
      val moveOneVertical = coordinates.xDelta == 0 && coordinates.yDelta == 1 && enemyPieceOpt.isEmpty

      color match
        case Black if blackPawn.contains(direction) =>
          (coordinates.start.y == 6 && moveOneOrTwoVertical) ||
            attackDiagonally ||
            moveOneVertical

        case White if whitePawn.contains(direction) =>
          (coordinates.start.y == 1 && moveOneOrTwoVertical) ||
            attackDiagonally ||
            moveOneVertical

        case _ => false
    }
  }

end Piece