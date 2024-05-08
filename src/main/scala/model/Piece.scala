package com.whitehatgaming
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

sealed trait Piece(val name: Char, val color: Color, val directions: Set[Direction] = Set.empty) {
  def isOppositeColor(other: Piece): Boolean = color != other.color

  def containsDirection(direction: Direction): Boolean = directions.contains(direction)

  def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean = containsDirection(direction)
}

object Piece:
  case class Rook(override val color: Color) extends Piece('r', color, Set(LEFT, UP, RIGHT, DOWN))

  case class Knight(override val color: Color) extends Piece('n', color) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean =
      (coordinates.xDelta == 1 && coordinates.yDelta == 2) ||
        (coordinates.xDelta == 2 && coordinates.yDelta == 1)
  }

  case class Bishop(override val color: Color) extends Piece('b', color, diagonal) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean = Direction.isDiagonal(direction, coordinates)
  }

  case class King(override val color: Color) extends Piece('k', color, all) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean =
      containsDirection(direction) && coordinates.xDelta <= 1 && coordinates.yDelta <= 1
  }

  case class Queen(override val color: Color) extends Piece('q', color, all) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean = {
      val isValidDiagonalDirection = Direction.isDiagonal(direction, coordinates)
      isValidDiagonalDirection || (!Direction.diagonal.contains(direction) && containsDirection(direction))
      //will this work ok?
    }
  }

  case class Pawn(override val color: Color) extends Piece('p', color, Set(UP, DOWN, UP_LEFT, UP_RIGHT, DOWN_LEFT, DOWN_RIGHT)) {
    override def canMoveTo(direction: Direction, coordinates: Coordinates): Boolean = {
      containsDirection(direction) && ((color == White && coordinates.xDelta == 0 &&
        coordinates.start.y == 1 && coordinates.yDelta <= 2) ||
        (color == Black && coordinates.xDelta == 0 && coordinates.start.y == 6 && coordinates.yDelta <= 2) ||
        (coordinates.xDelta == 0 && coordinates.yDelta == 1)) // plus diagonal moves
    }
  }

end Piece