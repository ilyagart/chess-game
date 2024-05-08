package com.whitehatgaming
package model

import Direction.*

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
}

object Piece:
  case class Rook(override val color: Color) extends Piece('r', color, Set(LEFT, UP, RIGHT, DOWN))

  case class Knight(override val color: Color) extends Piece('n', color)

  case class Bishop(override val color: Color) extends Piece('b', color, diagonal)

  case class King(override val color: Color) extends Piece('k', color, all)

  case class Queen(override val color: Color) extends Piece('q', color, all)

  case class Pawn(override val color: Color) extends Piece('p', color, Set(UP, DOWN, UP_LEFT, UP_RIGHT, DOWN_LEFT, DOWN_RIGHT))

end Piece