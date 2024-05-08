package com.whitehatgaming
package model

case class Square(piece: Option[Piece])

object Square:
  def show(self: Square): Char = {
    self.piece match
      case Some(piece) if piece.color == White => piece.name.toUpper
      case Some(piece) => piece.name.toLower
      case None => ' '
  }

end Square
