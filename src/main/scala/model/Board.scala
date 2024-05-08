package com.whitehatgaming
package model

import com.whitehatgaming

case class Board(pieces: BoardArray) {
  def isPieceTheRightColor(x: Int, y: Int, color: Color): Boolean = {
    pieces(x)(y).piece match
      case Some(piece) if piece.color == color => true
      case _ => false
  }

  def findPieces(piece: Piece): List[(Int, Int)] = {
    {
      for {
        x <- pieces.indices
        y <- pieces(x).indices
        foundPiece <- pieces(x)(y).piece
        if foundPiece == piece && foundPiece.color == piece.color
      } yield (x, y)
    }.toList
  }
}

object Board:
  def resetBoard: Board = {
    import Piece.*

    val initialBoard: BoardArray = Array.tabulate(8, 8) { (x, y) =>
      (x, y) match
        case (0, 0) => Square(Some(Rook(White))) // White pieces
        case (1, 0) => Square(Some(Knight(White))) // White pieces
        case (2, 0) => Square(Some(Bishop(White))) // White pieces
        case (3, 0) => Square(Some(Queen(White))) // White pieces
        case (4, 0) => Square(Some(King(White))) // White pieces
        case (5, 0) => Square(Some(Bishop(White))) // White pieces
        case (6, 0) => Square(Some(Knight(White))) // White pieces
        case (7, 0) => Square(Some(Rook(White))) // White pieces
        case (x, 1) => Square(Some(Pawn(White))) // White pieces
        case (x, 6) => Square(Some(Pawn(Black))) // Black pieces
        case (0, 7) => Square(Some(Rook(Black))) // Black pieces
        case (1, 7) => Square(Some(Knight(Black))) // Black pieces
        case (2, 7) => Square(Some(Bishop(Black))) // Black pieces
        case (3, 7) => Square(Some(Queen(Black))) // Black pieces
        case (4, 7) => Square(Some(King(Black))) // Black pieces
        case (5, 7) => Square(Some(Bishop(Black))) // Black pieces
        case (6, 7) => Square(Some(Knight(Black))) // Black pieces
        case (7, 7) => Square(Some(Rook(Black))) // Black pieces
        case _ => Square(None) // Empty fields
    }

    Board(initialBoard)
  }


end Board
