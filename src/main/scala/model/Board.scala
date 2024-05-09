package com.chess
package model

import model.Piece.King
import model.Square.show

import com.chess

class Board(val square: BoardArray) {
  def isPieceTheRightColor(coordinate: Position, color: Color): Boolean = {
    square(coordinate.x)(coordinate.y).piece match
      case Some(piece) if piece.color == color => true
      case _ => false
  }

  private def findPieces(piece: Piece): Seq[Position] =
    for {
      x <- square.indices
      y <- square(x).indices
      foundPiece <- square(x)(y).piece
      if foundPiece == piece && foundPiece.color == piece.color
    } yield Position(x, y)

  def findAllEnemyPieces(color: Color): Seq[Position] = for {
    x <- square.indices
    y <- square(x).indices
    foundPiece <- square(x)(y).piece
    if foundPiece.color == color.opposite
  } yield Position(x, y)

  def getKingCoordinates(color: Color): Position = findPieces(King(color)).head

  def getPieceAtCoordinates(coordinate: Position): Option[Piece] =
    square(coordinate.x)(coordinate.y).piece

  def performMove(move: Move): Unit = {
    val coordinates = Coordinates.fromMove(move)
    square(coordinates.end.x)(coordinates.end.y) = square(coordinates.start.x)(coordinates.start.y)
    square(coordinates.start.x)(coordinates.start.y) = Square(None)
  }

  def display(): Unit = {
    val range = 0 to 7
    val boardSeparator = "*".repeat(15)
    println(boardSeparator)
    range.reverse.foreach { y =>
      range.foreach(x => print(show(square(x)(y)) + " "))
      println
    }
    println(boardSeparator + "\n")
  }

  def resetBoard(): Unit = {
    import Piece.*

    Array.tabulate(8, 8) { (x, y) =>
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
    }.copyToArray(square)
  }


}

object Board:
  def emptyBoard(): Board = new Board(Array.ofDim(8, 8))

  def from(boardArray: BoardArray): Board = new Board(boardArray)
end Board
