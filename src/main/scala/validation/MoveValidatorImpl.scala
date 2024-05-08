package com.whitehatgaming
package validation

import model.*
import model.Piece.*
import util.Direction
import util.Direction.*

import scala.util.{Failure, Success, Try}

class MoveValidatorImpl extends MoveValidator {

  private val success: Success[Boolean] = Success(true)
  private lazy val failure = (ex: Exception) => Failure(throw ex)

  override def validate(move: Move, board: Board, currentPlayer: Color): Try[Boolean] = {
    for {
      coordinates <- validateAndExtractCoordinates(move)
      _ <- canCurrentPlayerMove(coordinates, board, currentPlayer)
      _ <- isNotOutOfBounds(move)
      _ <- startAndEndPiecesExist(coordinates, board)
      _ <- directionPossible(coordinates, board)
      result <- isPotentialCheck(move, board, currentPlayer)
    } yield result
  }

  private def isPotentialCheck(move: Move, board: Board, currentPlayer: Color): Try[Boolean] = {
    // clone the board and see if the move potentially results in a king check
    val square: BoardArray = board.square.map(_.clone())
    val futureBoard = Board.from(square)
    futureBoard.performMove(move)
    val enemyColor = currentPlayer.opposite
    val ownKing: Position = futureBoard.getKingCoordinates(currentPlayer)
    val enemyKing: Position = futureBoard.getKingCoordinates(enemyColor)

    if (kingChecked(futureBoard, ownKing, currentPlayer))
      failure(new IllegalStateException("Can't leave your king checked, invalid move"))
    else if (kingChecked(futureBoard, enemyKing, enemyColor)) {
      println(s"Check! $enemyColor")
      success
    } else
      success
  }

  private def kingChecked(board: Board, kingPosition: Position, color: Color): Boolean =
    board.findAllEnemyPieces(color).exists(piecePosition => canAttackThisSquare(board, Coordinates(piecePosition, kingPosition)))

  private def canAttackThisSquare(board: Board, coordinates: Coordinates): Boolean =
    pieceCanReachTarget(coordinates, board)

  private def canCurrentPlayerMove(coordinates: Coordinates, board: Board, color: Color): Try[Boolean] =
    if (board.isPieceTheRightColor(coordinates.start, color))
      success
    else
      failure(new IllegalStateException(s"Sneaky try! Right now is $color's turn"))

  private def validateAndExtractCoordinates(move: Move): Try[Coordinates] =
    if ((0 to 3).forall(move(_).isValidInt) && move.length == 4)
      Success(Coordinates.fromMove(move))
    else
      failure(new IllegalArgumentException(s"Could not extract coordinates for move: ${move.mkString}"))

  private def isNotOutOfBounds(move: Move): Try[Boolean] =
    if (move.exists(index => index < 0 || index > 7))
      failure(new IndexOutOfBoundsException(s"Out of bounds move: ${move.mkString}"))
    else
      success

  private def startAndEndPiecesExist(coordinates: Coordinates, board: Board): Try[Boolean] =
    board.getPieceAtCoordinates(coordinates.start) match
      case Some(startPiece) => board.getPieceAtCoordinates(coordinates.end) match {
        case Some(endPiece) if startPiece.isOppositeColor(endPiece) => success
        case None => success
        case _ => failure(new IllegalArgumentException(s"Same color piece on the square! Can't eat it! End square: ${coordinates.end}"))
      }
      case None => failure(new IllegalArgumentException(s"No piece on the starting square ${coordinates.start}"))

  private def noCollidingPieces(coordinates: Coordinates, board: Board, direction: Direction): Boolean =
    !direction.findCollidingPieces(coordinates, board).exists(_.nonEmpty)

  private def pieceCanReachTarget(coordinates: Coordinates, board: Board): Boolean = {
    val startPiece = board.getPieceAtCoordinates(coordinates.start).get

    Direction.find(coordinates) match
      case Some(direction) if startPiece.canMoveTo(direction, coordinates) =>
        startPiece match
          case Knight(_) => true
          case _ if noCollidingPieces(coordinates, board, direction) => true
          case _ => false
      case _ => false
  }

  private def directionPossible(coordinates: Coordinates, board: Board): Try[Boolean] =
    if (pieceCanReachTarget(coordinates, board))
      success
    else
      failure(new IllegalArgumentException(s"Impossible direction for move: ${coordinates.toString}"))
}

object MoveValidatorImpl:
  def apply(): MoveValidator = new MoveValidatorImpl
end MoveValidatorImpl
