package com.whitehatgaming
package validation

import Direction.*
import model.*
import model.Piece.*

import scala.util.{Failure, Success, Try}

class MoveValidatorImpl extends MoveValidator {

  private val success: Success[Boolean] = Success(true)

  override def validate(move: Move, board: Board, currentPlayer: Color): Try[Move] = {
    val tryBoolean: Try[Boolean] = for {
      ((ax, ay), (bx, by)) <- validateAndExtractCoordinates(move)
      _ <- canCurrentPlayerMove(ax, ay, board, currentPlayer)
      _ <- isNotOutOfBounds(move)
      _ <- startAndEndPiecesExist(ax, ay, bx, by, board)
      result <- directionPossible(ax, ay, bx, by, board)
      //            result <- isPotentialCheck(move, board, currentPlayer)
    } yield result

    tryBoolean match
      case Failure(exception) => Failure(exception)
      case Success(_) => Success(move)
  }

  def isPotentialCheck(move: Move, board: Board, currentPlayer: Color): Try[Boolean] = {
    val (ownKingX, ownKingY): (Int, Int) = board.findPieces(King(currentPlayer)).head
    val (enemyKingX, enemyKingY): (Int, Int) = board.findPieces(King(currentPlayer.opposite)).head
    if (underAttack(board, ownKingX, ownKingY, currentPlayer))
      Failure(throw new IllegalStateException("Can't leave your king checked"))
    else if (underAttack(board, enemyKingX, enemyKingY, currentPlayer.opposite)) {
      println(s"Check! ${currentPlayer.opposite}")
      Success(false)
    } else
      Success(true)
  }

  private def underAttack(board: Board, x: Int, y: Int, color: Color): Boolean = {
    def predicate(piece: Piece, coords: (Int, Int)): Boolean =
      pieceCanAttackSquare(piece, board, (coords._1, coords._2), x, y)

    val rooksCheck: Boolean = board.findPieces(Rook(color)).exists(predicate(Rook(color), _))
    val knightsCheck: Boolean = board.findPieces(Knight(color)).exists(predicate(Knight(color), _))
    val bishopsCheck: Boolean = board.findPieces(Bishop(color)).exists(predicate(Bishop(color), _))
    val pawnsCheck: Boolean = board.findPieces(Pawn(color)).exists(predicate(Pawn(color), _))
    val queenCheck: Boolean = predicate(Queen(color), board.findPieces(Queen(color)).head)

    rooksCheck || knightsCheck || bishopsCheck || pawnsCheck || queenCheck
  }

  private def pieceCanAttackSquare(piece: Piece, board: Board, currentSquare: (Int, Int), x: Int, y: Int) = {
    canAttackThisSquare(piece, board, currentSquare._1, currentSquare._2, x, y)
  }

  private def canAttackThisSquare(piece: Piece, board: Board, x: Int, y: Int, targetX: Int, targetY: Int): Boolean = {
    // go through all pieces and find what's the first piece in every direction (possible move for knight) if it collides with king's coordinates targetX targetY
    false
  }

  private def canCurrentPlayerMove(x: Int, y: Int, board: Board, color: Color): Try[Boolean] = {
    if (board.isPieceTheRightColor(x, y, color))
      Success(true)
    else
      Failure(throw new IllegalStateException(s"Sneaky try! Right now is $color's turn"))
  }

  private def validateAndExtractCoordinates(move: Move): Try[((Int, Int), (Int, Int))] = {
    if ((0 to 3).forall(move(_).isValidInt) && move.length == 4)
      Success((move(0), move(1)), (move(2), move(3)))
    else
      Failure(throw new IllegalArgumentException(s"Could not extract coordinates for move: ${move.mkString}"))
  }

  private def isNotOutOfBounds(move: Move): Try[Boolean] = {
    if (move.exists(index => index < 0 || index > 7))
      Failure(throw new IndexOutOfBoundsException(s"Out of bounds move: ${move.mkString}"))
    else
      success
  }

  private def startAndEndPiecesExist(ax: Int, ay: Int, bx: Int, by: Int, board: Board): Try[Boolean] = {
    board.pieces(ax)(ay).piece match
      case Some(piece) => board.pieces(bx)(by).piece match {
        case Some(otherPiece) if piece.isOppositeColor(otherPiece) => success
        case None => success
        case _ => Failure(throw new IllegalArgumentException(s"Same color piece on the square! Can't eat it! End square: $bx $by"))
      }
      case None => Failure(throw new IllegalArgumentException(s"No piece on the starting square $ax $ay"))
  }

  private def getPiece(ax: Int, ay: Int, board: Board): Option[Piece] =
    board.pieces(ax)(ay).piece

  private def directionPossible(ax: Int, ay: Int, bx: Int, by: Int, board: Board): Try[Boolean] = {
    import Piece.*

    val initialPiece = getPiece(ax, ay, board).get
    val endPiece = getPiece(bx, by, board)

    def handleSameColorPiece(color: Color): Try[Boolean] = {
      if (endPiece.iterator.exists(_.color != color) || endPiece.isEmpty) success else Failure(throw new IllegalArgumentException(s"Can't eat the piece of the same color! move: $ax$ay $bx$by"))
    }

    def pieceCanMoveThatFar(piece: Piece, ax: Int, ay: Int, bx: Int, by: Int, direction: Direction) = {
      val xDelta = math.abs(ax - bx)
      val yDelta = math.abs(ay - by)
      piece match
        case Knight(_) => (xDelta == 1 && yDelta == 2) || (xDelta == 2 && yDelta == 1)
        case King(_) => piece.directions.contains(direction) && xDelta <= 1 && yDelta <= 1
        case Pawn(color) => (color == White && xDelta == 0 && ay == 1 && yDelta <= 2) || (color == Black && xDelta == 0 && ay == 6 && yDelta <= 2) || (xDelta == 0 && yDelta == 1) // plus diagonal moves
        case _ => piece.directions.contains(direction)
    }

    def isCorrectDiagonalDirection(ax: Int, ay: Int, bx: Int, by: Int, direction: Direction) = {
      if (Direction.diagonal.contains(direction))
        math.abs(ax - bx) == math.abs(ay - by)
      else
        true
    }

    Direction.all.find(direction => direction.shift == ((bx - ax).sign, (by - ay).sign)) match
      case Some(direction) if pieceCanMoveThatFar(initialPiece, ax, ay, bx, by, direction) =>
        initialPiece match
          case Rook(color) => handleSameColorPiece.andThen(_ => noPiecesInDirection(ax, ay, bx, by, board, direction))(color)
          case Knight(color) => handleSameColorPiece(color)
          case Bishop(color) if isCorrectDiagonalDirection(ax, ay, bx, by, direction) => handleSameColorPiece.andThen(_ => noPiecesInDirection(ax, ay, bx, by, board, direction))(color)
          case King(color) => handleSameColorPiece.andThen(_ => noPiecesInDirection(ax, ay, bx, by, board, direction))(color)
          case Queen(color) if isCorrectDiagonalDirection(ax, ay, bx, by, direction) => handleSameColorPiece.andThen(_ => noPiecesInDirection(ax, ay, bx, by, board, direction))(color)
          case Pawn(color) => handleSameColorPiece.andThen(_ => noPiecesInDirection(ax, ay, bx, by, board, direction))(color)
          case _ => Failure(throw new IllegalArgumentException(s"Impossible direction for move, other piece is in the way: $ax$ay $bx$by"))
      case _ => Failure(throw new IllegalArgumentException(s"Impossible direction for move: $ax$ay $bx$by"))
  }

  private def noPiecesInDirection(ax: Int, ay: Int, bx: Int, by: Int, board: Board, direction: Direction): Try[Boolean] = {
    lazy val failure = Failure(throw new IllegalArgumentException(s"There's a piece in the way! for move: $ax$ay $bx$by"))

    val positiveX = ax + 1 until bx
    val negativeX = ax - 1 until bx by -1
    val positiveY = ay + 1 until by
    val negativeY = ay - 1 until by by -1

    direction match
      case UP_LEFT => if (negativeX.zip(positiveY).exists((x, y) => board.pieces(x)(y).piece.nonEmpty)) failure else success
      case UP => if (positiveY.exists(board.pieces(ax)(_).piece.nonEmpty)) failure else success
      case UP_RIGHT => if (positiveX.zip(positiveY).exists((x, y) => board.pieces(x)(y).piece.nonEmpty)) failure else success
      case RIGHT => if (positiveX.exists(board.pieces(_)(ay).piece.nonEmpty)) failure else success
      case DOWN_RIGHT => if (positiveX.zip(negativeY).exists((x, y) => board.pieces(x)(y).piece.nonEmpty)) failure else success
      case DOWN => if (negativeY.exists(board.pieces(ax)(_).piece.nonEmpty)) failure else success
      case DOWN_LEFT => if (negativeX.zip(negativeY).exists((x, y) => board.pieces(x)(y).piece.nonEmpty)) failure else success
      case LEFT => if (negativeX.exists(board.pieces(_)(ay).piece.nonEmpty)) failure else success
  }
}

object MoveValidatorImpl:
  def apply(): MoveValidator = new MoveValidatorImpl
end MoveValidatorImpl
