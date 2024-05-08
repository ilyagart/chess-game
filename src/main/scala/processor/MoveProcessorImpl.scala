package com.whitehatgaming
package processor

import model.{Board, Color, Square, White}
import util.DisplayBoard
import validation.MoveValidator

import scala.util.{Failure, Success}

class MoveProcessorImpl(validator: MoveValidator, var currentPlayer: Color) extends MoveProcessor {

  override def process(moves: List[String]): Unit = {
    moves.foreach(move => processLocal(move))
  }

  private def processLocal(move: String): Unit = {
    val file = UserInputFile(move)
    // reset everything
    val board: Board = Board.resetBoard
    currentPlayer = White
    println(s"Initial board for game $move")
    DisplayBoard.display(board)
    var currentMove: Array[Int] = file.nextMove()
    while {
      currentMove != null
    } do {
      correctMove(currentMove)
      validator.validate(currentMove, board, currentPlayer) match
        case Failure(exception) =>
          // don't change current player's color, no actions, just show exception in the console
          println(exception.getMessage)
        case Success(value) =>
          /*
          if value - do everything as ok
          if !value - under
           */
          val (ax, ay, bx, by) = (currentMove(0), currentMove(1), currentMove(2), currentMove(3))
          // all the actions were validated in the validator
          board.pieces(bx)(by) = board.pieces(ax)(ay)
          board.pieces(ax)(ay) = Square(None)
          currentPlayer = currentPlayer.opposite
          DisplayBoard.display(board)
      currentMove = file.nextMove()
    }
  }

  /**
   * doing (7 - move(1)) and (7 - move(3)) since Y axis input is reversed, [[UserInputFile]] should be modified
   * to match X and Y axis to extract coordinates in a more intuitive way, for example:
   * line.charAt(0) - 97, line.charAt(1) - 48, line.charAt(2) - 97, line.charAt(3) - 48
   */
  private def correctMove(move: Move): Unit = {
    move(1) = 7 - move(1)
    move(3) = 7 - move(3)
  }
}

object MoveProcessorImpl:
  def apply(validator: MoveValidator, color: Color = White): MoveProcessor = new MoveProcessorImpl(validator, color)
end MoveProcessorImpl
