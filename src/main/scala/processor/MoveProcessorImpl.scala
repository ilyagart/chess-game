package com.chess
package processor

import model.*
import validation.MoveValidator

import com.whitehatgaming.UserInputFile

import scala.util.{Failure, Success}

class MoveProcessorImpl(validator: MoveValidator, var currentPlayer: Color) extends MoveProcessor {

  override def process(gameFiles: List[String]): Unit = {
    gameFiles.foreach(gameFile => processLocal(gameFile))
  }

  private def processLocal(gameFile: String): Unit = {
    val file = UserInputFile(gameFile)
    // reset everything
    val board: Board = Board.emptyBoard()
    board.resetBoard()
    currentPlayer = White
    println(s"Initial board for game $gameFile")
    board.display()
    var currentMove: Move = file.nextMove()
    while {
      currentMove != null
    } do {
      correctMove(currentMove)
      validator.validate(currentMove, board, currentPlayer) match
        case Failure(exception) =>
          println(exception.getMessage)
        case Success(value) =>
          board.performMove(currentMove)
          currentPlayer = currentPlayer.opposite
          board.display()
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
