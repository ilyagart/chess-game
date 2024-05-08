package com.whitehatgaming
package validation

import model.{Board, Color}

import scala.util.Try

trait MoveValidator {
  def validate(move: Move, board: Board, whoToMove: Color): Try[Boolean]
}
