package com.whitehatgaming
package model

case class Coordinates(start: Position, end: Position) {
  def xDelta: Int = math.abs(start.x - end.x)

  def yDelta: Int = math.abs(start.y - end.y)

  override def toString: String = s"Start position {${start.x} ${start.y}}, End position: {${end.x} ${end.y}}"
}

object Coordinates:
  def fromMove(move: Move): Coordinates = Coordinates(Position(move(0), move(1)), Position(move(2), move(3)))

end Coordinates

case class Position(x: Int, y: Int)