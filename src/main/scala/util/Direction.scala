package com.whitehatgaming


sealed trait Direction(val shift: (Int, Int))

object Direction {
  case object UP_LEFT extends Direction(-1, 1)

  case object UP extends Direction((0, 1))

  case object UP_RIGHT extends Direction(1, 1)

  case object RIGHT extends Direction(1, 0)

  case object DOWN_RIGHT extends Direction(1, -1)

  case object DOWN extends Direction(0, -1)

  case object DOWN_LEFT extends Direction(-1, -1)

  case object LEFT extends Direction(-1, 0)

  val all: Set[Direction] = Set(UP_LEFT, UP, UP_RIGHT, RIGHT, DOWN_RIGHT, DOWN, DOWN_LEFT, LEFT)
  val diagonal: Set[Direction] = Set(UP_LEFT, UP_RIGHT, DOWN_RIGHT, DOWN_LEFT)
}

