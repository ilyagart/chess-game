package com.whitehatgaming
package util

import model.Square.show
import model.{Board, Square}

object DisplayBoard:
  def display(board: Board): Unit = {
    val range = 0 to 7
    val boardSeparator = "*".repeat(15)
    println(boardSeparator)
    range.foreach { x =>
      range.foreach(y => print(show(board.pieces(y)(x)) + " "))
      println
    }
    println(boardSeparator + "\n")
  }
end DisplayBoard
