package com.chess
package processor

trait MoveProcessor {
  def process(moves: List[String]): Unit
}
