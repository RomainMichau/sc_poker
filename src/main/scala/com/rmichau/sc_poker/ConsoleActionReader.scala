package com.rmichau.sc_poker

import com.rmichau.sc_poker.core_game.ActionReader

import scala.io.StdIn.readLine

object ConsoleActionReader  extends ActionReader {
  override def apply(ask: String): String = readLine(ask)
}
