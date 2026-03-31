package it.unibo.pps.polyglot.a01a

import scala.util.Random

enum Result:
  case HIT, MISS, WON, LOST

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
trait NewLogics:
  def hit(row: Int, col: Int): Result

class LogicsImpl(private val size: Int, private val boat: Int) extends NewLogics:

  private val random = Random(42)
  private var missedShot = 0
  private var hitShot = 0
  private val boatRow = random.nextInt(size)
  private val boatStartCol = random.nextInt(size - boat + 1)

  println(s"DEBUG - Boat row: $boatRow, col: $boatStartCol")

  override def hit(row: Int, col: Int): Result = {
    if row == boatRow && boatStartCol + boat > col && col >= boatStartCol then
      hitShot = hitShot + 1
      if hitShot == boat then Result.WON else Result.HIT
    else
      missedShot = missedShot + 1
      if missedShot == 5 then Result.LOST else Result.MISS
  }

