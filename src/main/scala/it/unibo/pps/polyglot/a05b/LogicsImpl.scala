package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics
import it.unibo.pps.util.Optionals.Optional
import it.unibo.pps.util.Optionals.Optional.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private var center: Optional[(Int, Int)] = Empty()
  private var steps: Int = 0
  private val random = Random(42)

  override def tick(): Unit = center match
    case Empty() => center = Just((random.nextInt(size), random.nextInt(size)))
    case Just(_) => steps = steps + 1

  override def isOver: Boolean = center match
    case Empty() => false
    case Just((x, y)) => x - steps < 0 || x + steps >= size || y - steps < 0 || y + steps >= size

  override def hasElement(x: Int, y: Int): Boolean = center match
    case Empty() => false
    case Just((cx, cy)) =>
      val isOnRay = (x == cx) || (y == cy) || (math.abs(cx - x) == math.abs(cy - y))
      val isWithinSteps = math.max(math.abs(cx - x), math.abs(cy - y)) <= steps
      isOnRay && isWithinSteps
