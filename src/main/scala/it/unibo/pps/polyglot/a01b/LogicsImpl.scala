package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.util.Optionals.Optional as ScalaOptional

import java.util.Optional
import java.util.Optional.*
import scala.jdk.javaapi.OptionConverters
import scala.util.Random

trait NewLogics:
  def hit(x: Int, y: Int): Optional[Integer]
  def won: Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends NewLogics:
  private var minesPos: Set[(Int, Int)] = Set()
  private var selectedPos: Set[(Int, Int)] = Set()
  val random = Random()

  //for i <- 0 until mines do minesPos = Sequence.Cons((random.nextInt(size), random.nextInt(size)), minesPos) first wrong idea

  while minesPos.size < mines do
    minesPos = minesPos + (random.nextInt(size) -> random.nextInt(size))

  println(s"DEBUG - Mines position: $minesPos")

  override def hit(x: Int, y: Int): Optional[Integer] =
    if minesPos.contains(x -> y) then Optional.empty() else
      selectedPos = selectedPos + (x -> y)
      val nearMines = for
        i <- x - 1 to x + 1
        j <- y - 1 to y + 1
        if (i -> j) != (x -> y)
      yield i -> j
      Optional.of(nearMines.count(minesPos.contains))

  //OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter

  override def won: Boolean =
    selectedPos.size == (size * size) - mines
