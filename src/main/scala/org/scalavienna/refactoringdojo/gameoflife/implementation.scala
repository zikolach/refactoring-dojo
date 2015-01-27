package org.scalavienna.refactoringdojo.gameoflife

import scala.language.{implicitConversions, postfixOps}

/**
 * You can change ANYTHING you want here (and you *really* should!)
 * Feel free to use other concepts, other names, approaches, etc.
 * Feel free to change the file-structure as well (use more files, etc.)
 * Feel free to introduce new classes, case-classes, objects, etc.
 * Finally: refactor mercilessly!
 */
trait implementation /// don't like yellow warning :)

object GoL extends implementation {

  // This Map should have only keys "x" and "y"!!!
  // It is used to store coordinates.
  // They are 1-based.
  case class Pos(x: Int, y: Int) {
    def neighbours = for {
      nx <- x - 1 to x + 1
      ny <- y - 1 to y + 1 if nx != x || ny != y
    } yield Pos(nx, ny)
  }

  object Pos {
    //// construct case class like a map
    //    def apply(elems: scala.Tuple2[String, Int]*): Pos = {
    //      val m = elems.toMap
    //      (m("x"), m( "y")) match {
    //        case (x, y) => Pos(x, y)
    //        case _ => throw new IllegalArgumentException
    //      }
    //    }
    //// implicitly convert case class to map
    //    implicit def convert(p: Pos): Map[String, Int] =
    //      Map("x" -> p.x, "y" -> p.y)
  }

  //// Properties / Fields
  var size: Int = 0
  var live_cells: Seq[Pos] = Seq.empty

  //// Initialization from String
  def from_string(in: String): Unit = {
    //// Parse lines
    val lines = in.split("\n").map(_.trim).filter(_.nonEmpty)
    //// Calculate size of world from lines
    size = lines.last.length
    //// Find live-cell positions from lines
    live_cells = for {
      y <- 0 until lines.length
      x <- 0 until lines(y).length
      if lines(y)(x) == 'O'
    } yield Pos(x + 1, y + 1)
  }

  // Calculate next iteration
  def calcNextIteration(): Unit = {
    live_cells = 1 to size flatMap { x =>
      1 to size map { y =>
        val pos = Pos(x = x, y = y)
        (pos, countAround(pos), livcell(pos))
      }
    } withFilter {
      case (pos, cnt, alive) if (cnt == 2 && alive) || cnt == 3 => true
      case _ => false
    } map (_._1)
  }

  // Is there a live cell at the given position?
  private def livcell(p: Pos): Boolean = live_cells.contains(p)

  // Produce a formatted world
  def to_str: String =
    (1 to size) map { y =>
      (1 to size) map { x =>
        if (livcell(Pos(x = x, y = y))) 'O' else '.'
      } mkString
    } mkString "\n"

  // Count live neighbours around position
  private def countAround(p: Pos): Int = {
    val liveWithin = (pos: Pos) => within(pos) && livcell(pos)
    p.neighbours.count(liveWithin)
  }

  // Is the position within world / board?
  private def within(pos: Pos): Boolean =
    pos.x > 0 && pos.x <= size && pos.y > 0 && pos.y <= size

}

