package gol

import akka.actor.{Props, ActorRef, Actor}
import scala.concurrent.duration._
import gol.Cell._
import scala.concurrent.ExecutionContext.Implicits.global
import gol.Grid.CellState

object Grid {
  case class CellState(point : Point, state : State)
}

class Grid(private val rows : Int, columns : Int, initiallyAliveCells : List[Point]) extends Actor{

  private case object Tick

  private val cells = for{
    x <- 0 to rows - 1
    y <- 0 to columns - 1
  } yield Point(x,y)

  private val cellMap =  cells.foldLeft(Map.empty[Point,ActorRef]){(map, point) =>
    val state = if(initiallyAliveCells.contains(point)) Alive else Dead
    map + (point -> context.actorOf(Props(classOf[Cell],point, state, GenerationRules),s"cell${point.x},${point.y}"))
  }

  context.system.scheduler.schedule(0 second, 500 milli, self, Tick)

  private var awaitingCurrentStates = 0

  def receive = {
    case Tick =>
      awaitingCurrentStates = cells.length
      cellMap.values.foreach(_ ! SendState)
    case CellState(p,s) =>
      awaitingCurrentStates -= 1
      if(s == Alive) {
        val neighbouringPoints = p.getNeighbouringPoints.filter(_.liesWithin(Point(0,0), Point(rows - 1, columns - 1)))
        neighbouringPoints.foreach(cellMap(_) ! AliveNeighbour)
      }
      if(awaitingCurrentStates == 0){
        cellMap.values.foreach(_ ! ChangeState)
      }
    case message@StateChanged(p,s) =>
      context.parent ! message
  }
}
