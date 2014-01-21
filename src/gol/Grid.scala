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

  private val neighboursMap = cells.foldLeft(Map.empty[Point,List[Point]]){(map, point) =>
    map + (point -> point.getNeighbouringPoints.filter(_.liesWithin(Point(0,0), Point(rows - 1, columns - 1))))
  }

  private val cellMap =  cells.foldLeft(Map.empty[Point,ActorRef]){(map, point) =>
    val state = if(initiallyAliveCells.contains(point)) Alive else Dead
    map + (point -> context.actorOf(Props(classOf[Cell],point, state, GenerationRules),s"cell${point.x},${point.y}"))
  }

  context.system.scheduler.schedule(0 second, 500 milli, self, Tick)

  private def init(awaitingStates : Int) : Receive = {
    case Tick =>
      cellMap.values.foreach(_ ! SendState)
      context.become(init(cells.length))
    case CellState(p,s) =>
      if(s == Alive) {
        neighboursMap(p).foreach(cellMap(_) ! AliveNeighbour)
      }
      if(awaitingStates == 1){
        cellMap.values.foreach(_ ! ChangeState)
      }
      context.become(init(awaitingStates - 1))
    case message@StateChanged(p,s) => context.parent ! message
  }

  def receive = init(0)
}
