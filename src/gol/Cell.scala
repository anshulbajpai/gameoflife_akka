package gol

import akka.actor.Actor
import gol.Grid.CellState
import gol.Cell._

class Cell(private val point : Point, private var state : State, private val generationRules : GenerationRules) extends Actor{
   private var liveNeighbours = 0

   def receive = {
     case SendState =>
       liveNeighbours = 0
       sender !  CellState(point,state)
     case AliveNeighbour => liveNeighbours += 1
     case ChangeState =>
       val newState = generationRules.apply(state, liveNeighbours)
       if(state != newState){
         state = newState
         sender ! StateChanged(point, state)
       }
   }
 }

sealed trait State

case object Dead extends State

case object Alive extends State

case class Point(x : Int, y : Int){
  def getNeighbouringPoints : List[Point] = List(Point(x+1,y),Point(x-1,y), Point(x,y+1), Point(x,y-1)
    ,Point(x+1,y+1), Point(x+1,y-1),Point(x-1,y+1),Point(x-1,y-1))

  def liesWithin(bottomLeft : Point, topRight : Point) = x >= bottomLeft.x && x <= topRight.x && y >= bottomLeft.y && y <= topRight.y
}

object Cell {
  case object AliveNeighbour
  case object SendState
  case object ChangeState
}