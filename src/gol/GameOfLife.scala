package gol

import akka.actor.{Actor, ActorSystem, Props}
import scala.Predef._
import javax.swing.{JFrame, JButton}
import java.awt.{Color, GridLayout}
import scala.util.Random

case class StateChanged(point : Point, state : State)

trait GameOfLife {
  def rows : Int
  def columns : Int
  def liveCells : List[Point]

  def init() { ActorSystem("system").actorOf(Props(classOf[GameOfLifeImpl], rows, columns, liveCells),"GAMEOFLIFE") }
}

class GameOfLifeImpl(rows : Int, columns : Int, liveCells : List[Point]) extends Actor{

  var cells = Map.empty[Point,JButton]
  val cellHeight = 20
  val cellWidth = 20

  val frame = new JFrame(){
    setLayout(new GridLayout(rows,columns))
    for{
      y <- 0 to columns-1
      x <- 0 to rows -1
      point <- Some(Point(x,y))
    } {
      val button = new JButton(){setBackground(getCellColor(point))}
      cells = cells + (point -> button)
      add(button)
    }
    setSize(rows * cellHeight, columns * cellWidth)
    setLocationRelativeTo(null)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)
  }

  private def getCellColor(point : Point) = if(liveCells.contains(point)) Color.BLACK else Color.WHITE

  context.actorOf(Props(classOf[Grid],rows, columns, liveCells),"GRID")

  def receive: Actor.Receive = {
    case StateChanged(p,Alive) => cells(p).setBackground(Color.BLACK)
    case StateChanged(p,Dead) => cells(p).setBackground(Color.WHITE)
  }
}

object Blinker extends GameOfLife with App {
  val rows = 5
  val columns = 5
  val liveCells = List(Point(1,2),Point(2,2),Point(3,2))
  init()
}

object Toad extends GameOfLife with App {
  val rows = 6
  val columns = 6
  val liveCells = List(Point(2,1),Point(2,2),Point(2,3), Point(3,2), Point(3,3), Point(3,4))
  init()
}

object Pulsar extends GameOfLife with  App {
  val rows = 17
  val columns = 17
  val liveCells = List(Point(4,2),Point(5,2),Point(6,2),Point(10,2),Point(11,2),Point(12,2),

    Point(2,4),Point(7,4),Point(9,4),Point(14,4),
    Point(2,5),Point(7,5),Point(9,5),Point(14,5),
    Point(2,6),Point(7,6),Point(9,6),Point(14,6),

    Point(4,7),Point(5,7),Point(6,7),Point(10,7),Point(11,7),Point(12,7),
    Point(4,9),Point(5,9),Point(6,9),Point(10,9),Point(11,9),Point(12,9),

    Point(2,10),Point(7,10),Point(9,10),Point(14,10),
    Point(2,11),Point(7,11),Point(9,11),Point(14,11),
    Point(2,12),Point(7,12),Point(9,12),Point(14,12),

    Point(4,14),Point(5,14),Point(6,14),Point(10,14),Point(11,14),Point(12,14)
  )
  init()
}

object RandomShape extends GameOfLife with App {
  val dimension = Random.nextInt(50)
  val rows = dimension
  val columns = dimension
  val liveCells = (for{
    x <- 0 to rows - 1
    y <- 0 to columns - 1 if Random.nextBoolean()
  } yield Point(x,y)).toList
  init()
}

