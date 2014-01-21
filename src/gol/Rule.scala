package gol

trait Rule {
  def apply(currentState : State, liveNeighbours : Int) : State
}

class UnderPopulationRule extends Rule{
  def apply(currentState : State, liveNeighbours: Int) = if(currentState == Alive && liveNeighbours < 2) Dead else currentState
}

class StayAliveRule extends Rule {
  def apply(currentState : State, liveNeighbours: Int) = if(currentState == Alive && (liveNeighbours >= 2 || liveNeighbours == 3)) Alive else currentState
}

class OverCrowdingRule extends Rule {
  def apply(currentState : State,liveNeighbours: Int) = if(currentState == Alive && liveNeighbours > 3) Dead else currentState
}

class ReproductionRule extends Rule {
  def apply(currentState : State,liveNeighbours: Int) = if(currentState == Dead && liveNeighbours == 3) Alive else currentState
}

trait GenerationRules {
  def rules : List[Rule]
  def apply(currentState : State, liveNeighbours : Int) = rules.foldLeft(currentState){(state,rule) =>
    if(state == currentState) rule.apply(state, liveNeighbours) else state
  }
}

object GenerationRules extends GenerationRules{
  val rules = List(new UnderPopulationRule, new StayAliveRule, new OverCrowdingRule, new ReproductionRule)
}
