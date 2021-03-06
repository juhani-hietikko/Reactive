package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val infectionRateInverted = 100  // 1 % inverted = 100 / 1
    val transmissibilityRate = 0.4
    val deathRate = 0.25
    val flightProbability = 0.01
    val reducedMobility = false
    val vaccinationRateInverted = 20000
  }

  import SimConfig._

  val persons: List[Person] = for (pid <- List.range(0, population)) yield new Person(pid)
  
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }
  import Direction._
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    if ((id + 1) % vaccinationRateInverted == 0) {
      immune = true
    }
    if (id % infectionRateInverted == 0) {
      getInfected
    }
    scheduleNextMove
    
    private def scheduleNextMove = afterDelay(movementDelay)(move)
    
    private def movementDelay = {
      if (reducedMobility && sick) {
        1 + randomBelow(20)
      } else if (reducedMobility) {
        1 + randomBelow(10)
      } else {
        1 + randomBelow(5)
      }
    }
    
    def move {
      if (!dead) {
        if (random < flightProbability) {
          row = randomBelow(roomRows)
          col = randomBelow(roomColumns)
          if (!infected && !immune && getsInfected) {
            getInfected
          }
        } else {
          val potentialMoves = List(coordsIfMoving(Up), coordsIfMoving(Right), coordsIfMoving(Down), coordsIfMoving(Left)).filter(isSafeToEnter)
          if (potentialMoves.size > 0) {
            val selectedMove = potentialMoves(randomBelow(potentialMoves.size))
            row = selectedMove._1
            col = selectedMove._2
            if (!infected && !immune && getsInfected) {
              getInfected
            }
          }
        }
        scheduleNextMove
      }
    }
    
    private def getInfected {
      if (!dead) {
        infected = true
        afterDelay(6)(becomeSick)
      }
    }
    
    private def becomeSick {
      if (!dead) {
        sick = true
        afterDelay(8)(maybeDie)
      }
    }
    
    private def maybeDie {
      if (!dead) {
        if (random < deathRate) {
          dead = true;
        } else {
          afterDelay(2)(becomeImmune)
        }
      }
    }
    
    private def becomeImmune {
      if (!dead) {
        immune = true
        sick = false
        afterDelay(2)(loseImmunity)
      }
    }
    
    private def loseImmunity {
      if (!dead) {
        immune = false
        infected = false
      }
    }
    
    private def coordsIfMoving(dir: Direction.Value) = {
      dir match {
        case Up => {
          val newRow = if (row == roomRows - 1) 0 else row + 1
          (newRow, col)
        }
        case Down => {
          val newRow = if (row == 0) roomRows - 1 else row - 1
          (newRow, col)
        }
        case Left => {
          val newCol = if (col == 0) roomColumns - 1 else col - 1
          (row, newCol)
        }
        case Right => {
          val newCol = if (col == roomColumns - 1) 0 else col + 1
          (row, newCol)
        }
      }
    }
    
    def isSafeToEnter(coords: (Int, Int)) = coords match {
      case (x, y) => !persons.exists(p => p.row == x && p.col == y && p.sick)
    }
    
    private def getsInfected = {
      if (persons.exists(p => p.row == row && p.col == col && p.infected)) {
        random < transmissibilityRate
      } else {
        false;
      }
    }
  }
}
