import java.util.Random
import Population._
//remove if not needed
import scala.collection.JavaConversions._

object Population {

  private val elitism = 5
}

class Population(size: Int) {

  var _population: Array[Individual] = new Array[Individual](_size)

  private var _size: Int = size + elitism

  private var totalFitness: Double = 0.0

  for (i <- 0 until _size) {
    _population(i) = new Individual()
    _population(i).randGene()
  }

  this.evaluate()

  def returnSize(): Int = _size

  def setNewPop(pop: Array[Individual]) {
    System.arraycopy(pop, 0, _population, 0, _size)
  }

  def getIndividual(index: Int): Individual = _population(index)

  def rouletteSelection(): Individual = {
    val rand = new Random()
    var rNum = rand.nextDouble() * this.totalFitness
    var i: Int = 0
    i = 0
    while (i < _size && rNum > 0) {
      rNum -= _population(i).FitnessValueGetter()
      i
    }
    _population(i - 1)
  }

  def crossover(indiv1: Individual, indiv2: Individual): Array[Individual] = {
    val rand = new Random()
    val newInds = Array.ofDim[Individual](2)
    newInds(0) = new Individual()
    newInds(1) = new Individual()
    val randPivot = rand.nextInt(Individual.returnGeneSize())
    var i: Int = 0
    i = 0
    while (i < randPivot) {
      newInds(0).setGene(i, indiv1.returnGeneValue(i))
      newInds(1).setGene(i, indiv2.returnGeneValue(i))
      i
    }
    while (i < Individual.returnGeneSize()) {
      newInds(0).setGene(i, indiv2.returnGeneValue(i))
      newInds(1).setGene(i, indiv1.returnGeneValue(i))
      i
    }
    newInds
  }

  def getFittest(): Individual = {
    var fittest = _population(0)
    for (i <- 0 until popSize() if fittest.FitnessValueGetter() <= getIndividual(i).FitnessValueGetter()) {
      fittest = getIndividual(i)
    }
    fittest
  }

  def popSize(): Int = _population.length

  def returnTotalFitness(): Double = this.totalFitness

  def evaluate(): Double = {
    this.totalFitness = 0.0
    for (i <- 0 until _size) {
      this.totalFitness += _population(i).evaluate()
    }
    this.totalFitness
  }
}
