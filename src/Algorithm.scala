import java.util.Random
import java.util.Scanner
//remove if not needed
import scala.collection.JavaConversions._

object Algorithm {

  def main(args: Array[String]) {
    val ELITISM = 5
    val crossover_rate = 0.6
    val mutation_rate = 0.05
    val reader = new Scanner(System.in)
    System.out.print("Enter A Population Size: ")
    val n = reader.nextInt()
    println("Enter number of iterations: ")
    reader.reset()
    val numIterations = reader.nextInt()
    var indivs = Array.ofDim[Individual](2)
    val pop = new Population(n)
    val newPopulation = Array.ofDim[Individual](pop.returnSize())
    println("Total Fitness of Population: " + pop.returnTotalFitness())
    println("Best Fitness = " + pop.getFittest.FitnessValueGetter())
    val rand = new Random()
    var count: Int = 0
    for (i <- 0 until numIterations) {
      count = 0
      for (j <- 0 until ELITISM) {
        newPopulation(count) = pop.getFittest
        count += 1
      }
      while (count < pop.returnSize()) {
        indivs(0) = pop.rouletteSelection()
        indivs(1) = pop.rouletteSelection()
        if (rand.nextDouble() < crossover_rate) {
          indivs = pop.crossover(indivs(0), indivs(1))
        }
        if (rand.nextDouble() < mutation_rate) {
          indivs(0).mutate()
        }
        if (rand.nextDouble() < mutation_rate) {
          indivs(1).mutate()
        }
        newPopulation(count) = indivs(0)
        newPopulation(count + 1) = indivs(1)
        count += 2
      }
      pop.setNewPop(newPopulation)
      pop.evaluate()
      println("Total Fitness of Population: " + pop.returnTotalFitness())
      println("Best Fitness = " + pop.getFittest.FitnessValueGetter())
    }
    val bestIndividual = pop.getFittest
  }
}
