import java.util.Random
import Individual._
//remove if not needed
import scala.collection.JavaConversions._

object Individual {

  private val gene_size = 500

  def returnGeneSize(): Int = gene_size
}

class Individual {

  private var genes: Array[Int] = new Array[Int](gene_size)

  private var fitnessValue: Double = _

  def returnGeneValue(index: Int): Int = genes(index)

  def mutate() {
    val rand = new Random()
    val i = rand.nextInt(gene_size)
    this.setGene(i, 1 - this.returnGeneValue(i))
  }

  def setGene(index: Int, gene: Int) {
    this.genes(index) = gene
  }

  def FitnessValueGetter(): Double = fitnessValue

  def FitnessValueSetter(fitness: Int) {
    this.fitnessValue = fitness
  }

  def evaluate(): Int = {
    var fitness = 0
    for (i <- 0 until gene_size) {
      fitness += this.returnGeneValue(i)
    }
    this.FitnessValueSetter(fitness)
    fitness
  }

  def randGene() {
    val rand = new Random()
    for (i <- 0 until gene_size) {
      this.setGene(i, rand.nextInt(2))
    }
  }
}
