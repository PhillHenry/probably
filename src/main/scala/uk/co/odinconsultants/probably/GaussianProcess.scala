package uk.co.odinconsultants.probably

import com.cra.figaro.language.Element
import com.cra.figaro.library.atomic.continuous.Normal
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, MatrixUtils, RealMatrix, RealVector}
import scala.language.postfixOps

/**
 * General form of a covariance function,
 * taking two items of type T and producing a measure
 */
trait CovarianceFunction[T] {
  def apply(v1: T, v2: T): Double
}

/** The Gaussian, or radial basis function kernel / covariance function between univariate observations */
class GaussianCovarianceFunction(val gamma: Double) extends CovarianceFunction[Double] {

  /** Computes covariance using the L2 norm */
  override def apply(v1: Double, v2: Double): Double = {
    Math.exp(-gamma * Math.pow(v1 - v2, 2))
  }

  override def toString = "GaussianCovarianceFunction(gamma=" + gamma + ")"
}

/**
 * Estimates and performs posterior prediction from a Gaussian process.
 * @param covarianceFunction Defines the inner product between feature vectors
 * @param noiseVariance Prior variance of noise at design points
 */
class GaussianProcess[Input](var covarianceFunction: CovarianceFunction[Input], noiseVariance: Double = 0.001) {

  case class Trained(priorMean: Double,
                     covarianceInverse: RealMatrix,
                     inputs: Seq[Input],
                     alpha: RealMatrix,
                     responses: RealVector)

  /**
   * Estimate the conditional density of a new point.
   * @param newInput The point to evaluate at
   * @returns Posterior normal distribution
   */
  def model(newInput: Input, trained: Trained): Element[Double] = {
    import trained._

    val newCovariance = new ArrayRealVector((0 until inputs.length).map(i => covarianceFunction(inputs(i), newInput)).toArray)
    val variance      = 1 - covarianceInverse.preMultiply(newCovariance).dotProduct(newCovariance)
    val mean          = alpha.preMultiply(newCovariance).getEntry(0)

    Normal(priorMean + mean, Math.sqrt(variance))
  }

  /**
   * Estimates the parameters of the Gaussian process (the inverse of the covariance matrix) from data
   * @param data A sequence of input, output pairs to use when fitting the model
   */
  def train(data: List[(Input, Double)]): Trained = {
    val inputs    = data map { _._1 }
    val priorMean = (data map { _._2 } sum) / data.length
    val responses = new ArrayRealVector(data map { _._2 - priorMean } toArray)

    // construct covariance matrix
    val rows = (0 until data.length).map(i => {
      (0 until data.length).map(j => {
        covarianceFunction(data(i)._1, data(j)._1)
      }).toArray
    }).toArray

    val covariance        = new Array2DRowRealMatrix(rows, false)
    val covarianceInverse = MatrixUtils.inverse(covariance.add(MatrixUtils.createRealIdentityMatrix(data.length).scalarMultiply(noiseVariance)))
    val alpha             = covarianceInverse.multiply(new Array2DRowRealMatrix(responses toArray))

    Trained(priorMean, covarianceInverse, inputs, alpha, responses)
  }
}
