package uk.co.odinconsultants.probably

import com.cra.figaro.language.Element
import com.cra.figaro.library.atomic.continuous.Normal
import org.apache.commons.math3.linear._

import scala.language.postfixOps

/**
  * General form of a covariance function,
  * taking two items of type T and producing a measure
  */
trait CovarianceFunction[T] {
  def apply(v1: T, v2: T): Double
}

/** The Gaussian, or radial basis function kernel / covariance function between univariate observations */
class GaussianCovarianceFunction(val gamma: Double)
    extends CovarianceFunction[Double] {

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
class GaussianProcess[Input](covarianceFunction: CovarianceFunction[Input],
                             noiseVariance: Double = 0.001
) {

  case class Trained(priorMean: Double,
                     covarianceInverse: RealMatrix,
                     inputs: Seq[Input],
                     alpha: RealMatrix,
                     responses: RealVector
  )

  /**
    * Estimate the conditional density of a new point.
    * @param newInput The point to evaluate at
    * @returns Posterior normal distribution
    */
  def model(newInput: Input, trained: Trained): Element[Double] = {
    import trained._

    val newCovariance = new ArrayRealVector(
      (0 until inputs.length)
        .map(i => covarianceFunction(inputs(i), newInput))
        .toArray
    )
//    println("newCovariance\n" + newCovariance)
    val newDotOldCo: RealVector = covarianceInverse.preMultiply(newCovariance)
//    println("newDotOldCo\n" + newDotOldCo)
    val variance      =
      1 - newDotOldCo.dotProduct(newCovariance)
    val mean          = alpha.preMultiply(newCovariance).getEntry(0)
//    println(s"newInput = $newInput, mean = $mean, variance = $variance")
    Normal(priorMean + mean, Math.sqrt(variance))
  }

  /**
    * Estimates the parameters of the Gaussian process (the inverse of the covariance matrix) from data
    * @param data A sequence of input, output pairs to use when fitting the model
    */
  def train(data: List[(Input, Double)]): Trained = {
    val inputs    = data map { _._1 }
    val n: Int = data.length
    val priorMean = (data map { _._2 } sum) / n
    val responses = new ArrayRealVector(data map { _._2 - priorMean } toArray)

    // construct covariance matrix
    val rows = (0 until n)
      .map(i => {
        (0 until n)
          .map(j => {
            covarianceFunction(data(i)._1, data(j)._1)
          })
          .toArray
      })
      .toArray

    val covariance        = new Array2DRowRealMatrix(rows, false)

//    println("covariance\n"+covariance)

    val bias: RealMatrix  = MatrixUtils
      .createRealIdentityMatrix(n)
      .scalarMultiply(noiseVariance)

//    println("bias:\n" + bias)

    val covarianceInverse = MatrixUtils.inverse(covariance.add(bias))

//    println("co . invCo\n" + covariance.add(bias).multiply(covarianceInverse)) // = I

    val responseMatrix = new Array2DRowRealMatrix(responses toArray)
    print("responseMatrix:\n" + responseMatrix)
    val alpha             =
      covarianceInverse.multiply(responseMatrix)

    Trained(priorMean, covarianceInverse, inputs, alpha, responses)
  }
}
