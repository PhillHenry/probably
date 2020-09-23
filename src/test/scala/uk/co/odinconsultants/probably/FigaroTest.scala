package uk.co.odinconsultants.probably

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Chain
import com.cra.figaro.library.atomic.continuous
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.util.random
import scala.language.postfixOps

/**
 * Adapted from https://github.com/p2t2/figaro/blob/7bdc7c26b011633b4e0a66decc068ffa6f8177f2/FigaroExamples/src/main/scala/com/cra/figaro/example/GaussianProcessTraining.scala
 */
class FigaroTest extends munit.FunSuite {
  test("Gaussian process") {
    // set up the model
    // y = x^2 + eps, eps ~ N(0, 1)
    val f: Double => Double = math.pow(_, 2)
    val x = Range.BigDecimal(1, 10, 1).map(_.doubleValue())
    val y = x.map(xi => f(xi) + random.nextGaussian())

    // wire together dependence structure
    val gp = new GaussianProcess(new GaussianCovarianceFunction(1 / 2.0))
    val trained = gp.train(x zip y toList)

    println(s"covariant Inverse:\n" + trained.covarianceInverse)
    println(s"alpha:\n" + trained.alpha)

    val xElement = continuous.Uniform(0, 11)
    val yElement = Chain(xElement, gp.model(_, trained))

    // estimate conditional expectation
    val observed    = 7.5
    val nIterations = 1
    xElement.observe(observed)
    var importance = Importance(nIterations, yElement)
    importance.start()
    val expectedYVal = importance.computeExpectation(yElement, (v: Double) => v)
    importance.kill()

    println(s"E[Y|X=$observed] = " + expectedYVal)
    assert(clue(expectedYVal) > clue(53))
    assert(clue(expectedYVal) < clue(59))

    // now adding an effect of y
    val zElement = Chain(yElement, (v:Double) => Normal(v + 3, 1))

    importance = Importance(nIterations, zElement)
    importance.start()
    val expectedZVal = importance.computeExpectation(zElement, (v: Double) => v)
    importance.kill()
    println(s"E[Z|X=$observed] = " + expectedZVal)

    /*
    E[Y|X=7.5] = 54.87774459868127
    E[Z|X=7.5] = 57.85821728332067
    */
    assert(clue(expectedZVal) > clue(54))
    assert(clue(expectedZVal) < clue(62))
  }
}
