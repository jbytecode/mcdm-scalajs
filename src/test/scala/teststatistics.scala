import munit.Assertions as A

import scala.math as math
import org.expr.mcdm.Matrix
import org.expr.mcdm.Statistics

class TestStatistics extends munit.FunSuite {

  test("Mean of vector") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val mean = Statistics.mean(a)
    val expected = 3.0
    A.assertEquals(mean, expected)
  }
  test("Variance of vector") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val variance = Statistics.variance(a)
    val expected = 2.5
    A.assertEquals(variance, expected)
  }
  test("Standard deviation of vector") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val std = Statistics.std(a)
    val expected = math.sqrt(2.5)
    A.assertEquals(std, expected)
  }
  test("Correlation of vector with itself") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val covariance = Statistics.correlation(a, a)
    val expected = 1.0
    A.assertEquals(covariance, expected)
  }
  test("Correlation of two different vectors (-1)") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val b = Array(5.0, 4.0, 3.0, 2.0, 1.0)
    val covariance = Statistics.correlation(a, b)
    val expected = -1.0
    A.assertEquals(covariance, expected)
  }
  test("Correlation matrix of columns of matrix") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 90.0))
    val corr = Statistics.correlation(a)
    val expected = Array(
      Array(1.0, 1.0, 0.880812),
      Array(1.0, 1.0, 0.880812),
      Array(0.880812, 0.880812, 1.0)
    )
    A.assert(Matrix.elementwise_equal(corr, expected, 1e-6))
  }
  test("Geometric mean (with logarithmic formula)") {

    val x = Array(0.9995440160681826, 0.7608611955213163, 0.8967084105498316,
      0.9955675288968396, 0.007713272360632573)

    val expected = 0.3497950008915474

    A.assertEqualsDouble(Statistics.geomean(x), expected, 1e-6)
  }
  test("Linear Regression") {
    val X = Array(
      Array(1.0, 1.0),
      Array(1.0, 2.0),
      Array(1.0, 3.0),
      Array(1.0, 4.0),
      Array(1.0, 5.0)
    )
    val y = Array(3.0, 5.0, 7.0, 9.0, 11.0)
    val beta = Statistics.linearregression(X, y)
    val expected = Array(1.0, 2.0)
    A.assert(Matrix.elementwise_equal(beta, expected, 1e-6))
  }
  test("Median"){
    val x = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val median = Statistics.median(x)
    val expected = 3.0
    A.assertEqualsDouble(median, expected, 1e-6)

    val y = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).reverse
    val median2 = Statistics.median(y)
    val expected2 = 3.5
    A.assertEqualsDouble(median2, expected2, 1e-6)
  }

}
