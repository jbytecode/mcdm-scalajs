import munit.Assertions as A

import scala.math as math
import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction._
import org.expr.mcdm.Normalization._
import org.expr.mcdm.Normalization

class TestNormalizations extends munit.FunSuite {

  test("Vector Norm Normalization") {
    val mat =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val columnnorms = Matrix.applyFunctionToColumns(mat, Matrix.norm)
    Matrix.prettyPrint(columnnorms)
    val expected = Array(
      math.sqrt(1.0 * 1.0 + 4.0 * 4.0 + 7.0 * 7.0),
      math.sqrt(2.0 * 2.0 + 5.0 * 5.0 + 8.0 * 8.0),
      math.sqrt(3.0 * 3.0 + 6.0 * 6.0 + 9.0 * 9.0)
    )
    A.assert(Matrix.elementwise_equal(columnnorms, expected))
  }

  test("Divide By Columns Sum Normalization"){
    val decmat = Array(
            Array(0.616812,  0.415094 ,  0.0168757,  0.338965 ,  0.190954),
            Array(0.938958,  0.363936 ,  0.528116 ,  0.739575 ,  0.669537),
            Array(0.816357,  0.538917 ,  0.306811 ,  0.356321 ,  0.380972),
            Array(0.874629,  0.0295879,  0.340564 ,  0.0345955,  0.291483))

    val weights = Array(0.2, 0.2, 0.2, 0.2, 0.2)

    val directions = Array(Maximize, Maximize, Minimize, Minimize, Maximize)

    val normalizedMatrix = DivideByColumnnsSumNormalization(decmat, weights, directions)

    val expected = Array(
       Array(0.189978,  0.30804 ,  0.0141531,  0.230674 ,  0.124567),
       Array(0.289199,  0.270075,  0.442914 ,  0.503298 ,  0.436765),
       Array(0.251438,  0.399928,  0.257313 ,  0.242485 ,  0.248523),
       Array(0.269385,  0.021957,  0.28562  ,  0.0235431,  0.190145))
       
    A.assert(Matrix.elementwise_equal(normalizedMatrix, expected, 1e-5))
  }
  
  test("Max Min Range Normalization"){
      val mat = Array(
         Array(0.868611,  0.35464 ,  0.703252,  0.764339,  0.114943),
         Array(0.63451 ,  0.77493 ,  0.869208,  0.11187 ,  0.669837),
         Array(0.121569,  0.158548,  0.193922,  0.296039,  0.581193),
         Array(0.682846,  0.710512,  0.230163,  0.249112,  0.546892))
      val directions = Array(Maximize, Minimize, Maximize, Minimize, Maximize)
      // Weights are not used in this normalization
      val weights = Array(0.2, 0.2, 0.2, 0.2, 0.2)
      val normalizedMatrix = MaxMinRangeNormalization(mat, weights, directions)

      val expected = Array(
         Array(1.0     ,  0.681866,  0.754244 ,  0.0     ,  0.0),
         Array(0.686629,  0.0     ,  1.0      ,  1.0     ,  1.0),
         Array(0.0     ,  1.0     ,  0.0      ,  0.717735,  0.840251),
         Array(0.751333,  0.104509,  0.0536683,  0.789658,  0.778435))

      A.assert(Matrix.elementwise_equal(normalizedMatrix, expected, 1e-5))
  }

  test("Divide by column max min normalization"){
    val mat = Array(
       Array(0.0264859,  0.0565293,  0.960557,  0.765195,  0.135447),
       Array(0.31746  ,  0.631628 ,  0.142819,  0.94584 ,  0.553917),
       Array(0.166361 ,  0.197306 ,  0.725104,  0.334121,  0.202895))

    val directions = Array(Maximize, Minimize, Maximize, Minimize, Maximize)

    val result = Normalization.DivideByColumnMaxMinNormalization(mat, Array.emptyDoubleArray, directions)

    val expected = Array(
       Array(0.0834308,  1.0      ,  1.0     ,  0.436648,  0.244525),
       Array(1.0      ,  0.0894977,  0.148684,  0.353253,  1.0),
       Array(0.524038 ,  0.286505 ,  0.75488 ,  1.0     ,  0.366291))

    A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }

  test("Divide By All Norm Normalization"){
      val mat = Array(
       Array(0.0264859,  0.0565293,  0.960557,  0.765195,  0.135447),
       Array(0.31746  ,  0.631628 ,  0.142819,  0.94584 ,  0.553917),
       Array(0.166361 ,  0.197306 ,  0.725104,  0.334121,  0.202895))

      val directions = Array(Maximize, Minimize, Maximize, Minimize, Maximize)

      val result = Normalization.DivideByAllNormNormalization(mat, Array.emptyDoubleArray, directions)

      val expected = Array(
         Array(0.0132476,  0.0282746,  0.480447 ,  0.382732,  0.0677471),
         Array(0.158786 ,  0.315925 ,  0.0714348,  0.473086,  0.277056),
         Array(0.0832097,  0.0986878,  0.36268  ,  0.167119,  0.101483))

      A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }

  test("Null Normalization"){
      val mat = Array(
       Array(0.0264859,  0.0565293,  0.960557,  0.765195,  0.135447),
       Array(0.31746  ,  0.631628 ,  0.142819,  0.94584 ,  0.553917),
       Array(0.166361 ,  0.197306 ,  0.725104,  0.334121,  0.202895))

      val directions = Array(Maximize, Minimize, Maximize, Minimize, Maximize)

      val result = Normalization.NullNormalization(mat, Array.emptyDoubleArray, directions)

      val expected = mat

      A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }
  
}
