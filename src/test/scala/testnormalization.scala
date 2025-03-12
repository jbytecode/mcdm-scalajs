import munit.Assertions as A

import scala.math as math
import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction._
import org.expr.mcdm.Normalization._

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
  
}
