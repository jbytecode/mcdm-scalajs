import munit.Assertions as A

import scala.math as math
import org.expr.mcdm.Matrix

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
  
}
