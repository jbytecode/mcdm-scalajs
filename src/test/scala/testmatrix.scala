import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction

class TestMatrix extends munit.FunSuite {
  test("zeros(10)") {
    val zeros10 = Matrix.zeros(10)
    val expected = Array.fill(10)(0.0)
    A.assert(Matrix.elementwise_equal(zeros10, expected))
  }
  test("zeros(4,5)") {
    val zeros45 = Matrix.zeros(4, 5)
    val expected = Array.fill(4, 5)(0.0)
    A.assert(Matrix.elementwise_equal(zeros45, expected, 1e-6))
  }
  test("ones(10)") {
    val ones10 = Matrix.ones(10)
    val expected = Array.fill(10)(1.0)
    A.assert(Matrix.elementwise_equal(ones10, expected))
  }
  test("ones(4,5)") {
    val ones45 = Matrix.ones(4, 5)
    val expected = Array.fill(4, 5)(1.0)
    A.assert(Matrix.elementwise_equal(ones45, expected, 1e-6))
  }
  test("fill(array, 100.0)") {
    val myzeros = Matrix.zeros(10)
    val filled = Matrix.fill(myzeros, 100.0)
    val expected = Array.fill(10)(100.0)
    A.assert(Matrix.elementwise_equal(filled, expected))
  }
  test("fill(array of array, 100.0)") {
    val myzeros = Matrix.zeros(4, 5)
    val filled = Matrix.fill(myzeros, 100.0)
    val expected = Array.fill(4, 5)(100.0)
    A.assert(Matrix.elementwise_equal(filled, expected, 1e-6))
  }
  test("idendity(5)") {
    val id5 = Matrix.identity(5)
    val expected = Array.tabulate(5, 5)((i, j) => if i == j then 1.0 else 0.0)
    A.assert(Matrix.elementwise_equal(id5, expected, 1e-6))
  }
  test("colsums") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val colsums = Matrix.colsums(a)
    val expected = Array(12.0, 15.0, 18.0)
    A.assert(Matrix.elementwise_equal(colsums, expected))
  }
  test("rowsums") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val rowsums = Matrix.rowsums(a)
    val expected = Array(6.0, 15.0, 24.0)
    A.assert(Matrix.elementwise_equal(rowsums, expected))
  }
  test("getrowat") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val row = Matrix.getrowat(a, 1)
    val expected = Array(4.0, 5.0, 6.0)
    A.assert(Matrix.elementwise_equal(row, expected))
  }
  test("getcolat") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val col = Matrix.getcolat(a, 1)
    val expected = Array(2.0, 5.0, 8.0)
    A.assert(Matrix.elementwise_equal(col, expected))
  }
  test("getelementat") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val element = Matrix.elementat(a, 1, 1)
    val expected = 5.0
    A.assertEquals(element, expected)
  }
  test("setrowat") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val row = Array(10.0, 11.0, 12.0)
    val newa = Matrix.setrowat(a, 1, row)
    val expected =
      Array(Array(1.0, 2.0, 3.0), Array(10.0, 11.0, 12.0), Array(7.0, 8.0, 9.0))
    A.assert(Matrix.elementwise_equal(newa, expected, 1e-6))
  }
  test("setcolat") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val col = Array(10.0, 11.0, 12.0)
    val newa = Matrix.setcolat(a, 1, col)
    val expected =
      Array(Array(1.0, 10.0, 3.0), Array(4.0, 11.0, 6.0), Array(7.0, 12.0, 9.0))
    A.assert(Matrix.elementwise_equal(newa, expected, 1e-6))
  }
  test("appendrow") {
    val a = Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0))
    val row = Array(7.0, 8.0, 9.0)
    val newa = Matrix.appendrow(a, row)
    val expected =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    A.assert(Matrix.elementwise_equal(newa, expected, 1e-6))
  }
  test("appendcol") {
    val a = Array(Array(1.0, 2.0), Array(4.0, 5.0), Array(7.0, 8.0))
    val col = Array(3.0, 6.0, 9.0)
    val newa = Matrix.appendcol(a, col)
    val expected =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    A.assert(Matrix.elementwise_equal(newa, expected, 1e-6))
  }
  test("row mins") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val mins = Matrix.rowmins(a)
    val expected = Array(1.0, 4.0, 7.0)
    A.assert(Matrix.elementwise_equal(mins, expected))
  }
  test("row maxs") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val maxs = Matrix.rowmaxs(a)
    val expected = Array(3.0, 6.0, 9.0)
    A.assert(Matrix.elementwise_equal(maxs, expected))
  }
  test("col mins") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val mins = Matrix.colmins(a)
    val expected = Array(1.0, 2.0, 3.0)
    A.assert(Matrix.elementwise_equal(mins, expected))
  }
  test("col maxs") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val maxs = Matrix.colmaxs(a)
    val expected = Array(7.0, 8.0, 9.0)
    A.assert(Matrix.elementwise_equal(maxs, expected))
  }
  test("which min of vector") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val whichmin = Matrix.whichmin(a)
    val expected = 0
    A.assertEquals(whichmin, expected)
  }
  test("which max of vector") {
    val a = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val whichmax = Matrix.whichmax(a)
    val expected = 4
    A.assertEquals(whichmax, expected)
  }
  test("which min of matrix") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val whichmin = Matrix.whichmin(a)
    val expected = (0, 0)
    A.assertEquals(whichmin, expected)
  }
  test("which max of matrix") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val whichmax = Matrix.whichmax(a)
    val expected = (2, 2)
    A.assertEquals(whichmax, expected)
  }
  test("diagonal") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val diag = Matrix.diagonal(a)
    val expected = Array(1.0, 5.0, 9.0)
    A.assert(Matrix.elementwise_equal(diag, expected))
  }
  test("weightize columns") {
    val a =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val weights = Array(0.1, 0.2, 0.3)
    val weighted = Matrix.weightizeColumns(a, weights)
    val expected =
      Array(Array(0.1, 0.4, 0.9), Array(0.4, 1.0, 1.8), Array(0.7, 1.6, 2.7))
    A.assert(Matrix.elementwise_equal(weighted, expected, 1e-3))
  }
  test("Vector norm") {
    val a = Array(1.0, 2.0, 3.0)
    val norm = Matrix.norm(a)
    val expected = math.sqrt(14.0)
    A.assertEquals(norm, expected)
  }
  test("Multiply Row by Scalar") {
    val mat =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val scalar = 3.0
    val newmat = Matrix.multiplyRowByScalar(mat, 1, scalar)
    val expected =
      Array(Array(1.0, 2.0, 3.0), Array(12.0, 15.0, 18.0), Array(7.0, 8.0, 9.0))
    A.assert(Matrix.elementwise_equal(newmat, expected, 1e-6))
  }
  test("Multiply Column By Scalar") {
    val mat =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val scalar = 10.0
    val newmat = Matrix.multiplyColumnByScalar(mat, 1, scalar)
    val expected =
      Array(Array(1.0, 20.0, 3.0), Array(4.0, 50.0, 6.0), Array(7.0, 80.0, 9.0))
    A.assert(Matrix.elementwise_equal(newmat, expected, 1e-4))
  }
  test("Matrix size") {
    val mat = Array(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(4.0, 5.0, 6.0, 7.0),
      Array(7.0, 8.0, 9.0, 10.0)
    )
    val size = Matrix.size(mat)
    val expected = (3, 4)
    A.assertEquals(size, expected)
  }
  test("Apply Function to Columns - 1 (norm)") {
    val mat = Array(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(4.0, 5.0, 6.0, 7.0),
      Array(7.0, 8.0, 9.0, 10.0)
    )
    val f = (a: Array[Double]) => math.sqrt(a.map(x => x * x).sum)
    val result = Matrix.applyFunctionToColumns(mat, f)
    val expected = Array(
      math.sqrt(1 + 16 + 49),
      math.sqrt(4 + 25 + 64),
      math.sqrt(9 + 36 + 81),
      math.sqrt(16 + 49 + 100)
    )
    A.assert(Matrix.elementwise_equal(result, expected))
  }
  test("Apply Function to Columns - 2 (sum)") {
    val mat = Array(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(4.0, 5.0, 6.0, 7.0),
      Array(7.0, 8.0, 9.0, 10.0)
    )
    val f = (a: Array[Double]) => a.sum
    val result = Matrix.applyFunctionToColumns(mat, f)
    val expected = Array(
      1.0 + 4.0 + 7.0,
      2.0 + 5.0 + 8.0,
      3.0 + 6.0 + 9.0,
      4.0 + 7.0 + 10.0
    )
    A.assert(Matrix.elementwise_equal(result, expected))
  }
  test("Apply Function to Rows") {
    val mat = Array(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(4.0, 5.0, 6.0, 7.0),
      Array(7.0, 8.0, 9.0, 10.0)
    )
    val f = (a: Array[Double]) => a.sum
    val result = Matrix.applyFunctionToRows(mat, f)
    val expected = Array(
      1.0 + 2.0 + 3.0 + 4.0,
      4.0 + 5.0 + 6.0 + 7.0,
      7.0 + 8.0 + 9.0 + 10.0
    )
    A.assert(Matrix.elementwise_equal(result, expected))
  }
  test("Append a Column Vector to Matrix") {
    val mat =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    val col = Array(10.0, 11.0, 12.0)
    val newmat = Matrix.appendcol(mat, col)
    val expected = Array(
      Array(1.0, 2.0, 3.0, 10.0),
      Array(4.0, 5.0, 6.0, 11.0),
      Array(7.0, 8.0, 9.0, 12.0)
    )
    A.assert(Matrix.elementwise_equal(newmat, expected, 1e-6))
  }
  test("Append a Row Vector to Matrix") {
    val mat = Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0))
    val row = Array(7.0, 8.0, 9.0)
    val newmat = Matrix.appendrow(mat, row)
    val expected =
      Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0), Array(7.0, 8.0, 9.0))
    A.assert(Matrix.elementwise_equal(newmat, expected, 1e-6))
  }
  test("Inverse of a matrix") {
    val mat =
      Array(Array(1.0, 2.0, -7.0), Array(4.0, 15.0, 6.0), Array(7.0, 8.0, 9.0))
    val inv = Matrix.inverse(mat)
    val expected = Array(
      Array(0.142623, -0.121311, 0.191803),
      Array(0.00983607, 0.095082, -0.0557377),
      Array(-0.119672, 0.00983607, 0.0114754)
    )
    A.assert(Matrix.elementwise_equal(inv, expected, 1e-6))
  }
  test("Inverse of a matrix - 2") {
    val mat = Array(
      Array(1.0, 5.0, 6.0),
      Array(-1.0, 10.0, 9.0),
      Array(9.0, 17.0, 12.0)
    )
    val expected = Array(
      Array(0.157143, -0.2, 0.0714286),
      Array(-0.442857, 0.2, 0.0714286),
      Array(0.509524, -0.133333, -0.0714286)
    )
    val inv = Matrix.inverse(mat)
    A.assert(Matrix.elementwise_equal(inv, expected, 1e-6))
  }
  test("Column min max regarding to direction vector - 1") {
    val mat = Array(
      Array(1.0, 5.0, 6.0),
      Array(-1.0, 10.0, 9.0),
      Array(9.0, 17.0, 12.0)
    )
    val direction =
      Array(Direction.Minimize, Direction.Maximize, Direction.Minimize)
    val values = Matrix.colminmax(mat, direction)
    val expected = Array(-1.0, 17.0, 6.0)
    A.assert(Matrix.elementwise_equal(values, expected))
  }
  test("Column min max regarding to direction vector - 2") {
    val mat = Array(
      Array(1.0, 5.0, 6.0, 10.0, 10.0),
      Array(-1.0, 10.0, 9.0, 11.0, 11.0),
      Array(9.0, 17.0, 12.0, 12.0, 12.0)
    )
    val direction = Array(
      Direction.Minimize,
      Direction.Maximize,
      Direction.Minimize,
      Direction.Maximize,
      Direction.Minimize
    )
    val values = Matrix.colminmax(mat, direction)
    val expected = Array(-1.0, 17.0, 6.0, 12.0, 10.0)
    A.assert(Matrix.elementwise_equal(values, expected))
  }
  test("Inverse direction vector") {
    val direction = Array(
      Direction.Minimize,
      Direction.Maximize,
      Direction.Minimize,
      Direction.Maximize,
      Direction.Minimize
    )
    val inv = Matrix.inversedirections(direction)
    val expected = Array(
      Direction.Maximize,
      Direction.Minimize,
      Direction.Maximize,
      Direction.Minimize,
      Direction.Maximize
    )
    direction.zip(expected).foreach { case (a, b) => A.assertNotEquals(a, b) }
  }
  test("Matrix subtract") {
    val a = Array(
      Array(1.0, 5.0, 6.0, 10.0, 10.0),
      Array(-1.0, 11.0, 9.0, 11.0, 11.0),
      Array(9.0, 17.0, 12.0, 12.0, 12.0)
    )
    val b = Array(
      Array(2.0, 5.0, 6.0, 10.0, 10.0),
      Array(-1.0, 10.0, 9.0, 11.0, 11.0),
      Array(9.0, 17.0, 12.0, 12.0, 13.0)
    )
    val result = Matrix.subtract(a, b)
    val expected = Array(
      Array(-1.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.0, 1.0, 0.0, 0.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, -1.0)
    )
    A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }
  test("Matrix Multiplication") {
    val Amat = Array(
      Array(1.0, 2.0, 3.0),
      Array(4.0, -5.0, 6.0),
      Array(7.0, 8.0, 9.0)
    )
    val result = Matrix.mul(Amat, Amat)
    val expected = Array(
      Array(30.0, 16.0, 42.0),
      Array(26.0, 81.0, 36.0),
      Array(102.0, 46.0, 150.0)
    )
    A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }
  test("Matrix Multiplication 4x4") {
    val Amat = Array(
      Array(0.413366, 0.0846592, 0.881597, 0.241684),
      Array(0.447778, 0.339442, 0.6959, 0.666622),
      Array(0.0650627, 0.511026, 0.797978, 0.522368),
      Array(0.725426, 0.12043, 0.454939, 0.362997)
    )

    val Bmat = Array(
      Array(0.875121, 0.0988522),
      Array(0.518189, 0.178776),
      Array(0.827807, 0.105984),
      Array(0.659508, 0.555764)
    )

    val result = Matrix.mul(Amat, Bmat)

    val expected = Array(
      Array(1.2948, 0.283751),
      Array(1.58347, 0.549186),
      Array(1.32682, 0.472677),
      Array(1.31324, 0.343197)
    )

    A.assert(Matrix.elementwise_equal(result, expected, 1e-5))
  }
}
