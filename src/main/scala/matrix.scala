package org.expr.mcdm

import scala.math as math

object Matrix:

  def prettyPrint(a: Mat): String =
    a.map(row => row.mkString(" ")).mkString("\n")

  def prettyPrint(a: Vec): String = a.mkString(" ")

  def zeros(n: Int): Vec = Array.fill(n)(0.0)

  def zeros(n: Int, m: Int): Mat = Array.fill(n, m)(0.0)

  def ones(n: Int): Vec = Array.fill(n)(1.0)

  def ones(n: Int, m: Int): Mat = Array.fill(n, m)(1.0)

  def elementwise_equal(a: Vec, b: Vec, eps: Double = 1e-6): Boolean =
    if a.length != b.length then false
    else a.zip(b).forall((x, y) => math.abs(x - y) < eps)

  def elementwise_equal(a: Mat, b: Mat, eps: Double): Boolean =
    a.zip(b).forall((x, y) => elementwise_equal(x, y, eps))

  def fill(a: Vec, value: Double): Vec = a.map(_ => value)

  def fill(a: Mat, value: Double): Mat = a.map(row => fill(row, value))

  def identity(n: Int): Mat =
    Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  def rowsums(a: Mat): Vec = a.map(row => row.sum)

  def colsums(a: Mat): Vec = a.transpose.map(col => col.sum)

  def getrowat(a: Mat, i: Int): Vec = a(i)

  def getrowsat(a: Mat, indices: Array[Int]): Mat =
    indices.map(i => a(i))

  def getcolsat(a: Mat, indices: Array[Int]): Mat = 
    indices.map(i => a.transpose.apply(i)).transpose

  def getcolat(a: Mat, j: Int): Vec = a.map(row => row(j))

  def elementat(a: Mat, i: Int, j: Int): Double = a(i)(j)

  def elementsat(a: Vec, indices: Array[Int]): Vec =
    indices.map(i => a(i))

  def setrowat(a: Mat, i: Int, row: Vec): Mat = a.updated(i, row)

  def setcolat(a: Mat, j: Int, col: Vec): Mat =
    a.transpose.updated(j, col).transpose

  def appendrow(a: Mat, row: Vec): Mat = a :+ row

  def appendcol(a: Mat, col: Vec): Mat = (a.transpose :+ col).transpose

  def rowmins(a: Mat): Vec = a.map(row => row.min)

  def colmins(a: Mat): Vec = a.transpose.map(col => col.min)

  def rowmaxs(a: Mat): Vec = a.map(row => row.max)

  def colmaxs(a: Mat): Vec = a.transpose.map(col => col.max)

  def whichmin(a: Vec): Int = a.zipWithIndex.minBy(_._1)._2

  def whichmax(a: Vec): Int = a.zipWithIndex.maxBy(_._1)._2

  def whichmin(a: Mat): (Int, Int) =
    val (minval, (i, j)) = a.zipWithIndex
      .map((row, i) => (row.min, (i, row.indexOf(row.min))))
      .minBy(_._1)
    (i, j)

  def whichmax(a: Mat): (Int, Int) =
    val (maxval, (i, j)) = a.zipWithIndex
      .map((row, i) => (row.max, (i, row.indexOf(row.max))))
      .maxBy(_._1)
    (i, j)

  def diagonal(a: Mat): Vec = a.zipWithIndex.map((row, i) => row(i))

  def weightizeColumns(a: Mat, w: Vec): Mat =
    a.transpose
      .zip(w)
      .map((row, weight) => row.map(value => value * weight))
      .transpose

  def norm(a: Vec): Double = math.sqrt(a.map(x => x * x).sum)

  def applyFunctionToColumns(a: Mat, f: Vec => Double): Vec = a.transpose.map(f)

  def applyFunctionsToColumns(a: Mat, f: Array[Vec => Double]): Vec = 
    a.transpose
      .zip(f)
      .map((col, func) => func(col))

  def applyFunctionToRows(a: Mat, f: Vec => Double): Vec = a.map(f)

  def multiplyRowByScalar(a: Mat, i: Int, scalar: Double): Mat =
    a.updated(i, a(i).map(_ * scalar))

  def multiplyColumnByScalar(a: Mat, j: Int, scalar: Double): Mat =
    val at = a.transpose
    at.updated(j, at(j).map(_ * scalar)).transpose

  def size(a: Mat): (Int, Int) = (a.length, a(0).length)

  def inverse(a: Array[Array[Double]]): Array[Array[Double]] =
    val n = a.length
    val m = a(0).length
    val identity = Array.tabulate(n, m)((i, j) => if i == j then 1.0 else 0.0)
    var anew = a.map(_.clone)
    for i <- 0 until n do
      val pivot = anew(i)(i)
      for j <- 0 until m do
        anew(i)(j) /= pivot
        identity(i)(j) /= pivot
      for k <- 0 until n do
        if k != i then
          val factor = anew(k)(i)
          for j <- 0 until m do
            anew(k)(j) -= factor * anew(i)(j)
            identity(k)(j) -= factor * identity(i)(j)
    identity

  def colminmax(a: Mat, dirs: Array[Direction]): Vec =
    val n = a(0).length
    val result = Array.fill(n)(0.0)
    for j <- 0 until n do
      val col = getcolat(a, j)
      val currentdir = dirs(j)
      result(j) = currentdir match
        case Direction.Minimize => col.min
        case Direction.Maximize => col.max
    result

  def inversedirections(dirs: Array[Direction]): Array[Direction] =
    dirs.map {
      case Direction.Minimize => Direction.Maximize
      case Direction.Maximize => Direction.Minimize
    }

  def subtract(a: Mat, b: Mat): Mat =
    a.zip(b).map((rowa, rowb) => rowa.zip(rowb).map((x, y) => x - y))

  def makeRowMatrix(v: Vec): Mat = v.map(x => Array(x))

  def makeColumnMatrix(v: Vec): Mat = Array(v)

  def mul(a: Mat, b: Mat): Mat =
    val n = a.length
    val m = b(0).length
    val p = b.length
    val result = Array.fill(n, m)(0.0)
    for i <- 0 until n do
      for j <- 0 until m do
        for k <- 0 until p do result(i)(j) += a(i)(k) * b(k)(j)
    result

  def mul(a: Mat, b: Vec): Vec = mul(a, makeRowMatrix(b)).flatten

  def solve(A: Mat, b: Vec): Vec = mul(inverse(A), b)

  def elementwiseMultiply(a: Mat, b: Mat): Mat =
    a.zip(b).map((rowa, rowb) => rowa.zip(rowb).map((x, y) => x * y))

  def similar(a: Mat): Mat = 
    val (rows, cols) = Matrix.size(a)
    Matrix.zeros(rows, cols)

  def sumproduct(u: Vec, v: Vec): Double =
    u.zip(v).map((x, y) => x * y).sum




