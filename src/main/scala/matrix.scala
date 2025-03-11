package org.expr.mcdm

import scala.math as math



object Matrix:

  def prettyPrint(a: Mat): String = a.map(row => row.mkString(" ")).mkString("\n")

  def prettyPrint(a: Vec): String = a.mkString(" ")

  def zeros(n: Int): Vec = Array.fill(n)(0.0)

  def zeros(n: Int, m: Int): Mat = Array.fill(n, m)(0.0)

  def ones(n: Int): Vec = Array.fill(n)(1.0)

  def ones(n: Int, m: Int): Mat = Array.fill(n, m)(1.0)

  def elementwise_equal(a: Vec, b: Vec, eps: Double = 1e-6): Boolean =
    a.zip(b).forall((x, y) => math.abs(x - y) < eps)

  def elementwise_equal(a: Mat, b: Mat, eps: Double): Boolean =
    a.zip(b).forall((x, y) => elementwise_equal(x, y, eps))

  def fill(a: Vec, value: Double): Vec = a.map(_ => value)

  def fill(a: Mat, value: Double): Mat = a.map(row => fill(row, value))

  def identity(n: Int): Mat =
    Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  def rowsums(a: Mat): Vec = a.map(row => row.sum)

  def colsums(a: Mat): Vec = a.transpose.map(col => col.sum)

  def getrowat(a: Mat, i: Int): Vec = a(i)

  def getcolat(a: Mat, j: Int): Vec = a.map(row => row(j))

  def elementat(a: Mat, i: Int, j: Int): Double = a(i)(j)

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
    a.transpose.zip(w).map((row, weight) => row.map(value => value * weight)).transpose    
    
  def norm(a: Vec): Double = math.sqrt(a.map(x => x * x).sum)

  def applyFunctionToColumns(a: Mat, f: Vec => Double): Vec = a.transpose.map(f)

  def applyFunctionToRows(a: Mat, f: Vec => Double): Vec = a.map(f)

  def multiplyRowByScalar(a: Mat, i: Int, scalar: Double): Mat =
    a.updated(i, a(i).map(_ * scalar))

  def multiplyColumnByScalar(a: Mat, j: Int, scalar: Double): Mat =
    val at = a.transpose
    at.updated(j, at(j).map(_ * scalar)).transpose

  def size(a: Mat): (Int, Int) = (a.length, a(0).length)

  def appendColumnVec(a: Mat, b: Vec): Mat = a :+ b

  def appendRowVec(a: Mat, b: Vec): Mat = (a.transpose :+ b).transpose

  def inverse(a: Array[Array[Double]]): Array[Array[Double]] =
    val n = a.length
    val m = a(0).length
    val identity = Array.tabulate(n, m)((i, j) => if i == j then 1.0 else 0.0)
    for i <- 0 until n do
      val pivot = a(i)(i)
      for j <- 0 until m do
        a(i)(j) /= pivot
        identity(i)(j) /= pivot
      for k <- 0 until n do
        if k != i then
          val factor = a(k)(i)
          for j <- 0 until m do
            a(k)(j) -= factor * a(i)(j)
            identity(k)(j) -= factor * identity(i)(j)
    identity

