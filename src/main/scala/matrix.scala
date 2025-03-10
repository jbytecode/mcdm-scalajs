package org.expr.mcdm

import scala.math as math

type Mat = Array[Array[Double]]
type Vec = Array[Double]

object Matrix:
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
