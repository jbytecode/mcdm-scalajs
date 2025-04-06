package org.expr.mcdm

import scala.math.{log, exp}

object Statistics:

  def mean(a: Vec): Double = a.sum / a.length

  def variance(a: Vec): Double =
    val m = mean(a)
    a.map(x => (x - m) * (x - m)).sum / (a.length - 1.0)

  def std(a: Vec): Double = math.sqrt(variance(a))

  def correlation(a: Vec, b: Vec): Double =
    val ma = mean(a)
    val mb = mean(b)
    var va = variance(a)
    var vb = variance(b)
    (a.zip(b).map((x, y) => (x - ma) * (y - mb)).sum / math.sqrt(
      va * vb
    )) / (a.length - 1.0)

  def correlation(a: Mat): Mat =
    val n = a.length
    val m = a(0).length
    val cor = Matrix.zeros(m, m)
    for i <- 0 until m do
      for j <- 0 until m do
        cor(i)(j) = correlation(Matrix.getcolat(a, i), Matrix.getcolat(a, j))
    cor

  def euclideanDistance(a: Vec, b: Vec): Double =
    math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum)

  def geomean(x: Vec): Double = math.exp(x.map(math.log).sum / x.length)

  def linearregression(x: Mat, y: Vec): Vec =
    val n = x.length
    val m = x(0).length
    val xtx = Matrix.mul(x.transpose, x)
    val xty = Matrix.mul(x.transpose, Matrix.makeRowMatrix(y))
    val beta = Matrix.mul(Matrix.inverse(xtx), xty)
    beta.flatten

  def median(x: Vec): Double = 
    val L = x.length
    val sorted = x.sorted
    L match 
      case n if n % 2 == 0 => (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0
      case _ => sorted(L / 2)


