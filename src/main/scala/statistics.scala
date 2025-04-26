package org.expr.mcdm

import scala.math.{log, exp}

object Statistics:

  def maximum(a: Vec): Double = a.max

  def minimum(a: Vec): Double = a.min

  def sum(a: Vec): Double = a.sum
  
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
    val (n, m) = Matrix.size(a)
    Array.tabulate(m, m)((i, j) => correlation(Matrix.getcolat(a, i), Matrix.getcolat(a, j)))

  def euclideanDistance(a: Vec, b: Vec): Double =
    math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum)

  def geomean(x: Vec): Double = math.exp(x.map(math.log).sum / x.length)

  def linearregression(x: Mat, y: Vec): Vec =
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


