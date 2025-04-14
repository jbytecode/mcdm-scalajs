package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class CilosResult(
    normalizedmatrix: Mat,
    weights: Vec
) extends MCDMResult

def cilos(
    decmat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
    options: Map[String, Any] = Map.empty
): CilosResult =

  val (n, m) = Matrix.size(decmat)

  var X = Matrix.replicate(decmat)

  val A = Matrix.zeros(m, m)

  val P = Matrix.zeros(m, m)

  for j <- 0 until m do
    if directions(j) == Minimize then
      val minval = Matrix.getcolat(X, j).min
      val vv =
        Matrix.elementwiseDivide(Array.fill(n)(minval), Matrix.getcolat(X, j))
      X = Matrix.setcolat(X, j, vv)

  val artificialdirs = Array.fill(m)(Maximize)
  val normalizedmatrix =
    normalization(X, Array.emptyDoubleArray, artificialdirs)

  val highestvaluerows = Matrix.zeros(m)
  for j <- 0 until m do
    highestvaluerows(j) = Matrix.whichmax(Matrix.getcolat(normalizedmatrix, j))

  val columnmax = Matrix.colmaxs(normalizedmatrix)

  for i <- 0 until m do
    A(i) = Matrix.getrowat(normalizedmatrix, highestvaluerows(i).toInt)

  for i <- 0 until m do
    for j <- 0 until m do P(i)(j) = (columnmax(j) - A(i)(j)) / columnmax(j)

  val Fm = Matrix.replicate(P)
  for i <- 0 until m do Fm(i)(i) = -Matrix.getcolat(P, i).sum

  val Xmat = Matrix.appendrow(Fm, Matrix.ones(m))

  val yvec = Matrix.zeros(m) ++ Array(1.0)

  val W = Statistics.linearregression(Xmat, yvec)

  CilosResult(
    normalizedmatrix,
    W
  )
