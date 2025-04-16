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
      Normalization.DivideByColumnsSumNormalization,
    options: Map[String, Any] = Map.empty
): CilosResult =

  val (n, m) = Matrix.size(decmat)

  var X = Matrix.replicate(decmat)

  for j <- 0 until m do
    if directions(j) == Minimize then
      val minval = Matrix.getcolat(X, j).min
      val vv =
        Matrix.elementwiseDivide(Array.fill(n)(minval), Matrix.getcolat(X, j))
      X = Matrix.setcolat(X, j, vv)

  val artificialdirs = Array.fill(m)(Maximize)

  val normalizedmatrix =
    normalization(X, Array.emptyDoubleArray, artificialdirs)

  val highestvaluerows = Array.range(0, m).map { j =>
    Matrix.whichmax(Matrix.getcolat(normalizedmatrix, j))
  }

  val columnmax = Matrix.colmaxs(normalizedmatrix)

  val A = Array.tabulate(m, m)((i, j) =>
    Matrix.getrowat(normalizedmatrix, highestvaluerows(i).toInt)(j)
  )

  val P =
    Array.tabulate(m, m)((i, j) => (columnmax(j) - A(i)(j)) / columnmax(j))

  val Fm = Array.tabulate(m, m)((i, j) =>
    if (i == j) -Matrix.getcolat(P, i).sum
    else P(i)(j)
  )

  val Xmat = Matrix.appendrow(Fm, Matrix.ones(m))

  val yvec = Matrix.zeros(m) ++ Array(1.0)

  val W = Statistics.linearregression(Xmat, yvec)

  CilosResult(
    normalizedmatrix,
    W
  )
