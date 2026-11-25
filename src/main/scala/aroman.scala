package org.expr.mcdm

import org.expr.mcdm.Direction._

case class AromanResult(
    weights: Vec,
    linearNormalizedMatrix: Mat,
    vectorNormalizedMatrix: Mat,
    aggregatedNormalizedMatrix: Mat,
    weightedNormalizedMatrix: Mat,
    minimumSums: Vec,
    maximumSums: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

// Higher scores are better.
def aroman(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    beta: Double = 0.5,
    lambda: Double = 0.5,
    options: Map[String, Any] = Map.empty
): AromanResult =

    val (n, m) = Matrix.size(decmat)

    require(weights.length == m, "weights must match the number of criteria")
    require(directions.length == m, "directions must match the number of criteria")
    require(beta >= 0.0 && beta <= 1.0, "beta must be between 0 and 1")
    require(lambda >= 0.0 && lambda <= 1.0, "lambda must be between 0 and 1")
    require(weights.sum != 0.0, "weights must not sum to zero")

    val w = weights.map(_ / weights.sum)

    val colmins = Matrix.colmins(decmat)
    val colmaxs = Matrix.colmaxs(decmat)
    val norms = Matrix.applyFunctionToColumns(decmat, Matrix.norm)

    for j <- 0 until m do
      require(colmaxs(j) != colmins(j), s"criterion $j must not have identical values")
      require(norms(j) != 0.0, s"criterion $j must not have a zero vector norm")

    val linearNormalizedMatrix =
      Array.tabulate(n, m)((i, j) => (decmat(i)(j) - colmins(j)) / (colmaxs(j) - colmins(j)))

    val vectorNormalizedMatrix =
      Array.tabulate(n, m)((i, j) => decmat(i)(j) / norms(j))

    val aggregatedNormalizedMatrix = Array.tabulate(n, m)((i, j) =>
      (beta * linearNormalizedMatrix(i)(j) + (1.0 - beta) * vectorNormalizedMatrix(i)(j)) / 2.0
    )

    val weightedNormalizedMatrix = Matrix.weightizeColumns(aggregatedNormalizedMatrix, w)

    def sumsFor(dir: Direction): Vec =
      weightedNormalizedMatrix.map(row => (0 until m).filter(directions(_) == dir).map(row(_)).sum)

    val minimumSums = sumsFor(Minimize)
    val maximumSums = sumsFor(Maximize)

    val scores = minimumSums.zip(maximumSums).map((mn, mx) =>
      math.pow(mn, lambda) + math.pow(mx, 1.0 - lambda)
    )

    val ranks = ranksfromscores(scores)

    AromanResult(
      w,
      linearNormalizedMatrix,
      vectorNormalizedMatrix,
      aggregatedNormalizedMatrix,
      weightedNormalizedMatrix,
      minimumSums,
      maximumSums,
      scores,
      ranks
    )
