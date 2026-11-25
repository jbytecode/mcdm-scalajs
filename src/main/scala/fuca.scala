package org.expr.mcdm

import org.expr.mcdm.Direction._

case class FucaResult(
    weights: Vec,
    rankMatrix: Mat,
    weightedRankMatrix: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

// Ranks values within a criterion (1 = best); ties get the average rank.
private def fucaRankColumn(values: Vec, direction: Direction): Vec =
  values.map { v =>
    val better = values.count(t => if direction == Maximize then t > v else t < v)
    val equal = values.count(_ == v)
    better + (equal + 1) / 2.0
  }

// Lower scores are better.
def fuca(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    options: Map[String, Any] = Map.empty
): FucaResult =

    val (n, m) = Matrix.size(decmat)

    require(weights.length == m, "Length of weights must be equal to number of criteria")
    require(directions.length == m, "Length of directions must be equal to number of criteria")
    require(weights.forall(_ >= 0.0) && weights.sum > 0.0, "weights must be nonnegative with a positive sum")

    val w = weights.map(_ / weights.sum)

    val rankMatrix =
      Array.tabulate(m)(j => fucaRankColumn(Matrix.getcolat(decmat, j), directions(j))).transpose

    val weightedRankMatrix = Matrix.weightizeColumns(rankMatrix, w)

    val scores = Matrix.rowsums(weightedRankMatrix)

    val ranks = ranksfromscores(scores, false)

    FucaResult(w, rankMatrix, weightedRankMatrix, scores, ranks)
