package org.expr.mcdm

import org.expr.mcdm.Direction._

case class SpotisResult(
    normalizedMatrix: Mat,
    weightedNormalizedMatrix: Mat,
    weights: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

// Lower scores are better. Bounds default to column minimums/maximums.
def spotis(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    lowerbounds: Option[Vec] = None,
    upperbounds: Option[Vec] = None,
    options: Map[String, Any] = Map.empty
): SpotisResult =

    val (n, m) = Matrix.size(decmat)

    require(weights.length == m, "Length of weights must be equal to number of criteria")
    require(directions.length == m, "Length of directions must be equal to number of criteria")

    val w = weights.map(_ / weights.sum)

    val lower = lowerbounds.getOrElse(Matrix.colmins(decmat))
    val upper = upperbounds.getOrElse(Matrix.colmaxs(decmat))

    require(
      lower.length == m && upper.length == m,
      "Length of lowerbounds/upperbounds must be equal to number of criteria"
    )
    require(
      lower.zip(upper).forall((l, u) => u >= l),
      "upperbounds must be greater than or equal to lowerbounds"
    )

    val normalizedMatrix = Array.tabulate(n, m) { (i, j) =>
      val ideal = directions(j) match
        case Minimize => lower(j)
        case Maximize => upper(j)
      val span = upper(j) - lower(j)
      if span == 0.0 then 0.0 else math.abs(decmat(i)(j) - ideal) / span
    }

    val weightedNormalizedMatrix = Matrix.weightizeColumns(normalizedMatrix, w)

    val scores = Matrix.rowsums(weightedNormalizedMatrix)

    val ranks = ranksfromscores(scores, false)

    SpotisResult(normalizedMatrix, weightedNormalizedMatrix, w, scores, ranks)
