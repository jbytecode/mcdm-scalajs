package org.expr.mcdm

import org.expr.mcdm.Direction._

case class ErvdResult(
    normalizedMatrix: Mat,
    normalizedReferencePoints: Vec,
    valueMatrix: Mat,
    idealDesired: Vec,
    idealUndesired: Vec,
    distanceToIdeal: Vec,
    distanceToNegative: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

// Reference points default to the column means when omitted.
def ervd(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    referencePoints: Option[Vec] = None,
    lambda: Double = 2.25,
    alpha: Double = 0.88,
    normalization: NormalizationFunction =
      Normalization.DivideByColumnsSumNormalization,
    options: Map[String, Any] = Map.empty
): ErvdResult =

    val (n, m) = Matrix.size(decmat)

    require(weights.length == m, "Length of weights must be equal to number of criteria")
    require(directions.length == m, "Length of directions must be equal to number of criteria")
    require(lambda > 0.0, "lambda must be greater than zero")
    require(alpha > 0.0, "alpha must be greater than zero")

    val colsums = Matrix.colsums(decmat)
    val refs = referencePoints.getOrElse(colsums.map(_ / n))
    require(refs.length == m, "Length of referencePoints must be equal to number of criteria")

    val w = weights.map(_ / weights.sum)

    val normalizedMatrix = normalization(decmat, weights, directions)

    val normalizedReferencePoints = refs.zip(colsums).map(_ / _)

    val valueMatrix = Array.tabulate(n, m) { (i, j) =>
      val d = normalizedMatrix(i)(j) - normalizedReferencePoints(j)
      directions(j) match
        case Maximize =>
          if d > 0 then math.pow(d, alpha) else -lambda * math.pow(-d, alpha)
        case Minimize =>
          if d < 0 then math.pow(-d, alpha) else -lambda * math.pow(d, alpha)
    }

    val idealDesired = Matrix.colmaxs(valueMatrix)
    val idealUndesired = Matrix.colmins(valueMatrix)

    def weightedDistance(row: Vec, target: Vec): Double =
      (0 until m).map(j => w(j) * math.abs(row(j) - target(j))).sum

    val distanceToIdeal = valueMatrix.map(weightedDistance(_, idealDesired))
    val distanceToNegative = valueMatrix.map(weightedDistance(_, idealUndesired))

    val scores = distanceToNegative.zip(distanceToIdeal).map((dn, di) => dn / (di + dn))

    val ranks = ranksfromscores(scores)

    ErvdResult(
      normalizedMatrix,
      normalizedReferencePoints,
      valueMatrix,
      idealDesired,
      idealUndesired,
      distanceToIdeal,
      distanceToNegative,
      scores,
      ranks
    )
