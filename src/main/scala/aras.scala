package org.expr.mcdm

import org.expr.mcdm.Direction.{Minimize, Maximize}

case class ArasResult(
    referenceRow: Vec,
    extendMat: Mat,
    normalizedMat: Mat,
    optimality_degrees: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def aras(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
    options: Map[String, Any] = Map.empty
): ArasResult =
  val (nrows, ncols) = Matrix.size(decmat)

  val referenceRow = Matrix.colminmax(decmat, directions)

  val extendMat = Matrix
    .appendrow(decmat, referenceRow)
    .transpose
    .zip(directions)
    .map((col, dir) =>
      dir match
        case Minimize => col.map(value => 1.0 / value)
        case Maximize => col
    )
    .transpose

  val normalized = normalization(extendMat, weights, directions)

  val optimalityDegrees = Array
    .range(0, nrows + 1)
    .map(i =>
      weights.zip(Matrix.getrowat(normalized, i)).map((w, x) => w * x).sum
    )

  val utilityDegrees = Array
    .range(0, nrows)
    .map(i => optimalityDegrees(i) / optimalityDegrees(nrows))

  val ranks = ranksfromscores(utilityDegrees)

  ArasResult(
    referenceRow,
    extendMat,
    normalized,
    optimalityDegrees,
    utilityDegrees,
    ranks
  )
