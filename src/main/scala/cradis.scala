package org.expr.mcdm

import org.expr.mcdm.Direction._

case class CradisResult(
    normalizedMatrix: Mat,
    weightedNormalizedMatrix: Mat,
    tIdeal: Double,
    tAntiIdeal: Double,
    idealAlternative: Vec,
    antiIdealAlternative: Vec,
    splus: Vec,
    sminus: Vec,
    splus0: Double,
    sminus0: Double,
    kplus: Vec,
    kminus: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def cradis(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
): CradisResult =

    val normalizedMatrix = normalization(decmat, weights, directions)

    val weightedNormalizedMatrix =
      Matrix.weightizeColumns(normalizedMatrix, weights)

    val tIdeal = weightedNormalizedMatrix.map(_.max).max
    val tAntiIdeal = weightedNormalizedMatrix.map(_.min).min

    val idealAlternative = Matrix.colmaxs(weightedNormalizedMatrix)
    val antiIdealAlternative = Matrix.colmins(weightedNormalizedMatrix)

    val splus = weightedNormalizedMatrix.map(row => row.map(tIdeal - _).sum)
    val sminus = weightedNormalizedMatrix.map(row => row.map(_ - tAntiIdeal).sum)

    val splus0 = idealAlternative.map(tIdeal - _).sum
    val sminus0 = idealAlternative.map(_ - tAntiIdeal).sum

    val kplus = splus.map(splus0 / _)
    val kminus = sminus.map(_ / sminus0)

    val scores = kplus.zip(kminus).map((kp, km) => (kp + km) / 2.0)

    val ranks = ranksfromscores(scores)

    CradisResult(
      normalizedMatrix,
      weightedNormalizedMatrix,
      tIdeal,
      tAntiIdeal,
      idealAlternative,
      antiIdealAlternative,
      splus,
      sminus,
      splus0,
      sminus0,
      kplus,
      kminus,
      scores,
      ranks
    )
