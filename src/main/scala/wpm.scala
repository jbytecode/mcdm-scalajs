package org.expr.mcdm

case class WpmResult(
    normalizedDecisionMatrix: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def wpm(
    mat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
): WpmResult =

  val (n, p) = Matrix.size(mat)

  val normalizedDecisionMat = normalization(mat, weights, directions)

  val scoreMat = Array.tabulate(n, p)((i, j) => math.pow(normalizedDecisionMat(i)(j), weights(j)))

  val scores = scoreMat.map(_.product)

  val ranks = ranksfromscores(scores)

  WpmResult(
    normalizedDecisionMat,
    scores,
    ranks
  )
