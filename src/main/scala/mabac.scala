package org.expr.mcdm

case class MabacResult(
    normalizedDecisionMatrix: Mat,
    weightedDecisionMatrix: Mat,
    geometricMeans: Vec,
    Q: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def mabac(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty 
): MabacResult =

    val (row, col) = Matrix.size(decmat)

    val colMax = Matrix.colmaxs(decmat)

    val colMin = Matrix.colmins(decmat)

    val A = normalization(decmat, weights, directions)

    // wA = Utilities.weightise((A .+ one(zerotype)), w)
    val wA = Matrix.weightizeColumns(A.map(row => row.map(_ + 1.0)), weights)

    // g[i] = geomean(wA[:, i])
    val g = Matrix.applyFunctionToColumns(wA, Statistics.geomean)

    // Q = wA .- g'
    val Q = wA.map(row => row.zip(g).map { case (x, y) => x - y })


    // scores[i] = sum(Q[i, :])
    val scores = Q.map(row => row.sum)

    // Ranks 
    val ranks = ranksfromscores(scores)

    MabacResult(
        normalizedDecisionMatrix = A,
        weightedDecisionMatrix = wA,
        geometricMeans = g,
        Q = Q,
        scores = scores,
        ranks = ranks
    )

