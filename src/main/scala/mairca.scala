package org.expr.mcdm



case class MaircaResult(
    T: Mat,
    A: Mat,
    S: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def mairca(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty 
): MaircaResult = 

    val (row, col) = Matrix.size(decmat)

    var T = Matrix.zeros(row, col)

    for (i <- 0 until col) {
        for (j <- 0 until row) {
            T(j)(i) = weights(i) * (1.0 / row)
        }
    }

    val A = Matrix.elementwiseMultiply(normalization(decmat, weights, directions), T)

    val S = Matrix.subtract(T, A)

    val scores = S.map(row => row.sum)


    // In Mairca, the ranks are calculated based on the sorted order of the scores
    // smallest score is the best
    val ranks = ranksfromscores(scores, reverse = false)

    MaircaResult(
        T = T,
        A = A,
        S = S,
        scores = scores,
        ranks = ranks
    )