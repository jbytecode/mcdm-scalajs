package org.expr.mcdm



case class MaircaResult(
    T: Mat,
    A: Mat,
    S: Mat,
    scores: Vec,
    orderings: VecInt,
    bestIndex: Int,
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

    val orderings = scores.zipWithIndex.sortBy(-_._1).map(_._2)

    val bestIndex = orderings.last

    MaircaResult(
        T = T,
        A = A,
        S = S,
        scores = scores,
        orderings = orderings,
        bestIndex = bestIndex,
    )