package org.expr.mcdm

import scala.math.log

case class LmawResult (
    normalizedDecisionMatrix: Mat,
    Q: Mat,
    N: Mat,
    a: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def lmaw(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty 
): LmawResult = 

    def prod(a: Vec): Double = a.product

    val (row, col) = Matrix.size(decmat)

    val colMax = Matrix.colmaxs(decmat)

    val colMin = Matrix.colmins(decmat)

    val A = normalization(decmat, weights, directions).map(row => row.map(_ + 1.0))

    var Q = Matrix.zeros(row, col)

    var N = A.map(row => row.map(log))

    val a = Matrix.applyFunctionToColumns(A, prod).map(element => log(element))

    for j <- 0 until col do
        N = Matrix.setcolat(N, j, Matrix.getcolat(N, j).map(_ / a(j)))
        Q = Matrix.setcolat(Q, j,
          Matrix.getcolat(N, j).map(x => (2.0 * Math.pow(x, weights(j))) /
            (Math.pow(2.0 - x, weights(j)) + Math.pow(x, weights(j))))
        )


    val scores = Q.map(row => row.sum)

    val ranks = ranksfromscores(scores)

    LmawResult(
        A,
        Q,
        N,
        a,
        scores,
        ranks
    )