package org.expr.mcdm

case class GiniResult(
    weights: Vec,
    coefficients: Vec
) extends MCDMResult

def gini(
    decisionMat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty
): GiniResult =

    val (n, m) = Matrix.size(decisionMat)

    val coefficients = Array.range(0, m).map { j =>
        val values = Matrix.getcolat(decisionMat, j)
        val pairwise = (for i <- 0 until n; k <- 0 until n yield math.abs(values(i) - values(k))).sum
        pairwise / (2.0 * n * n * (values.sum / n))
    }

    val w = coefficients.map(_ / coefficients.sum)

    GiniResult(w, coefficients)
