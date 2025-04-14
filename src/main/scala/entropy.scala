package org.expr.mcdm

import scala.math.log

case class EntropyResult(
    weights: Vec
) extends MCDMResult

def entropy(
    decisionMat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty
): EntropyResult =

    val (row, col) = Matrix.size(decisionMat)

    val colsumnormmat = Matrix.colsums(decisionMat)

    val normalizedDM = Array.tabulate(row, col)((j, i) => decisionMat(j)(i) / colsumnormmat(i))

    val logMat = Array.tabulate(row, col)((i, j) => normalizedDM(i)(j) * log(normalizedDM(i)(j)))

    val e = Array.range(0, col).map(i => 1.0 - Matrix.getcolat(logMat, i).sum / -log(row))

    val esum = e.filter(x => !x.isNaN).sum

    val w = e.map(x => x / esum)

    EntropyResult(
        weights = w
    )