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

    val normalizedDM = Matrix.similar(decisionMat)


    val colsumnormmat = Matrix.colsums(decisionMat)

    for i <- 0 until col do
        for j <- 0 until row do
            normalizedDM(j)(i) = decisionMat(j)(i) / colsumnormmat(i)

    val logMat = Matrix.zeros(row, col)

    for i <- 0 until row do
        for j <- 0 until col do
            logMat(i)(j) = normalizedDM(i)(j) * log(normalizedDM(i)(j))

    val e = Matrix.zeros(col)

    for i <- 0 until col do
        e(i) = 1.0 - Matrix.getcolat(logMat, i).sum / -log(row)

    val esum = e.filter(x => !x.isNaN).sum

    val w = e.map(x => x / esum)

    EntropyResult(
        weights = w
    )