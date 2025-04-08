package org.expr.mcdm

import org.expr.mcdm.Direction.{Minimize, Maximize}

case class PivResult(
    scores: Vec
) extends MCDMResult

def piv(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.VectorNormNormalization,
    options: Map[String, Any] = Map.empty 
): PivResult = 

    val (nrow, ncol) = Matrix.size(decmat)

    val normalized_dec_mat = normalization(decmat, weights, directions)

    val weighted_norm_mat = Matrix.weightizeColumns(normalized_dec_mat, weights)

    val dirfunctions = directions.map {
      case Maximize => Statistics.maximum
      case Minimize => Statistics.minimum
    }
    val desiredvalues = Matrix.applyFunctionsToColumns(weighted_norm_mat, dirfunctions)

    var finalmat = Matrix.zeros(nrow, ncol)

    for i <- 0 until nrow do
      for j <- 0 until ncol do
        if directions(j) == Maximize then
          finalmat(i)(j) = desiredvalues(j) - weighted_norm_mat(i)(j)
        else if directions(j) == Minimize then
          finalmat(i)(j) = weighted_norm_mat(i)(j) - desiredvalues(j)

    // di values are scores
    val di = Matrix.rowsums(finalmat)

    PivResult(
        di
    )