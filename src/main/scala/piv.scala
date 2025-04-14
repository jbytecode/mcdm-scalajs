package org.expr.mcdm

import org.expr.mcdm.Direction.{Minimize, Maximize}

case class PivResult(
    scores: Vec,
    ranks: Vec
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

    val finalmat = Array.tabulate(nrow, ncol) { (i, j) =>
      directions(j) match
        case Maximize => desiredvalues(j) - weighted_norm_mat(i)(j)
        case Minimize => weighted_norm_mat(i)(j) - desiredvalues(j)
      }
    
    // di values are scores
    val di = Matrix.rowsums(finalmat)

    // ranks from scores
    // Ranks are calculated from the scores
    // by default, they are in descending order
    // The smallest score gets rank 1
    val ranks = ranksfromscores(di, reverse = false)

    PivResult(
        di,
        ranks
    )