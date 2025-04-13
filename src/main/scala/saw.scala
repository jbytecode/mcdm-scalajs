package org.expr.mcdm

import org.expr.mcdm._


case class SawResult(
    normalizedDecisionMat: Mat,
    weightedNormalizedDecMat: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def saw(
    mat: Mat, 
    weights: Vec,
    directions: Array[Direction], 
    normalization: NormalizationFunction = Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
    ): SawResult = 
        
        val (n, p) = Matrix.size(mat)

        val normalizedDecisionMat = normalization(mat, weights, directions)

        val weightedNormalizedDecMat = Matrix.weightizeColumns(normalizedDecisionMat, weights)

        val scores = Matrix.rowsums(weightedNormalizedDecMat)

        val ranks = ranksfromscores(scores)
        
        SawResult(
            normalizedDecisionMat,
            weightedNormalizedDecMat,
            scores,
            ranks
        )