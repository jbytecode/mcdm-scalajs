package org.expr.mcdm

import org.expr.mcdm._


case class SawResult(
    normalizedDecisionMat: Mat,
    weightedNormalizedDecMat: Mat,
    scores: Vec,
    orderings: VecInt,
    bestIndex: Int
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

        val orderings = scores.zipWithIndex.sortBy(_._1).map(_._2)

        val bestIndex = orderings.last
        
        SawResult(
            normalizedDecisionMat,
            weightedNormalizedDecMat,
            scores,
            orderings,
            bestIndex
        )