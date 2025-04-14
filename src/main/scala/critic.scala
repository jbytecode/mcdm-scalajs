package org.expr.mcdm

import org.expr.mcdm.Vec 
import org.expr.mcdm.Mat

case class CriticResult(
    weights: Vec,
    corMat: Mat,
    scores: Vec
) extends MCDMResult

def critic(
    mat: Mat, 
    directions: Array[Direction], 
    normalization: NormalizationFunction = Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty
    ): CriticResult = 

        val (n, m) = Matrix.size(mat)

        val normalizedMat = normalization(mat, Array.emptyDoubleArray, directions)

        val onesMat = Matrix.ones(m, m)

        val correlations = Statistics.correlation(normalizedMat)

        val corMat = Matrix.subtract(onesMat, correlations)

        val stds = Matrix.applyFunctionToColumns(normalizedMat, Statistics.std)
    
        val scores = Array.range(0, m).map(i => Matrix.getcolat(corMat, i).sum * stds(i))

        val w = scores.map(_ / scores.sum)

        CriticResult(
            w,
            corMat,
            scores)


