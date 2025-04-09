package org.expr.mcdm

import org.expr.mcdm.MCDMResult
import org.expr.mcdm.Direction._
import org.expr.mcdm.Matrix
import org.expr.mcdm.Vec
import org.expr.mcdm.Mat
import org.expr.mcdm.Statistics

case class TopsisResult(
    normalizedMatrix: Mat,
    weightedNormalizedMatrix: Mat,
    ideal: Vec,
    antiIdeal: Vec,
    distanceToIdeal: Vec,
    distanceToAntiIdeal: Vec,
    scores: Vec,
    rankings: VecInt,
    bestIndex: Int
) extends MCDMResult

def topsis(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction = Normalization.VectorNormNormalization,
    options: Map[String, Any] = Map.empty
): TopsisResult =
    
    val n, m = Matrix.size(decmat)
    
    val normalizedMatrix = normalization(decmat, weights, directions)
    
    val weightedNormalizedMatrix = Matrix.weightizeColumns(normalizedMatrix, weights)

    val ideal = Matrix.colminmax(weightedNormalizedMatrix, directions)
    
    val antiIdeal = Matrix.colminmax(weightedNormalizedMatrix, Matrix.inversedirections(directions))
    
    val distanceToIdeal = 
      Matrix.applyFunctionToRows(weightedNormalizedMatrix, (row: Vec) =>
        Statistics.euclideanDistance(row, ideal))
    
    val distanceToAntiIdeal =
      Matrix.applyFunctionToRows(weightedNormalizedMatrix, (row: Vec) =>
        Statistics.euclideanDistance(row, antiIdeal))

    val scores = distanceToAntiIdeal.zip(distanceToIdeal).map {
      case (danti, di) => danti / (danti + di)
    }

    val rankings = scores.zipWithIndex.sortBy(-_._1).map(_._2)

    val bestIndex = rankings.head
    
    TopsisResult(
      normalizedMatrix,
      weightedNormalizedMatrix,
      ideal,
      antiIdeal,
      distanceToIdeal,
      distanceToAntiIdeal,
      scores,
      rankings,
      bestIndex
    )
    
