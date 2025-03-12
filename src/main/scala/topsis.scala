package org.expr.mcdm

import org.expr.mcdm.MCDMResult
import org.expr.mcdm.Direction._
import org.expr.mcdm.Matrix
import org.expr.mcdm.Vec
import org.expr.mcdm.Mat
import org.expr.mcdm.Statistics

case class TopsisResult(
    val normalizedMatrix: Mat,
    val weightedNormalizedMatrix: Mat,
    val ideal: Vec,
    val antiIdeal: Vec,
    val distanceToIdeal: Vec,
    val distanceToAntiIdeal: Vec,
    val scores: Vec,
    val rankings: VecInt,
    val bestIndex: Int
) extends MCDMResult

def topsis(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction = Normalization.VectorNormNormalization
): TopsisResult =
  val n, m = Matrix.size(decmat)
    val normalizedMatrix = normalization(decmat, weights, directions)
    val weightedNormalizedMatrix = Matrix.weightizeColumns(normalizedMatrix, weights)
    val colmaxs = Matrix.colmaxs(weightedNormalizedMatrix)
    val colmins = Matrix.colmins(weightedNormalizedMatrix)
    val ideal = directions.zip(colmaxs.zip(colmins)).map {
      case (Maximize, (max, _)) => max
      case (Minimize, (_, min)) => min
    }
    val antiIdeal = directions.zip(colmaxs.zip(colmins)).map {
      case (Maximize, (_, min)) => min
      case (Minimize, (max, _)) => max
    }
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
    
