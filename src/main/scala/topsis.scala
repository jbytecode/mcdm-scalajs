package org.expr.mcdm

import org.expr.mcdm.MCDMResult
import org.expr.mcdm.Direction
import org.expr.mcdm.Matrix
import org.expr.mcdm.Vec
import org.expr.mcdm.Mat
import org.expr.mcdm.Statistics

case class TopsisResult(
    val normalizedMatrix: Array[Array[Double]],
    val weightedNormalizedMatrix: Array[Array[Double]],
    val ideal: Array[Double],
    val antiIdeal: Array[Double],
    val distanceToIdeal: Array[Double],
    val distanceToAntiIdeal: Array[Double],
    val scores: Array[Double],
    val rankings: Array[Int],
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
      case (Direction.Maximize, (max, _)) => max
      case (Direction.Minimize, (_, min)) => min
    }
    val antiIdeal = directions.zip(colmaxs.zip(colmins)).map {
      case (Direction.Maximize, (_, min)) => min
      case (Direction.Minimize, (max, _)) => max
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
    
