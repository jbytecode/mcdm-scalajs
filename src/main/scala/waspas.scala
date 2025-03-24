package org.expr.mcdm

import org.expr.mcdm.Normalization.DivideByColumnMaxMinNormalization

case class WaspasResult(
    normalizedDecisionMatrix: Mat,
    scoresWSM: Vec,
    scoresWPM: Vec,
    scores: Vec,
    bestIndex: Int,
    orderings: VecInt 
) extends MCDMResult

val defaultWaspasOptions: Map[String, Any] = Map(
  "lambda" -> 0.5
)

def waspas(
    decisionMat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction = DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = defaultWaspasOptions
): WaspasResult =

  val (row, col) = Matrix.size(decisionMat)

  val lambda = options("lambda").asInstanceOf[Double]

  val normalizedDecisionMat = normalization(decisionMat, weights, directions)

  val scoreMat = Array.tabulate(row, col)((i, j) =>
    math.pow(normalizedDecisionMat(i)(j), weights(j))
  )

  val scoresWPM = scoreMat.map(_.product)

  val scoresWSM =
    Matrix.rowsums(Matrix.weightizeColumns(normalizedDecisionMat, weights))

  var scoreTables = Matrix.zeros(row, 2)
  for (i <- 0 until row) {
    scoreTables(i)(0) = scoresWSM(i)
    scoreTables(i)(1) = scoresWPM(i)
  }

  val l = Array(lambda, 1.0 - lambda)

  val scores = Matrix.rowsums(Matrix.mul(scoreTables, Matrix.makeRowMatrix(l)))

  val bestIndex = scores.zipWithIndex.maxBy(_._1)._2

  val orderings = scores.zipWithIndex.sortBy(_._1).map(_._2)

  WaspasResult(normalizedDecisionMat, scoresWSM, scoresWPM, scores, bestIndex, orderings)
