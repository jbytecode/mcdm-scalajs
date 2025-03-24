package org.expr.mcdm

case class WpmResult(
    normalizedDecisionMatrix: Mat,
    scores: Vec,
    ordering: VecInt,
    bestIndex: Int
) extends MCDMResult

def wpm(
    mat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
): WpmResult =

  val (n, p) = Matrix.size(mat)

  val normalizedDecisionMat = normalization(mat, weights, directions)

  /*
  var scoreMat = Array.tabulate(n, p)((i, j) => 0.0)
  for (i <- 0 until p) {
    for (j <- 0 until n) {
      scoreMat(j)(i) = math.pow(normalizedDecisionMat(j)(i), weights(i))
    }
  }
  */  
  val scoreMat = Array.tabulate(n, p)((i, j) => math.pow(normalizedDecisionMat(i)(j), weights(j)))

  val scores = scoreMat.map(_.product)

  val orderings = scores.zipWithIndex.sortBy(_._1).map(_._2)

  val bestIndex = orderings.last

  WpmResult(
    normalizedDecisionMat,
    scores,
    orderings,
    bestIndex
  )
