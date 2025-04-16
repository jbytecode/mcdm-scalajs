package org.expr.mcdm

case class CoprasResult(
    normalizedMat: Mat,
    weightedNormalizedMat: Mat,
    splus: Vec,
    sminus: Vec,
    Q: Vec,
    Z: Double,
    maxQ: Double,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def copras(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnsSumNormalization,
    options: Map[String, Any] = Map.empty 
): CoprasResult = 

    val (nrows, ncols) = Matrix.size(decmat)

    val normalizedMat = normalization(decmat, weights, directions)

    val weightedNormalizedMat = Matrix.weightizeColumns(normalizedMat, weights)

    var sPlus = Matrix.zeros(nrows)
    
    var sMinus = Matrix.zeros(nrows)

    for row <- 0 until nrows do
        for col <- 0 until ncols do
            if directions(col) == Direction.Maximize then
                sPlus(row) = sPlus(row) + weightedNormalizedMat(row)(col)
            else if directions(col) == Direction.Minimize then
                sMinus(row) = sMinus(row) + weightedNormalizedMat(row)(col)


    var Z = sMinus.map(x => 1.0 / x).sum

    val Q = Array.range(0, nrows).map(i => sPlus(i) + (sMinus.sum / (sMinus(i) * Z)))

    val maxQ = Q.max

    val scores = Q.map(x => x / maxQ)

    val ranks = ranksfromscores(scores)

    CoprasResult(
        normalizedMat,
        weightedNormalizedMat,
        sPlus,
        sMinus,
        Q,
        Z,
        maxQ,
        scores,
        ranks
    )