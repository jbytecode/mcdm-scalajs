package org.expr.mcdm

case class CoprasResult(
    normalizedMat: Mat,
    weightedNormalizedMat: Mat,
    splus: Vec,
    sminus: Vec,
    Q: Vec,
    Z: Double,
    maxQ: Double,
    rankings: VecInt,
    bestIndex: Int,
    scores: Vec,
) extends MCDMResult

def copras(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
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
                sPlus(row) = sPlus(row) + Matrix.elementat(weightedNormalizedMat, row, col)
            else if directions(col) == Direction.Minimize then
                sMinus(row) = sMinus(row) + Matrix.elementat(weightedNormalizedMat, row, col)


    var Q = Matrix.zeros(nrows)
    var Z = sMinus.map(x => 1.0 / x).sum

    for row <- 0 until nrows do
        Q(row) = sPlus(row) + (sMinus.sum / (sMinus(row) * Z))

    val maxQ = Q.max
    val scores = Q.map(x => x / maxQ)

    val rankings = scores.zipWithIndex.sortBy(-_._1).map(_._2)
    val bestIndex = rankings.head

    CoprasResult(
        normalizedMat,
        weightedNormalizedMat,
        sPlus,
        sMinus,
        Q,
        Z,
        maxQ,
        rankings,
        bestIndex,
        scores
    )