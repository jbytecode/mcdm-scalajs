package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class MoosraResult(
    normalizedDecisionMat: Mat,
    weightedNormalizedMatrix: Mat,
    scores: Vec,
    ranks: Vec,
) extends MCDMResult

def emptyMoosraResult(decmat: Mat): MoosraResult =
    val (rows, cols) = Matrix.size(decmat)
    MoosraResult(
        normalizedDecisionMat = Matrix.zeros(rows, cols),
        weightedNormalizedMatrix = Matrix.zeros(rows, cols),
        scores = Matrix.zeros(rows),
        ranks = Matrix.zeros(rows),
    )

def moosra(
    decisionMat: Mat,
    weights: Vec = Array.emptyDoubleArray,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByAllNormNormalization,
    options: Map[String, Any] = Map.empty
): Either[String, MoosraResult] =

    // mincounts = count(x -> x == minimum, fns)
    val minCounts = directions.count(_ == Minimize)

    if minCounts < 1 then
        return Left("At least one criterion must be minimized in Moosra Method")

    val (row, col) = Matrix.size(decisionMat)

    val normalizedDecisionMat = normalization(decisionMat, weights, directions)

    val weightedNormalizedMatrix = Matrix.weightizeColumns(normalizedDecisionMat, weights)

    val scores = Matrix.zeros(row)

    /* 
        for i = 1:row
        positive = 0.0
        negative = 0.0
        for j = 1:col
            if fns[j] == maximum
                positive += weightedNormalizedMatrix[i, j]
            elseif fns[j] == minimum
                negative += weightedNormalizedMatrix[i, j]
            else
                error("fns[i] is not a proper function (direction of optimization)")
            end
        end
        scores[i] = positive / negative
    end
     */
    for i <- 0 until row do
        var positive = 0.0
        var negative = 0.0
        for j <- 0 until col do
            if directions(j) == Maximize then
                positive += weightedNormalizedMatrix(i)(j)
            else negative += weightedNormalizedMatrix(i)(j)
        scores(i) = positive / negative

    val ranks = ranksfromscores(scores)

    Right(
        MoosraResult(
            normalizedDecisionMat = normalizedDecisionMat,
            weightedNormalizedMatrix = weightedNormalizedMatrix,
            scores = scores,
            ranks = ranks
        )
    )


    

