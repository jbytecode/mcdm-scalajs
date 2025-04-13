package org.expr.mcdm

import scala.math.{pow}

case class CocosoResult(
    normalizedMat: Mat,
    scoreMat: Mat,
    P: Vec,
    S: Vec,
    kA: Vec,
    kB: Vec,
    kC: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult


def cocoso(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map("lambda" -> 0.5) 
): CocosoResult = 

    val (row, col) = Matrix.size(decmat)

    val A = normalization(decmat, weights, directions)

    var scoreMat = Matrix.zeros(row, col)

    for i <- 0 until col do
        scoreMat = Matrix.setcolat(scoreMat, i, Matrix.getcolat(A, i).map(x => pow(x, weights(i))))

    val P = Matrix.zeros(row)

    for i <- 0 until row do
        P(i) = Matrix.getrowat(scoreMat, i).sum
    
    val S = Matrix.weightizeColumns(A, weights).map(row => row.sum)

    val scoreTable = Matrix.zeros(row, 2)
    for i <- 0 until row do
        scoreTable(i) = Array(S(i), P(i))


    val kA = S.zip(P).map((x, y) => x + y).map(x => x / scoreTable.flatten.sum)

    val kB = (S.map(x => x / Statistics.minimum(S))
        .zip(P.map(x => x / Statistics.minimum(P)))
        .map((x, y) => x + y))


    val lambda = options.getOrElse("lambda", 0.5).asInstanceOf[Double]

    val invlambda = 1 - lambda 

    val kC = (S.map(x => lambda * x)
        .zip(P.map(x => invlambda * x))
        .map((x, y) => x + y)
        .zip(S.map(x => lambda * Statistics.maximum(S))
            .zip(P.map(x => invlambda * Statistics.maximum(P)))
            .map((x, y) => x + y))
        .map((x, y) => x / y))


    val kApluskBpluskC = kA.zip(kB).zip(kC).map((x, y) => x._1 + x._2 + y)

    val kApluskBpluskCdiv3 = kApluskBpluskC.map(x => x / 3)

    val kAprod = kA.zip(kB).zip(kC).map((x, y) => x._1 * x._2 * y)

    val kAprod3 = kAprod.map(x => math.pow(x, 1.0 / 3.0))

    val scores = kApluskBpluskCdiv3.zip(kAprod3).map((x, y) => x + y)

    val ranks = ranksfromscores(scores)
    
    CocosoResult(
        normalizedMat = A,
        scoreMat = scoreMat,
        P = P,
        S = S,
        kA = kA,
        kB = kB,
        kC = kC,
        scores = scores,
        ranks
    )

