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

    val scoreMat = Array.tabulate(row, col)( (i, j) => pow(A(i)(j), weights(j)) )

    val P = Matrix.rowsums(scoreMat)
    
    val S = Matrix.weightizeColumns(A, weights).map(row => row.sum)

    val scoreTable = Array.tabulate(row, 2)( (i, j) => if j == 0 then S(i) else P(i) )

    val kA = S.zip(P).map((x, y) => x + y).map(x => x / scoreTable.flatten.sum)

    val kB = (S.map(x => x / S.min)
        .zip(P.map(x => x / P.min))
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

