package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class RovResult(
    normalizedMat: Mat,
    uplus: Vec,
    uminus: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult


def rov(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty 
): RovResult = 

    val (n, p) = Matrix.size(decmat)

    val normalizedMat = normalization(decmat, weights, directions)

    val uplus = Matrix.zeros(n)

    val uminus = Matrix.zeros(n)

    val u = Matrix.zeros(n)

    val maxindices = directions.zipWithIndex.filter(_._1 == Maximize).map(_._2)

    val minindices = directions.zipWithIndex.filter(_._1 == Minimize).map(_._2)

    if maxindices.nonEmpty then
        for i <- 0 until n do 
            uplus(i) = 
                Matrix.sumproduct(
                    Matrix.elementsat(Matrix.getrowat(normalizedMat, i), maxindices),
                    Matrix.elementsat(weights, maxindices))

    if minindices.nonEmpty then
        for i <- 0 until n do 
            uminus(i) = 
                Matrix.sumproduct(
                    Matrix.elementsat(Matrix.getrowat(normalizedMat, i), minindices),
                    Matrix.elementsat(weights, minindices))


    // U values are scores
    for i <- 0 until n do 
        u(i) = (uminus(i) + uplus(i)) / 2.0

    val scores = u

    val ranks = ranksfromscores(scores)

         
    RovResult(
        normalizedMat,
        uplus,
        uminus,
        scores = u,
        ranks = ranks
    )