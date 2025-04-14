package org.expr.mcdm

import scala.math.{sqrt, pow, abs}

case class CodasResult(
    normalizedMatrix: Mat,
    E: Mat,
    Euc: Vec,
    T: Mat,
    Tax: Vec,
    EA: Mat,
    TA: Mat,
    RA: Mat,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def codas(
    decmat: Mat, 
    weights: Vec,
    directions: Array[Direction], 
    normalization: NormalizationFunction = Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map("tau" -> 0.02)
    ): CodasResult = 

    val (nrows, ncols) = Matrix.size(decmat)

    val A = normalization(decmat, weights, directions)

    val wA = Array.tabulate(nrows, ncols)( (i, j) => A(i)(j) * weights(j))
    
    val wAmin = Matrix.colmins(wA)

    val E = Array.tabulate(nrows, ncols)( (i, j) => pow(wA(i)(j) - wAmin(j), 2))

    val Euc = Matrix.rowsums(E).map(sqrt)

    val T = Array.tabulate(nrows, ncols)( (i, j) => abs(wA(i)(j) - wAmin(j)))

    val Tax = Matrix.rowsums(T)

    val tau = options.getOrElse("tau", 0.02).asInstanceOf[Double]

    val EA = Array.tabulate(nrows, nrows)( (i, j) => Euc(i) - Euc(j))

    val TA = Array.tabulate(nrows, nrows)( (i, j) => Tax(i) - Tax(j))

    val RA = Array.tabulate(nrows, nrows)( (i, j) => if abs(Euc(i) - Euc(j)) < tau then 0.0 else 1.0)

    val scores = Array.tabulate(nrows)( i => 
        Matrix.getrowat(EA, i)
            .zip(Matrix.getrowat(RA, i))
            .zip(Matrix.getrowat(TA, i))
            .map { case ((ea, ra), ta) => ea + (ra * ta) }
            .sum
    )

    val ranks = ranksfromscores(scores)
    

    CodasResult(
        normalizedMatrix = A,
        E = E,
        Euc = Euc,
        T = T,
        Tax = Tax,
        EA = EA,
        TA = TA,
        RA = RA,
        scores = scores,
        ranks = ranks
    )