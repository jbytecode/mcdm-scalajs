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
    scores: Vec
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

    val wA = Matrix.zeros(nrows, ncols)

    for i <- 0 until ncols do
        for j <- 0 until nrows do
            wA(j)(i) = A(j)(i) * weights(i)

    
    val wAmin = Matrix.colmins(wA)


    val E = Matrix.zeros(nrows, ncols)

    for i <- 0 until nrows do
        for j <- 0 until ncols do
            E(i)(j) = pow(wA(i)(j) - wAmin(j), 2)


    val Euc = Matrix.zeros(nrows)

    for i <- 0 until nrows do
        Euc(i) = sqrt(Matrix.getrowat(E, i).sum)


    val T = Matrix.zeros(nrows, ncols)

    for i <- 0 until nrows do
        for j <- 0 until ncols do
            T(i)(j) = abs(wA(i)(j) - wAmin(j))


    val Tax = Matrix.zeros(nrows)

    for i <- 0 until nrows do
        Tax(i) = Matrix.getrowat(T, i).sum


    val EA = Matrix.zeros(nrows, nrows)

    val TA = Matrix.zeros(nrows, nrows)

    val RA = Matrix.zeros(nrows, nrows)


    val tau = options.getOrElse("tau", 0.02).asInstanceOf[Double]

    for i <- 0 until nrows do
        for j <- 0 until nrows do
            EA(i)(j) = Euc(i) - Euc(j)
            TA(i)(j) = Tax(i) - Tax(j)
            if abs(Euc(i) - Euc(j)) < tau then
                RA(i)(j) = 0
            else
                RA(i)(j) = 1
    

    val scores = Matrix.zeros(nrows)

    for i <- 0 until nrows do
        scores(i) = Matrix.getrowat(EA, i)
            .zip(Matrix.getrowat(RA, i))
            .zip(Matrix.getrowat(TA, i))
            .map { case ((ea, ra), ta) => ea + (ra * ta) }
            .sum
    

    CodasResult(
        normalizedMatrix = A,
        E = E,
        Euc = Euc,
        T = T,
        Tax = Tax,
        EA = EA,
        TA = TA,
        RA = RA,
        scores = scores
    )