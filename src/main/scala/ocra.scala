package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

case class OcraResult(
    I1: Vec,
    O1: Vec,
    I2: Vec,
    O2: Vec,
    scores: Vec
    ) extends MCDMResult 


def ocra(
    decisionMat: Mat,
    weights: Vec = Array.emptyDoubleArray,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.NullNormalization,
    options: Map[String, Any] = Map.empty
): OcraResult =

    val (row, col) = Matrix.size(decisionMat)

    var I1 = Matrix.zeros(row)

    val O1 = Matrix.zeros(row)

    val colMax = Matrix.colmaxs(decisionMat)
    val colMin = Matrix.colmins(decisionMat)

    for i <- 0 until row do
        for j <- 0 until col do
            if directions(j) == Minimize then
                I1(i) = I1(i) + weights(j) * (colMax(j) - decisionMat(i)(j)) / colMin(j)
            else
                O1(i) = O1(i) + weights(j) * (decisionMat(i)(j) - colMin(j)) / colMin(j)

    val I2 = I1.map(x => x - I1.min)

    val O2 = O1.map(x => x - O1.min)

    val i2o2 = I2.zip(O2).map { case (i, o) => i + o }

    val scores = i2o2.map(x => x - i2o2.min)

    OcraResult(
        I1 = I1,
        O1 = O1,
        I2 = I2,
        O2 = O2,
        scores = scores
    )