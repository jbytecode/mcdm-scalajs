package org.expr.mcdm

import scala.math.{log, sqrt}
import Statistics.sum
import org.expr.mcdm.Direction.{Maximize, Minimize}

case class LopcowResult(
    weights: Vec
) extends MCDMResult


def lopcow(
    decmat: Mat, 
    directions: Array[Direction], 
    normalization: NormalizationFunction = Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty
    ): LopcowResult = 

    val (row, col) = Matrix.size(decmat)

    val normalizedMat = normalization(decmat, Array.emptyDoubleArray, directions)

    val PV = Matrix.zeros(col)

    for i <- 0 until col do
        PV(i) = 
            val colat = Matrix.getcolat(normalizedMat, i)
            val colatsquared = colat.map(x => x * x)
            log(sqrt(sum(colatsquared) / row) / Statistics.std(colat)) * 100

    val weights = PV.map(x => x / sum(PV))


    LopcowResult(weights)