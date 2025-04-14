package org.expr.mcdm

import scala.math.max 

import org.expr.mcdm.Direction.{Minimize, Maximize}

case class EdasResult(
    PDAMatrix: Mat,
    NDAMatrix: Mat,
    weightedPDAMatrix: Mat,
    weightedNDAMatrix: Mat,
    SP: Vec,
    SN: Vec,
    NSP: Vec,
    NSN: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

def edas(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
    options: Map[String, Any] = Map.empty 
): EdasResult =

    val (row, col) = Matrix.size(decmat)

    val AV = Array.range(0, col).map(i => Statistics.mean(Matrix.getcolat(decmat, i)))

    val PDAMatrix = Array.tabulate(row, col)((i, j) => directions(j) match
        case Maximize => max(0.0, decmat(i)(j) - AV(j)) / AV(j)
        case Minimize => max(0.0, AV(j) - decmat(i)(j)) / AV(j)
    )

    val NDAMatrix = Array.tabulate(row, col)((i, j) => directions(j) match
        case Maximize => max(0.0, AV(j) - decmat(i)(j)) / AV(j)
        case Minimize => max(0.0, decmat(i)(j) - AV(j)) / AV(j)
    )
    
    val weightedPDAMatrix = Matrix.weightizeColumns(PDAMatrix, weights)

    val weightedNDAMatrix = Matrix.weightizeColumns(NDAMatrix, weights)


    val SP = weightedPDAMatrix.map(row => row.sum)

    val SN = weightedNDAMatrix.map(row => row.sum)


    val NSP = SP.map(_ / SP.max)

    val NSN = SN.map(x => 1.0 - x / SN.max)


    val scores = NSP.zip(NSN).map((x, y) => (x + y) / 2.0)
    
    val ranks = ranksfromscores(scores)

    EdasResult(
        PDAMatrix,
        NDAMatrix,
        weightedPDAMatrix,
        weightedNDAMatrix,
        SP,
        SN,
        NSP,
        NSN,
        scores,
        ranks
    )
