package org.expr.mcdm

import scala.math.max 

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
    orderings: VecInt,
    bestIndex: Int,
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

    var PDAMatrix = Matrix.zeros(row, col)
    var NDAMatrix = Matrix.zeros(row, col)

    var AV = Matrix.zeros(col)


    for i <- 0 until col do
        AV(i) = Statistics.mean(Matrix.getcolat(decmat, i))
        for j <- 0 until row do
            if directions(i) == Direction.Maximize then
                PDAMatrix(j)(i) = max(0.0, decmat(j)(i) - AV(i)) / AV(i)
                NDAMatrix(j)(i) = max(0.0, AV(i) - decmat(j)(i)) / AV(i)
            else
                PDAMatrix(j)(i) = Math.max(0.0, AV(i) - decmat(j)(i)) / AV(i)
                NDAMatrix(j)(i) = Math.max(0.0, decmat(j)(i) - AV(i)) / AV(i)
           
    


    val weightedPDAMatrix = Matrix.weightizeColumns(PDAMatrix, weights)
    val weightedNDAMatrix = Matrix.weightizeColumns(NDAMatrix, weights)


    val SP = weightedPDAMatrix.map(row => row.sum)
    val SN = weightedNDAMatrix.map(row => row.sum)


    val NSP = SP.map(_ / SP.max)
    val NSN = SN.map(x => 1.0 - x / SN.max)


    val scores = NSP.zip(NSN).map((x, y) => (x + y) / 2.0)
    val orderings = scores.zipWithIndex.sortBy(_._1).map(_._2)
    val bestIndex = orderings.last

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
        orderings,
        bestIndex
    )
