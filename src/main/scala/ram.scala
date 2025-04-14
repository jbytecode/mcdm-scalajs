package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class RamResult(
    normalized_decmat: Mat,
    weighted_normalized_decmat: Mat,
    siplus: Vec,
    siminus: Vec,
    scores: Vec,
    norRI: Vec,
    ranks: Vec
) extends MCDMResult 

def ram(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
    options: Map[String, Any] = Map.empty 
): RamResult = 

    val (n, m) = Matrix.size(decmat)

    val normalized_decmat = 
      normalization(decmat, weights, directions)

    val weighted_normalized_decmat = 
      Matrix.weightizeColumns(normalized_decmat, weights)

    var siplus = Matrix.zeros(n)
    var siminus = Matrix.zeros(n)
    for i <- 0 until n do
      for j <- 0 until m do
        if directions(j) == Minimize then
          siminus(i) += weighted_normalized_decmat(i)(j)
          else  
          siplus(i) += weighted_normalized_decmat(i)(j)
      
       
    val siplus2 = siplus.map(x => x + 2)

    val siminus2 = siminus.map(x => x + 2)
    
    val squarevals = siplus2.zip(siminus2).map((x, y) => Math.pow(x, 1.0 / y))

    val scores = squarevals

    val mmin = squarevals.min

    val mmax = squarevals.max
    
    val norRI = squarevals.map(x => (x - mmin) / (mmax - mmin))

    val ranks = ranksfromscores(scores)

    RamResult(
        normalized_decmat,
        weighted_normalized_decmat,
        siplus,
        siminus,
        squarevals,
        norRI,
        ranks
    )