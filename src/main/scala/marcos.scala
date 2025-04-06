package org.expr.mcdm

case class MarcosResult() extends MCDMResult 

def marcos(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MarcosNormalization,
    options: Map[String, Any] = Map.empty 
): MarcosResult = ???