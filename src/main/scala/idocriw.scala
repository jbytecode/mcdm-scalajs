package org.expr.mcdm

import org.expr.mcdm.entropy
import org.expr.mcdm.cilos

case class IdocriwResult(
    entropy_weights: Vec,
    cilos_weights: Vec,
    weights: Vec
) extends MCDMResult

def idocriw(
    mat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization,
    options: Map[String, Any] = Map.empty
): IdocriwResult =

  val entropy_result = entropy(mat, directions)

  val entropy_weight = entropy_result.weights

  val cilos_result = cilos(mat, directions, normalization)

  val cilos_weight = cilos_result.weights

  val qw = entropy_weight.zip(cilos_weight).map { case (a, b) => a * b }

  val qws = qw.sum

  val weights = qw.map(x => x / qws)

  IdocriwResult(
    entropy_weights = entropy_weight,
    cilos_weights = cilos_weight,
    weights = weights
  )
