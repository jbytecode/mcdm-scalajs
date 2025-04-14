package org.expr.mcdm

import org.expr.mcdm.Direction.{Minimize, Maximize}
import scala.math.{log, abs}

case class MerecResult(
    weights: Vec
) extends MCDMResult

def merec(
    decmat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.InverseDivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
): MerecResult =

  val (row, col) = Matrix.size(decmat)

  val NormalizeMatrix =
    normalization(decmat, Array.emptyDoubleArray, directions)

  val S = Array
    .range(0, row)
    .map(i =>
      log(
        1 + (
          Matrix
            .getrowat(NormalizeMatrix, i)
            .map(log)
            .map(abs)
            .sum / col
        )
      )
    )

  val S_ = Array.tabulate(row, col)((i, j) =>
    log(
      1 + (
        (Matrix
          .getrowat(NormalizeMatrix, i)
          .map(log)
          .map(abs)
          .sum - abs(log(NormalizeMatrix(i)(j)))) / col
      )
    )
  )

  val E = Array
    .range(0, col)
    .map(j =>
      abs(
        Matrix
          .subtract(Matrix.getcolat(S_, j), S)
          .sum
      )
    )

  val w = E.map(_ / E.sum)

  MerecResult(
    weights = w
  )
