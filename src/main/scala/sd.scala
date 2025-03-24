package org.expr.mcdm

case class SdResult(
    cmins: Vec,
    cmaxs: Vec,
    normalizedMat: Mat,
    sds: Vec,
    sumsds: Double,
    weights: Vec
) extends MCDMResult

def sd(
    mat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MaxMinRangeNormalization,
    options: Map[String, Any] = Map.empty
): SdResult =

  val (n, p) = Matrix.size(mat)

  val cmins = Matrix.colmins(mat)

  val cmaxs = Matrix.colmaxs(mat)

  val normalizedMat = normalization(mat, Array.emptyDoubleArray, directions)

  val sds = normalizedMat.transpose.map(Statistics.std(_))

  val sumsds = sds.sum

  val weights = sds.map(_ / sumsds)

  SdResult(
    cmins,
    cmaxs,
    normalizedMat,
    sds,
    sumsds,
    weights
  )
