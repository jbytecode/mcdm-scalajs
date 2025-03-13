package org.expr.mcdm

case class ArasResult(
    referenceRow: Vec,
    extendMat: Mat,
    normalizedMat: Mat,
    optimality_degrees: Vec,
    scores: Vec,
    orderings: VecInt,
    bestIndex: Int
) extends MCDMResult

def aras(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnnsSumNormalization
): ArasResult =
  val (nrows, ncols) = Matrix.size(decmat)

  val referenceRow = Matrix.colminmax(decmat, directions)

  val extendMat = Matrix
    .appendrow(decmat, referenceRow)
    .transpose
    .zip(directions)
    .map((col, dir) =>
      dir match
        case Direction.Minimize => col.map(value => 1.0 / value)
        case Direction.Maximize => col
    )
    .transpose

  val normalized = normalization(extendMat, weights, directions)

  var optimalityDegrees = Array.fill(nrows + 1)(0.0)
  for i <- 0 until (nrows + 1) do
    optimalityDegrees(i) =
      weights.zip(Matrix.getrowat(normalized, i)).map((w, x) => w * x).sum

  var utilityDegrees = Array.fill(nrows)(0.0)
  for i <- 0 until nrows do
    utilityDegrees(i) = optimalityDegrees(i) / optimalityDegrees(nrows)

  val orderings = utilityDegrees.zipWithIndex.sortBy(-_._1).map(_._2)
  val bestIndex = orderings.last

  ArasResult(
    referenceRow,
    extendMat,
    normalized,
    optimalityDegrees,
    utilityDegrees,
    orderings,
    bestIndex
  )
