package org.expr.mcdm

import org.expr.mcdm.Direction._

case class RimResult(
    normalizedMatrix: Mat,
    weightedNormalizedMatrix: Mat,
    weights: Vec,
    distanceToIdeal: Vec,
    distanceToOrigin: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult

// Ranges and reference ideals are (lower, upper) pairs per criterion. When omitted,
// ranges are the column min/max and the reference ideal is the best column value.
def rim(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    ranges: Option[Array[(Double, Double)]] = None,
    referenceIdeals: Option[Array[(Double, Double)]] = None,
    options: Map[String, Any] = Map.empty
): RimResult =

    val (n, m) = Matrix.size(decmat)

    require(weights.length == m, "Length of weights must be equal to number of criteria")
    require(weights.forall(_ >= 0.0) && weights.sum > 0.0, "weights must be nonnegative with a positive sum")

    val colmins = Matrix.colmins(decmat)
    val colmaxs = Matrix.colmaxs(decmat)

    val rs = ranges.getOrElse(Array.tabulate(m)(j => (colmins(j), colmaxs(j))))
    val ideals = referenceIdeals.getOrElse(
      Array.tabulate(m)(j =>
        directions(j) match
          case Maximize => (colmaxs(j), colmaxs(j))
          case Minimize => (colmins(j), colmins(j))
      )
    )

    require(rs.length == m, "Length of ranges must be equal to number of criteria")
    require(ideals.length == m, "Length of referenceIdeals must be equal to number of criteria")

    for j <- 0 until m do
      val (lower, upper) = rs(j)
      val (il, iu) = ideals(j)
      require(lower <= upper, s"ranges($j) must have lower <= upper")
      require(il <= iu, s"referenceIdeals($j) must have lower <= upper")
      require(lower <= il && iu <= upper, s"referenceIdeals($j) must be contained in ranges($j)")
      require(
        decmat.forall(row => row(j) >= lower && row(j) <= upper),
        s"decision matrix values for criterion $j must be contained in ranges($j)"
      )

    val w = weights.map(_ / weights.sum)

    val normalizedMatrix = Array.tabulate(n, m) { (i, j) =>
      val v = decmat(i)(j)
      val (lower, upper) = rs(j)
      val (il, iu) = ideals(j)
      if v >= il && v <= iu then 1.0
      else if v < il then 1.0 - (il - v) / (il - lower)
      else 1.0 - (v - iu) / (upper - iu)
    }

    val weightedNormalizedMatrix = Matrix.weightizeColumns(normalizedMatrix, w)

    val distanceToIdeal = weightedNormalizedMatrix.map(row => Statistics.euclideanDistance(row, w))
    val distanceToOrigin = weightedNormalizedMatrix.map(row => Matrix.norm(row))

    val scores = distanceToOrigin.zip(distanceToIdeal).map((dorigin, di) => dorigin / (di + dorigin))

    val ranks = ranksfromscores(scores)

    RimResult(normalizedMatrix, weightedNormalizedMatrix, w, distanceToIdeal, distanceToOrigin, scores, ranks)
