package org.expr.mcdm

case class PsiResult(
    normalizedDecisionMat: Mat,
    pvs: Vec,
    phis: Vec,
    sum_phis: Double,
    psis: Vec,
    scores: Vec,
    ranks: Vec
) extends MCDMResult


def psi(
    decisionMat: Mat,
    weights: Vec = Array.emptyDoubleArray,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnMaxMinNormalization,
    options: Map[String, Any] = Map.empty
): PsiResult =

    def PV(v: Vec): Double = 
        v.map(_ - Statistics.mean(v)).map(x => x * x).sum
        
    val (row, col) = Matrix.size(decisionMat)

    val normalizedDecisionMat = normalization(decisionMat, weights, directions)

    // Preference variation is computed for each criterion (column),
    // as defined by Maniya and Bhatt (2010).
    val pvs = Array.range(0, col).map(j => PV(Matrix.getcolat(normalizedDecisionMat, j)))

    val phis = pvs.map(x => 1 - x)

    val sum_phis = phis.sum

    val psis = phis.map(x => x / sum_phis)

    val Is = Array.range(0, row).map(i => Matrix.sumproduct(Matrix.getrowat(normalizedDecisionMat, i), psis))

    val scores = Is

    val ranks = ranksfromscores(scores)

    PsiResult(
        normalizedDecisionMat,
        pvs,
        phis,
        sum_phis,
        psis,
        scores,
        ranks
    )