package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}


case class DematelResult(
    comparisonMatrix: Mat,
    NDMatrix: Mat,
    relationShipMatrix: Mat,
    c: Vec,
    r: Vec,
    c_plus_r: Vec,
    c_minus_r: Vec,
    threshold: Double,
    influenceMatrix: Mat,
    weights: Vec
) extends MCDMResult


def dematel(
    comparisonMat: Mat,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.DivideByColumnsSumNormalization,
    options: Map[String, Any] = Map.empty
): DematelResult =

    val (n, _) = Matrix.size(comparisonMat)

    val csums = Matrix.colsums(comparisonMat)
    val rsums = Matrix.rowsums(comparisonMat)

    val largest = (csums ++ rsums).max

    val K = Matrix.replicate(comparisonMat)

    val ND = Matrix.elementwiseDivide(K, Array.fill(n, n)(largest))

    val I = Matrix.identity(n)

    val T = Matrix.mul(ND, Matrix.inverse(Matrix.subtract(I, ND)))

    val c = Matrix.rowsums(T)
    val r = Matrix.colsums(T)

    val c_plus_r = Matrix.elementwiseAdd(c, r)
    val c_minus_r = Matrix.subtract(c, r)

    val threshold = options.getOrElse("threshold", Matrix.globalSum(T) / (n * n)).asInstanceOf[Double]

    val influenceMatrix = Array.tabulate(n, n)((i, j) =>
        if T(i)(j) > threshold then 1.0 else 0.0
    )

    val w_row = Array.tabulate(n)(i =>
        math.sqrt(math.pow(c_plus_r(i), 2.0) + math.pow(c_minus_r(i), 2.0))
    )

    val w = Matrix.elementwiseDivide(w_row, Array.fill(n)(w_row.sum))

    DematelResult(
        comparisonMat,
        ND,
        T,
        c,
        r,
        c_plus_r,
        c_minus_r,
        threshold,
        influenceMatrix,
        w
    )