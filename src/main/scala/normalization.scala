package org.expr.mcdm

import org.expr.mcdm.Mat
import org.expr.mcdm.Vec

object Normalization:

  def VectorNormNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =
    val (n, m) = Matrix.size(decmat)
    val columnnorms = Matrix.applyFunctionToColumns(decmat, Matrix.norm)
    Array.tabulate(n, m)((i, j) => decmat(i)(j) / columnnorms(j))

  def DivideByColumnnsSumNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =
    val (n, m) = Matrix.size(decmat)
    val columnsums = Matrix.colsums(decmat)
    Array.tabulate(n, m)((i, j) => decmat(i)(j) / columnsums(j))
