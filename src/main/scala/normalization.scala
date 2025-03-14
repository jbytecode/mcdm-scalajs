package org.expr.mcdm

import org.expr.mcdm.Mat
import org.expr.mcdm.Vec
import org.expr.mcdm.Direction

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

  def MaxMinRangeNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =
    val (n, m) = Matrix.size(decmat)
    val columnmins = Matrix.colmins(decmat)
    val columnmaxs = Matrix.colmaxs(decmat)
    Array.tabulate(n, m)((i, j) =>
      if directions(j) == Direction.Maximize
      then (decmat(i)(j) - columnmins(j)) / (columnmaxs(j) - columnmins(j))
      else (columnmaxs(j) - decmat(i)(j)) / (columnmaxs(j) - columnmins(j))
    )

  def DivideByColumnMaxMinNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =
    val (n, m) = Matrix.size(decmat)
    val columnmins = Matrix.colmins(decmat)
    val columnmaxs = Matrix.colmaxs(decmat)

    Array.tabulate(n, m)((i, j) =>
      if directions(j) == Direction.Maximize
      then decmat(i)(j) / columnmaxs(j)
      else columnmins(j) / decmat(i)(j)
    )
