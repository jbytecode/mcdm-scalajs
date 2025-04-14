package org.expr.mcdm

import org.expr.mcdm.Mat
import org.expr.mcdm.Vec
import org.expr.mcdm.Direction

import scala.math

object Normalization:

  /* 
  Vector Norm Normalization
   */
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
      directions(j) match
        case Direction.Maximize => decmat(i)(j) / columnmaxs(j)
        case Direction.Minimize => columnmins(j) / decmat(i)(j)
    )



  def NullNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =
    decmat
  


  def DivideByAllNormNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =

    val (n, m) = Matrix.size(decmat)

    val matmat = Array.tabulate(n, m)((i, j) => decmat(i)(j) * decmat(i)(j))

    val sumofall = matmat.flatten.sum

    val sqrtsum = math.sqrt(sumofall)

    Array.tabulate(n, m)((i, j) => decmat(i)(j) / sqrtsum)



  def MarcosNormalization(
      decmat: Mat,
      weights: Vec,
      directions: Array[Direction]
  ): Mat =

    val (row, col) = Matrix.size(decmat)
    val AAI = Matrix.zeros(col)
    val AI = Matrix.zeros(col)
    val temp = Matrix.appendrow(Matrix.appendrow(decmat, AI), AAI)
    var normalizedDecisionMat = Matrix.similar(temp)


    for i <- 0 until col do
      if directions(i) == Direction.Maximize then
        AI(i) = Matrix.getcolat(decmat, i).max
        temp(row + 0)(i) = AI(i)
        AAI(i) = Matrix.getcolat(decmat, i).min
        temp(row + 1)(i) = AAI(i)
        normalizedDecisionMat = Matrix.setcolat(normalizedDecisionMat, i, Matrix.getcolat(temp, i).map(_ / AI(i)))
      else
        AI(i) = Matrix.getcolat(decmat, i).min
        temp(row + 0)(i) = AI(i)
        AAI(i) = Matrix.getcolat(decmat, i).max
        temp(row + 1)(i) = AAI(i)
        normalizedDecisionMat = Matrix.setcolat(normalizedDecisionMat, i, Matrix.getcolat(temp, i).map(AI(i)/_))

    normalizedDecisionMat
    

    

  def InverseDivideByColumnMaxMinNormalization(mat: Mat, weights: Vec = Array.emptyDoubleArray, fns: Array[Direction]): Mat =
    
    val (row, col) = Matrix.size(mat)
    
    Array.tabulate(row, col)((i, j) =>
      if fns(j) == Direction.Maximize then
        Matrix.colmins(mat)(j) / mat(i)(j)
      else
        mat(i)(j) / Matrix.colmaxs(mat)(j)
    )

