package org.expr.mcdm

type Mat = Array[Array[Double]]

type Vec = Array[Double]

type VecInt = Array[Int]

type NormalizationFunction = (Mat, Vec, Array[Direction]) => Mat

enum Direction:
    case Minimize, Maximize

trait MCDMResult;

case class MCDMProblem(
  var alternatives: Array[String],
  var criteria: Array[String],
  var data: Mat,
  var weights: Array[Double],
  var directions: Array[String]
) 
