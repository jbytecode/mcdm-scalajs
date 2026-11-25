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

def ranksfromscores(scores: Vec, reverse: Boolean = true): Vec =
  // Rank of each alternative in input order (1 = best).
  // reverse = true: a higher score is better; reverse = false: a lower score is better.
  val order =
    if reverse then scores.zipWithIndex.sortBy(_._1).map(_._2).reverse
    else scores.zipWithIndex.sortBy(_._1).map(_._2)
  val ranks = Array.fill(scores.length)(0.0)
  order.zipWithIndex.foreach((alternative, position) => ranks(alternative) = position + 1.0)
  ranks
