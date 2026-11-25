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
  // Rank of each alternative in input order: 1 + the number of alternatives with a
  // strictly better score, so tied scores share a rank (1 = best).
  // reverse = true: a higher score is better; reverse = false: a lower score is better.
  scores.map(s => 1.0 + scores.count(t => if reverse then t > s else t < s))
