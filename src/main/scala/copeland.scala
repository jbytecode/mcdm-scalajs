package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class CopelandResult(
    scores: Vec,
    ranks: Vec
)

def level_of_dominance(v1: Array[Int], v2: Array[Int]): Int =
  var lod = 0
  val n = v1.length
  for (i <- 0 until n)
    if (v1(i) < v2(i))
      lod += 1

  lod

def dominance_scores(ordering_mat: Array[Array[Int]]): Array[Array[Int]] =
  val n = ordering_mat.length
  Array.tabulate(n, n)((i, j) =>
    level_of_dominance(ordering_mat(i), ordering_mat(j))
  )

def winloss_scores(dommat: Array[Array[Int]]): Array[Array[Int]] =
  val n = dommat.length
  Array.tabulate(n, n)((i, j) => (dommat(i)(j) - dommat(j)(i)).sign.toInt)

def copeland(rankmatrix: Array[Array[Int]]): CopelandResult =

  val winloses = winloss_scores(dominance_scores(rankmatrix))
  val scores = Array.tabulate(winloses.length)(i => winloses(i).sum.toDouble)
  val ranks = ranksfromscores(scores)

  CopelandResult(
    scores = scores,
    ranks = ranks
  )
