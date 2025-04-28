package org.expr.mcdm

import org.expr.mcdm.Direction.{Maximize, Minimize}

case class CopelandResult(
    scores: Vec,
    ranks: Vec
)

def level_of_dominance(v1: Vec, v2: Vec): Int =
  v1.zip(v2).filter((a, b) => a < b).length


def dominance_scores(ordering_mat: Mat): Mat =

  val n = ordering_mat.length
  Array.tabulate(n, n)((i, j) =>
    level_of_dominance(ordering_mat(i), ordering_mat(j))
  )

  
def winloss_scores(dommat: Mat): Mat =

  val n = dommat.length
  Array.tabulate(n, n)((i, j) => (dommat(i)(j) - dommat(j)(i)).sign)


def copeland(rankmatrix: Mat): CopelandResult =

  val winloses = winloss_scores(dominance_scores(rankmatrix))
  val scores = Array.tabulate(winloses.length)(i => winloses(i).sum)
  val ranks = ranksfromscores(scores)

  CopelandResult(
    scores = scores,
    ranks = ranks
  )
