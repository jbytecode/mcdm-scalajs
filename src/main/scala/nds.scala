package org.expr.mcdm

import org.expr.mcdm.Direction.{Minimize, Maximize}

case class NdsResult(
    scores: Vec
) extends MCDMResult

def dominates(p1: Vec, p2: Vec, fns: Array[Direction]): Boolean =
  val n = p1.length
  val notworse = (0 until n).count(i =>
    if fns(i) == Maximize then p1(i) < p2(i)
    else p1(i) > p2(i)
  )
  val better = (0 until n).count(i =>
    if fns(i) == Maximize then p1(i) > p2(i)
    else p1(i) < p2(i)
  )
  (notworse == 0) && (better > 0)

def ndsranks(decmat: Mat, directions: Array[Direction]): Vec =

  val (n, m) = Matrix.size(decmat)

  val ranks = Matrix.zeros(n)

  for i <- 0 until n do
    for j <- 0 until n do
      if i != j then
        if dominates(decmat(i), decmat(j), directions) then ranks(i) += 1

  ranks

def nds(decmat: Mat, directions: Array[Direction]): NdsResult =

  val (n, m) = Matrix.size(decmat)

  val ranks = ndsranks(decmat, directions)

  NdsResult(
    scores = ranks
  )
