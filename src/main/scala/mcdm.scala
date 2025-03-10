package org.expr.mcdm

type Mat = Array[Array[Double]]
type Vec = Array[Double]

type NormalizationFunction = (Mat, Vec, Array[Direction]) => Mat

enum Direction:
    case Minimize, Maximize

trait MCDMResult;