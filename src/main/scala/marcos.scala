package org.expr.mcdm

case class MarcosResult(
  normalizedDecmat: Mat,
  S: Vec,
  KPlus: Vec,
  KMinus: Vec,
  fKPlus: Vec,
  fKMinus: Vec,
  scores: Vec,
  ranks: Vec,
) extends MCDMResult 

def marcos(
    decmat: Mat,
    weights: Vec,
    directions: Array[Direction],
    normalization: NormalizationFunction =
      Normalization.MarcosNormalization,
    options: Map[String, Any] = Map.empty 
): MarcosResult = 

  val (row, col) = Matrix.size(decmat)

  val normalizedDecmat = normalization(decmat, weights, directions)

  val S = normalizedDecmat.map(row => Matrix.sumproduct(weights, row))

  // KPlus = S[1:row] ./ S[row+1]
  // S.slice(0, 4) takes S(0), S(1), S(2), S(3)
  val KPlus = S.slice(0, row).map(x => x / S(row))

  // KMinus = S[1:row] ./ S[row+2]
  val KMinus = S.slice(0, row).map(x => x / S(row + 1))

  // fKPlus = KPlus ./ (KPlus .+ KMinus)
  val fKPlus = KPlus.zip(KMinus).map((x, y) => x / (x + y))

  // fKMinus = KMinus ./ (KPlus .+ KMinus)
  val fKMinus = KMinus.zip(KPlus).map((x, y) => x / (x + y))

  var scores = Matrix.zeros(row)
  for i <- 0 until row do
    scores(i) =
      (KPlus(i) + KMinus(i)) /
        ((1 + (1 - fKPlus(i)) / fKPlus(i)) + ((1 - fKMinus(i)) / fKMinus(i)))

  val ranks = ranksfromscores(scores)

  MarcosResult(
    normalizedDecmat,
    S,
    KPlus,
    KMinus,
    fKPlus,
    fKMinus,
    scores,
    ranks
  )