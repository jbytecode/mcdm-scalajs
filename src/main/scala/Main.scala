package org.expr.mcdm

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLTextAreaElement
import org.scalajs.dom.HTMLDialogElement
import org.scalajs.dom.HTMLSelectElement


import org.expr.mcdm.gui.HtmlUtils
import org.expr.mcdm.parser.Parser
import org.expr.mcdm.parser.Parser.empty_mcdm_problem
import org.expr.mcdm.gui.HtmlUtils.msgbox
import org.expr.mcdm.ranksfromscores

/*
Mutable state objects.
We are using the problem data as the state object.
 */
var problem: MCDMProblem = empty_mcdm_problem()

/*
HTML objects.
 */
val document = dom.document
val window = dom.window
val textarea_strdecmat: HTMLTextAreaElement = document
  .getElementById("textarea_strdecmat")
  .asInstanceOf[HTMLTextAreaElement]
val button_generate_decmat = document
  .getElementById("button_generate_decmat")
  .asInstanceOf[HTMLButtonElement]
val button_generate_weights = document
  .getElementById("button_generate_weights")
  .asInstanceOf[HTMLButtonElement]
val dialog_button_ok =
  document.getElementById("dialog_button_ok").asInstanceOf[HTMLButtonElement]
val dialog_messenger: HTMLDialogElement =
  document.getElementById("dialog_messenger").asInstanceOf[HTMLDialogElement]
val select_separator: HTMLSelectElement =
  document.getElementById("select_separator").asInstanceOf[HTMLSelectElement]
val select_weight_method: HTMLSelectElement = document
  .getElementById("select_weight_method")
  .asInstanceOf[HTMLSelectElement]
val input_criteria_directions: HTMLInputElement = document
  .getElementById("input_criteria_directions")
  .asInstanceOf[HTMLInputElement]
val button_generate_directions: HTMLButtonElement = document
  .getElementById("button_generate_directions")
  .asInstanceOf[HTMLButtonElement]
val button_evaluate: HTMLButtonElement = document
  .getElementById("button_evaluate")
  .asInstanceOf[HTMLButtonElement]
val div_output_scores: HTMLDivElement = document.getElementById("div_output_scores")
  .asInstanceOf[HTMLDivElement]
val div_output_ranks : HTMLDivElement = document.getElementById("div_output_ranks")
  .asInstanceOf[HTMLDivElement]
val div_final_decmat: HTMLDivElement = document
  .getElementById("div_final_decmat")
  .asInstanceOf[HTMLDivElement]
val check_aras = document
  .getElementById("check_aras")
  .asInstanceOf[HTMLInputElement]
val check_cocoso = document
  .getElementById("check_cocoso")
  .asInstanceOf[HTMLInputElement]
val check_codas = document
  .getElementById("check_codas")
  .asInstanceOf[HTMLInputElement]
val check_copras = document
  .getElementById("check_copras")
  .asInstanceOf[HTMLInputElement]
val check_edas = document
  .getElementById("check_edas")
  .asInstanceOf[HTMLInputElement]
val check_lmaw = document
  .getElementById("check_lmaw")
  .asInstanceOf[HTMLInputElement]
val check_lopcow = document
  .getElementById("check_lopcow")
  .asInstanceOf[HTMLInputElement]
val check_mabac = document
  .getElementById("check_mabac")
  .asInstanceOf[HTMLInputElement]
val check_mairca = document
  .getElementById("check_mairca")
  .asInstanceOf[HTMLInputElement]
val check_marcos = document
  .getElementById("check_marcos")
  .asInstanceOf[HTMLInputElement]
val check_moosra = document
  .getElementById("check_moosra")
  .asInstanceOf[HTMLInputElement]
val check_ocra = document
  .getElementById("check_ocra")
  .asInstanceOf[HTMLInputElement]
val check_piv = document
  .getElementById("check_piv")
  .asInstanceOf[HTMLInputElement]
val check_psi = document
  .getElementById("check_psi")
  .asInstanceOf[HTMLInputElement]
val check_ram = document
  .getElementById("check_ram")
  .asInstanceOf[HTMLInputElement]
val check_rov = document
  .getElementById("check_rov")
  .asInstanceOf[HTMLInputElement]
val check_saw = document
  .getElementById("check_saw")
  .asInstanceOf[HTMLInputElement]
val check_topsis = document
  .getElementById("check_topsis")
  .asInstanceOf[HTMLInputElement]
val check_waspas = document
  .getElementById("check_waspas")
  .asInstanceOf[HTMLInputElement]
val check_wpm = document
  .getElementById("check_wpm")
  .asInstanceOf[HTMLInputElement]

/*
Generation of the decision matrix.
 */
def button_generate_decmat_events(): Unit =
  button_generate_decmat.onclick = (e: dom.MouseEvent) =>
    val separator = select_separator.value
    val strdecmat = textarea_strdecmat.value
    val p = Parser.parseCSV(strdecmat, separator)
    problem = p.copy(
      alternatives = p.alternatives,
      criteria = p.criteria,
      data = p.data,
      weights = p.weights,
      directions = p.directions
    )
    val strdecmat_html = HtmlUtils.decmat2html(p)
    div_final_decmat.innerHTML = strdecmat_html
    dialog_button_ok.onclick = (e: dom.MouseEvent) =>
      HtmlUtils.dialog_messenger.close()
    HtmlUtils.msgbox(strdecmat_html)

/*
Generation of the directions.
 */
def button_generate_directions_events(): Unit =
  button_generate_directions.onclick = (e: dom.MouseEvent) =>
    val strdirections = input_criteria_directions.value
    val directions =
      strdirections
        .split(",")
        .map(_.trim)
        .map(_.toLowerCase)
        .filter(_.nonEmpty)
        .filter(x => x == "min" || x == "max")
    if directions.length != problem.criteria.length then
      HtmlUtils.msgbox(
        s"Invalid number of directions ($strdirections). The number of directions should be equal to the number of criteria. ${problem.criteria.length} criteria found, ${directions.length} directions found."
      )
      ()
    problem.directions = directions
    val strdirections_html = HtmlUtils.decmat2html(problem)
    div_final_decmat.innerHTML = strdirections_html
    dialog_button_ok.onclick = (e: dom.MouseEvent) =>
      HtmlUtils.dialog_messenger.close()
    HtmlUtils.msgbox(strdirections_html)

/*
Generation of the weights.
 */
def button_generate_weights_events(): Unit =
  button_generate_weights.onclick = (e: dom.MouseEvent) =>
    val methodname = select_weight_method.value.toString() match
      case "0" => {
        problem.weights =
          Array.fill(problem.criteria.length)(1.0 / problem.criteria.length)
        "Equal weights"
      }
      case "1" => {
        problem.weights = entropy(problem.data, Parser.str2directions(problem.directions)).weights
        "Entropy weights"
      }
      case "2" => {
        problem.weights = critic(problem.data, Parser.str2directions(problem.directions)).weights
        "Critic weights"
      }
      case "3" => {
        problem.weights = sd(problem.data, Parser.str2directions(problem.directions)).weights
        "Standard deviation weights"
      }
      case "4" => {
        problem.weights = merec(problem.data, Parser.str2directions(problem.directions)).weights
        "Merec weights"
      }
      case "5" => {
        problem.weights = 
          cilos(problem.data, Parser.str2directions(problem.directions)).weights
        "Cilos weights"
      }
      case "6" => {
        problem.weights = 
          idocriw(problem.data, Parser.str2directions(problem.directions)).weights
        "Idocriw weights"
      }
      case "7" => {
        problem.weights = 
          lopcow(problem.data, Parser.str2directions(problem.directions)).weights
        "Lopcow weights"
      }
      case "8" => {
        // Custom weights
        msgbox("Custom weights are not implemented yet. Assiging equal weights.")
        problem.weights =
          Array.fill(problem.criteria.length)(1.0 / problem.criteria.length)
        "Custom weights"
      }
      case _ => {
        HtmlUtils.msgbox("Invalid method name")
        problem.weights =
          Array.fill(problem.criteria.length)(1.0 / problem.criteria.length)
      }
    val strproblem = HtmlUtils.decmat2html(problem)
    HtmlUtils.msgbox(
      s"Weights generated by <b>$methodname<b><hr><br>" + 
        s"Problem: <br>" +
        strproblem
      )
    div_final_decmat.innerHTML = strproblem
    


/* 
MCDM Results.
 */
def button_generate_evaluate_events(): Unit = 
  button_evaluate.onclick = (e: dom.MouseEvent) =>
    var methodsnames = Array.empty[String]
    var scores = Matrix.zeros(0, problem.alternatives.length)
    var ranks = Matrix.zeros(0, problem.alternatives.length)
    if check_aras.checked then
      val arasresult = aras(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Aras")
      scores = Matrix.appendcol(scores, arasresult.scores)
      ranks = Matrix.appendcol(ranks, arasresult.ranks)
    if check_cocoso.checked then
      val cocosoresult = cocoso(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Cocoso")
      scores = Matrix.appendcol(scores, cocosoresult.scores)
      ranks = Matrix.appendcol(ranks, cocosoresult.ranks)
    if check_codas.checked then
      val codasresult = codas(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Codas")
      scores = Matrix.appendcol(scores, codasresult.scores)
      ranks = Matrix.appendcol(ranks, codasresult.ranks)
    if check_copras.checked then
      val coprasresult = copras(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Copras")
      scores = Matrix.appendcol(scores, coprasresult.scores)
      ranks = Matrix.appendcol(ranks, coprasresult.ranks)
    if check_edas.checked then
      val edasresult = edas(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Edas")
      scores = Matrix.appendcol(scores, edasresult.scores)
      ranks = Matrix.appendcol(ranks, edasresult.ranks)
    if check_lmaw.checked then
      val lmawresult = lmaw(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Lmaw")
      scores = Matrix.appendcol(scores, lmawresult.scores)
      ranks = Matrix.appendcol(ranks, lmawresult.ranks)
    if check_mabac.checked then
      val mabacresult = mabac(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Mabac")
      scores = Matrix.appendcol(scores, mabacresult.scores)
      ranks = Matrix.appendcol(ranks, mabacresult.ranks)
    if check_mairca.checked then
      val maircaresult = mairca(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Mairca")
      scores = Matrix.appendcol(scores, maircaresult.scores)
      ranks = Matrix.appendcol(ranks, maircaresult.ranks)
    if check_marcos.checked then
      val marcosresult = marcos(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Marcos")
      scores = Matrix.appendcol(scores, marcosresult.scores)
      ranks = Matrix.appendcol(ranks, marcosresult.ranks)
    if check_moosra.checked then
      val moosraresultLeftRight = moosra(problem.data, problem.weights, Parser.str2directions(problem.directions))
      val moosraresult = moosraresultLeftRight match
        case Left(value) => {
          msgbox("Error in Moosra: " + value)
          emptyMoosraResult(problem.data)
        }
        case Right(value) => value

      methodsnames = methodsnames.appended("Moosra")
      scores = Matrix.appendcol(scores, moosraresult.scores)
      ranks = Matrix.appendcol(ranks, moosraresult.ranks)
    if check_ocra.checked then
      val ocrarresult = ocra(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Ocra")
      scores = Matrix.appendcol(scores, ocrarresult.scores)
      ranks = Matrix.appendcol(ranks, ocrarresult.ranks)
    if check_piv.checked then
      val pivresult = piv(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Piv")
      scores = Matrix.appendcol(scores, pivresult.scores)
      ranks = Matrix.appendcol(ranks, pivresult.ranks)
    if check_psi.checked then
      val psiresult = psi(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Psi")
      scores = Matrix.appendcol(scores, psiresult.scores)
      ranks = Matrix.appendcol(ranks, psiresult.ranks)
    if check_ram.checked then
      val ramresult = ram(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Ram")
      scores = Matrix.appendcol(scores, ramresult.scores)
      ranks = Matrix.appendcol(ranks, ramresult.ranks)
    if check_rov.checked then
      val rovresult = rov(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Rov")
      scores = Matrix.appendcol(scores, rovresult.scores)
      ranks = Matrix.appendcol(ranks, rovresult.ranks)
    if check_saw.checked then
      val sawresult = saw(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Saw")
      scores = Matrix.appendcol(scores, sawresult.scores)
      ranks = Matrix.appendcol(ranks, sawresult.ranks)
    if check_topsis.checked then
      val topsisresult = topsis(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Topsis")
      scores = Matrix.appendcol(scores, topsisresult.scores)
      ranks = Matrix.appendcol(ranks, topsisresult.ranks)
    if check_waspas.checked then
      val waspasresult = waspas(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Waspas")
      scores = Matrix.appendcol(scores, waspasresult.scores)
      ranks = Matrix.appendcol(ranks, waspasresult.ranks)
    if check_wpm.checked then
      val wpmresult = wpm(problem.data, problem.weights, Parser.str2directions(problem.directions))
      methodsnames = methodsnames.appended("Wpm")
      scores = Matrix.appendcol(scores, wpmresult.scores)
      ranks = Matrix.appendcol(ranks, wpmresult.ranks)
    
    val resultstr = HtmlUtils.scorematrixtohtml(problem, methodsnames, scores)
    div_output_scores.innerHTML = resultstr

    val resultstr2 = HtmlUtils.scorematrixtohtml(problem, methodsnames, ranks)
    div_output_ranks.innerHTML = resultstr2

def register_events(): Unit =
  button_generate_decmat_events()
  button_generate_directions_events()
  button_generate_weights_events()
  button_generate_evaluate_events()
  window.console.log("register_events called")

@main def hello(): Unit =
  window.console.log("Hello, world!")
  register_events()
