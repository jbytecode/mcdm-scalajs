package org.expr.mcdm.gui

import org.scalajs.dom
import org.scalajs.dom._

object DomObjects:

  val document = dom.document
  val window = dom.window

  def getElt[T](id: String): T =
    document.getElementById(id).asInstanceOf[T]
    
  val div_step1 = getElt[HTMLDivElement]("div_step1")
  val div_step2 = getElt[HTMLDivElement]("div_step2")
  val div_step3 = getElt[HTMLDivElement]("div_step3")
  val div_step4 = getElt[HTMLDivElement]("div_step4")
  val button_showhide_step1 = getElt[HTMLButtonElement]("button_showhide_step1")
  val button_showhide_step2 = getElt[HTMLButtonElement]("button_showhide_step2")
  val button_showhide_step3 = getElt[HTMLButtonElement]("button_showhide_step3")
  val button_showhide_step4 = getElt[HTMLButtonElement]("button_showhide_step4")
  val button_showhide_finaldecmat = getElt[HTMLButtonElement]("button_showhide_finaldecmat")
  val textarea_strdecmat = getElt[HTMLTextAreaElement]("textarea_strdecmat")
  val button_generate_decmat = getElt[HTMLButtonElement]("button_generate_decmat")
  val button_generate_weights = getElt[HTMLButtonElement]("button_generate_weights")
  val dialog_div_content = getElt[HTMLDivElement]("dialog_div_content")
  val dialog_button_ok = getElt[HTMLButtonElement]("dialog_button_ok")
  val dialog_messenger = getElt[HTMLDialogElement]("dialog_messenger")
  val select_separator = getElt[HTMLSelectElement]("select_separator")
  val select_weight_method = getElt[HTMLSelectElement]("select_weight_method")
  val input_criteria_directions = getElt[HTMLInputElement]("input_criteria_directions")
  val button_generate_directions = getElt[HTMLButtonElement]("button_generate_directions")
  val button_evaluate = getElt[HTMLButtonElement]("button_evaluate")
  val div_output_scores = getElt[HTMLDivElement]("div_output_scores")
  val div_output_ranks = getElt[HTMLDivElement]("div_output_ranks")
  val div_final_decmat = getElt[HTMLDivElement]("div_final_decmat")
  val check_aras = getElt[HTMLInputElement]("check_aras")
  val check_cocoso = getElt[HTMLInputElement]("check_cocoso")
  val check_codas = getElt[HTMLInputElement]("check_codas")
  val check_copras = getElt[HTMLInputElement]("check_copras")
  val check_edas = getElt[HTMLInputElement]("check_edas")
  val check_lmaw = getElt[HTMLInputElement]("check_lmaw")
  val check_lopcow = getElt[HTMLInputElement]("check_lopcow")
  val check_mabac = getElt[HTMLInputElement]("check_mabac")
  val check_mairca = getElt[HTMLInputElement]("check_mairca")
  val check_marcos = getElt[HTMLInputElement]("check_marcos")
  val check_moosra = getElt[HTMLInputElement]("check_moosra")
  val check_ocra = getElt[HTMLInputElement]("check_ocra")
  val check_piv = getElt[HTMLInputElement]("check_piv")
  val check_psi = getElt[HTMLInputElement]("check_psi")
  val check_ram = getElt[HTMLInputElement]("check_ram")
  val check_rov = getElt[HTMLInputElement]("check_rov")
  val check_saw = getElt[HTMLInputElement]("check_saw")
  val check_topsis = getElt[HTMLInputElement]("check_topsis")
  val check_waspas = getElt[HTMLInputElement]("check_waspas")
  val check_wpm = getElt[HTMLInputElement]("check_wpm")
  val check_nds = getElt[HTMLInputElement]("check_nds")
  val check_copeland = getElt[HTMLInputElement]("check_copeland")

