package org.expr.mcdm.gui

import org.scalajs.dom
import org.scalajs.dom._

object DomObjects:

  val document = dom.document
  val window = dom.window
  val div_step1: HTMLDivElement =
    document.getElementById("div_step1").asInstanceOf[HTMLDivElement]
  val div_step2: HTMLDivElement =
    document.getElementById("div_step2").asInstanceOf[HTMLDivElement]
  val div_step3: HTMLDivElement =
    document.getElementById("div_step3").asInstanceOf[HTMLDivElement]
  val div_step4: HTMLDivElement =
    document.getElementById("div_step4").asInstanceOf[HTMLDivElement]
  val button_showhide_step1 = document
    .getElementById("button_showhide_step1")
    .asInstanceOf[HTMLButtonElement]
  val button_showhide_step2 = document
    .getElementById("button_showhide_step2")
    .asInstanceOf[HTMLButtonElement]
  val button_showhide_step3 = document
    .getElementById("button_showhide_step3")
    .asInstanceOf[HTMLButtonElement]
  val button_showhide_step4 = document
    .getElementById("button_showhide_step4")
    .asInstanceOf[HTMLButtonElement]
  val button_showhide_finaldecmat = document
    .getElementById("button_showhide_finaldecmat")
    .asInstanceOf[HTMLButtonElement]
  val textarea_strdecmat: HTMLTextAreaElement = document
    .getElementById("textarea_strdecmat")
    .asInstanceOf[HTMLTextAreaElement]
  val button_generate_decmat = document
    .getElementById("button_generate_decmat")
    .asInstanceOf[HTMLButtonElement]
  val button_generate_weights = document
    .getElementById("button_generate_weights")
    .asInstanceOf[HTMLButtonElement]
  val dialog_div_content: HTMLDivElement =
    document.getElementById("dialog_div_content").asInstanceOf[HTMLDivElement]
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
  val div_output_scores: HTMLDivElement = document
    .getElementById("div_output_scores")
    .asInstanceOf[HTMLDivElement]
  val div_output_ranks: HTMLDivElement = document
    .getElementById("div_output_ranks")
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
  val check_nds = document
    .getElementById("check_nds")
    .asInstanceOf[HTMLInputElement]
