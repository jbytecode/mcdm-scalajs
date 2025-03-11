package org.expr.mcdm

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLButtonElement
import org.scalajs.dom.HTMLTextAreaElement
import org.w3c.dom.html.HTMLUListElement


val document = dom.document
val window = dom.window

def registerEvents(): Unit =
  val input: HTMLInputElement = document.getElementById("btn_generate_decmat").asInstanceOf[HTMLInputElement]
  window.console.log("We are in registerEvents")
  input.onclick = (e: dom.Event) =>
    window.console.log("Button clicked")
    val decmatoutput: HTMLDivElement = document.getElementById("div_decmat_output").asInstanceOf[HTMLDivElement]
    val textarea : HTMLTextAreaElement = document.getElementById("textarea_decmat").asInstanceOf[HTMLTextAreaElement]
    val textcontent = textarea.value
    decmatoutput.innerHTML = ""
    decmatoutput.appendChild(HtmlUtils.makeTable(HtmlUtils.parseTextToTable(textcontent)))
@main def hello(): Unit =
  window.console.log("Hello, world!")
  registerEvents() 
  window.console.log("Registered events")


