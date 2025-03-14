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
    
    val decmat = Array(
      Array(1.0, 2.0, 3.0),
      Array(4.0, 5.0, 6.0),
      Array(7.0, 8.0, 9.0)
    )
    val weights = Array(0.3, 0.5, 0.2)
    val directions = Array(Direction.Maximize, Direction.Maximize, Direction.Maximize)
    val result = topsis(decmat, weights, directions)
    val result2 = aras(decmat, weights, directions)
    val result3 = saw(decmat, weights, directions)
    val result4 = critic(decmat, directions)
    val table1 = HtmlUtils.makeOutput(result)
    decmatoutput.appendChild(table1)
@main def hello(): Unit =
  window.console.log("Hello, world!")
  registerEvents() 
  window.console.log("Registered events")


