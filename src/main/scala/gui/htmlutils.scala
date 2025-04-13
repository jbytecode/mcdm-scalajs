package org.expr.mcdm.gui

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLDialogElement

import org.expr.mcdm.*

object HtmlUtils:
    val document = dom.document 
    val window = dom.window
    val dialog_messenger: HTMLDialogElement = document.getElementById("dialog_messenger").asInstanceOf[HTMLDialogElement]
   
    def msgbox(msg: String): Unit = 
        val divcontent = document.getElementById("dialog_div_content").asInstanceOf[HTMLDivElement]
        val dialog_button_ok = document.getElementById("dialog_button_ok").asInstanceOf[HTMLInputElement]
        divcontent.innerHTML = msg
        dialog_button_ok.onclick = (e: dom.MouseEvent) => 
            dialog_messenger.close()
        dialog_messenger.showModal()

    def scorematrixtohtml(problem: MCDMProblem, methodnames: Array[String], scores: Mat): String = 
        val sb = new StringBuilder
        sb.append("<table>")
        sb.append("<tr>")
        sb.append("<th>Alternatives</th>")
        for (i <- methodnames.indices) 
            sb.append(s"<th>${methodnames(i)}</th>")
        sb.append("</tr>")
        for (i <- problem.alternatives.indices) 
            sb.append("<tr>")
            sb.append(s"<td>${problem.alternatives(i)}</td>")
            for (j <- methodnames.indices) 
                sb.append(s"<td>${scores(i)(j)}</td>")
            sb.append("</tr>")
        sb.append("</table>")
        sb.toString()


    def decmat2html(problem: MCDMProblem): String = 
        if problem == null then 
            return "<p>Problem is null</p>"
        if problem.alternatives.isEmpty then
            return "<p>Problem has no alternatives</p>"
        if problem.criteria.isEmpty then
            return "<p>Problem has no criteria</p>"
        if problem.data.isEmpty then
            return "<p>Problem has no data</p>"
        if problem.directions.isEmpty then
            return "<p>Problem has no directions</p>"
        if problem.weights.isEmpty then
            return "<p>Problem has no weights</p>"
        if problem.data(0).isEmpty then
            return "<p>Problem has no data and/or problem is not parsed correctly. Did you select the right separator?</p>"

        val sb = new StringBuilder
        sb.append("<table>")
        sb.append("<tr>")
        sb.append("<th>Alternatives</th>")
        for (i <- problem.criteria.indices) 
            sb.append(s"<th>${problem.criteria(i)}</th>")
        sb.append("</tr>")
        for (i <- problem.alternatives.indices) 
            sb.append("<tr>")
            sb.append(s"<td>${problem.alternatives(i)}</td>")
            for (j <- problem.criteria.indices) 
                sb.append(s"<td>${problem.data(i)(j)}</td>")
            sb.append("</tr>")
        sb.append("<tr>")
        sb.append("<th>Weights</th>")
        for (i <- problem.criteria.indices) 
            sb.append(s"<td>${problem.weights(i)}</td>")
        sb.append("</tr>")
        sb.append("<tr>")
        sb.append("<th>Directions</th>")
        for (i <- problem.criteria.indices) 
            sb.append(s"<td>${problem.directions(i)}</td>")
        sb.append("</tr>")
        sb.append("</table>")
        
        sb.toString()