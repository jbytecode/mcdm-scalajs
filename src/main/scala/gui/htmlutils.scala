package org.expr.mcdm.gui

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.scalajs.dom.HTMLDialogElement


import org.expr.mcdm.*
import org.expr.mcdm.gui.DomObjects

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

    def roundToNDigits(x: Double, n: Int = 6): Double = 
        s"%.${n}f".format(x).toDouble

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
                sb.append(s"<td>${roundToNDigits(scores(i)(j))}</td>")
            sb.append("</tr>")
        sb.append("</table>")
        sb.toString()

    def prepareTable(problem: MCDMProblem): String = 
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
                sb.append(s"<td>${roundToNDigits(problem.data(i)(j))}</td>")
            sb.append("</tr>")
        sb.append("<tr>")
        sb.append("<th>Weights</th>")
        for (i <- problem.criteria.indices) 
            sb.append(s"<td>${roundToNDigits(problem.weights(i))}</td>")
        sb.append("</tr>")
        sb.append("<tr>")
        sb.append("<th>Directions</th>")
        for (i <- problem.criteria.indices) 
            sb.append(s"<td>${problem.directions(i)}</td>")
        sb.append("</tr>")
        sb.append("</table>")
        sb.toString()

    def checkProblemIntegrity(problem: MCDMProblem): Either[String, MCDMProblem] = 
        problem match 
            case null => Left("Problem is null")
            case _ if problem.alternatives.isEmpty => Left("Problem has no alternatives")
            case _ if problem.criteria.isEmpty => Left("Problem has no criteria")
            case _ if problem.data.isEmpty => Left("Problem has no data")
            case _ if problem.directions.isEmpty => Left("Problem has no directions")
            case _ if problem.weights.isEmpty => Left("Problem has no weights")
            case _ if problem.data(0).isEmpty => Left("Problem has no data and/or problem is not parsed correctly. Did you select the right separator?")
            case _ => Right(problem)

    def decmat2html(inproblem: MCDMProblem): String = 
        val integrity = checkProblemIntegrity(inproblem)
        integrity match 
            case Left(msg) => 
                msgbox(msg)
                msg
            case Right(problem) => 
                prepareTable(problem)

    def askforcustomweights(p:MCDMProblem): Unit = 
        DomObjects.dialog_div_content.innerHTML = "<p>Enter custom weights for the criteria:</p>"
        for (i <- p.criteria.indices) 
            DomObjects.dialog_div_content.innerHTML += s"<p>${p.criteria(i)}: <input type='text' id='customweight${i}' value='${p.weights(i)}'></p>"
        
        for (i <- p.criteria.indices) 
            val input: HTMLInputElement = document.getElementById(s"customweight${i}").asInstanceOf[HTMLInputElement]
            input.onchange = (e: dom.Event) => 
                val dblvaloption = input.value.toDoubleOption
                val dblval = dblvaloption match 
                    case Some(dbl) => {
                        input.style.backgroundColor = "lemonchiffon"
                        dbl
                    }
                    case None => {
                        input.style.backgroundColor = "red"
                        0.0
                    }

        DomObjects.dialog_button_ok.onclick = (e: dom.MouseEvent) => 
            for (i <- p.criteria.indices) 
                val input = document.getElementById(s"customweight${i}").asInstanceOf[HTMLInputElement]
                p.weights(i) = input.value.toDouble
            dialog_messenger.close()
            val strproblem = HtmlUtils.decmat2html(problem)
            DomObjects.div_final_decmat.innerHTML = strproblem
        dialog_messenger.showModal()

