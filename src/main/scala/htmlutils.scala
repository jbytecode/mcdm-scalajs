package org.expr.mcdm

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement

object HtmlUtils:
    val document = dom.document 
    val window = dom.window

    def makeTable(a: Mat): dom.HTMLTableElement = 
        val table = document.createElement("table").asInstanceOf[dom.HTMLTableElement]
        a.foreach(row => 
            val tr = document.createElement("tr").asInstanceOf[dom.HTMLTableRowElement]
            row.foreach(cell => 
                val td = document.createElement("td").asInstanceOf[dom.HTMLTableCellElement]
                td.className = "matrixcell"
                td.textContent = cell.toString
                tr.appendChild(td)
            )
            table.appendChild(tr)
        )
        table

    def parseTextToTable(text: String): Mat = 
        text.split("\n").map(row => row.split(" ").map(cell => cell.toDouble))