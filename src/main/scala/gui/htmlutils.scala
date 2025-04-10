package org.expr.mcdm.gui

import org.scalajs.dom
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.HTMLDivElement
import org.expr.mcdm.*

object HtmlUtils:
    val document = dom.document 
    val window = dom.window

    def makeOutput(input: TopsisResult): HTMLDivElement = 
        val div = document.createElement("div").asInstanceOf[HTMLDivElement]
        val table1 = makeTable(input.normalizedMatrix)
        val table2 = makeTable(input.weightedNormalizedMatrix)
        val table3 = makeTable(Matrix.makeColumnMatrix(input.ideal))
        val table4 = makeTable(Matrix.makeColumnMatrix(input.antiIdeal))
        val table5 = makeTable(Matrix.makeRowMatrix(input.scores))
        div.appendChild(document.createElement("h2")).textContent = "Normalized matrix"
        div.appendChild(table1)
        div.appendChild(document.createElement("h2")).textContent = "Weighted normalized matrix"
        div.appendChild(table2)
        div.appendChild(document.createElement("h2")).textContent = "Ideal"
        div.appendChild(table3)
        div.appendChild(document.createElement("h2")).textContent = "Anti-ideal"
        div.appendChild(table4)
        div.appendChild(document.createElement("h2")).textContent = "Scores"
        div.appendChild(table5)
        div

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