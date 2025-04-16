package org.expr.mcdm.parser

import org.expr.mcdm.MCDMProblem
import org.expr.mcdm.Direction
import org.expr.mcdm.gui.HtmlUtils.msgbox

object Parser:

    /* 
    Data model is like 

        ,Criteria 1, Criteria 2, Criteria 3, Criteria 4
        Alternative 1, 0.5, 0.3, 0.2, 0.4
        Alternative 2, 0.6, 0.7, 0.8, 0.9
        Alternative 3, 0.1, 0.2, 0.3, 0.4
        Alternative 4, 0.5, 0.6, 0.7, 0.8

        as well as 

        Criteria 1, Criteria 2, Criteria 3, Criteria 4
        Alternative 1, 0.5, 0.3, 0.2, 0.4
        Alternative 2, 0.6, 0.7, 0.8, 0.9
        Alternative 3, 0.1, 0.2, 0.3, 0.4
        Alternative 4, 0.5, 0.6, 0.7, 0.8

        the first row represents the criteria and the first column represents the alternatives names.
        the first row may and may not include a comma at the front of the first criteria name.
        commas can be replaced with semicolons.
     */
    def parseCSV(input: String, separator: String = ","): MCDMProblem =
        if input == null || input.isEmpty then
            msgbox("The input is empty.")
            empty_mcdm_problem()
        val lines = input.split("\n").map(_.trim).filter(_.nonEmpty)
        if lines.length < 2 then
            msgbox("The input is not valid.")
            empty_mcdm_problem()
        val criteria = lines.head.split(separator).map(_.trim).filter(_.nonEmpty)
        val alternatives = lines.tail.map(_.split(separator).head.trim).filter(_.nonEmpty)
        val data = lines.tail.map(_.split(separator).tail.map(_.trim.toDouble)).map(_.toList.toArray).toArray
        val weights = Array.fill(criteria.length)(1.0 / criteria.length)
        val directions = Array.fill(criteria.length)("max")
        if criteria.length != data.head.length then
            msgbox("The number of criteria and the number of data columns do not match.")
        if alternatives.length != data.length then
            msgbox("The number of alternatives and the number of data rows do not match.")
        if data.length == 0 then
            msgbox("The data is empty.")
        MCDMProblem(alternatives, criteria, data, weights, directions)

    def empty_mcdm_problem(): MCDMProblem =
        MCDMProblem(
            Array[String](),
            Array[String](),
            Array[Array[Double]](),
            Array[Double](),
            Array[String]()
        )

    def str2directions(arr: Array[String]): Array[Direction] =
        arr.map(_.toLowerCase).map {
            case "min" => Direction.Minimize
            case "max" => Direction.Maximize
            case _ => Direction.Maximize
        }