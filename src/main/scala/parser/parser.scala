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
    /*
    Shows a message and returns an empty problem when the input is not valid,
    so that the caller can keep the previous problem.
     */
    def parseCSV(input: String, separator: String = ","): MCDMProblem =
        validateCSV(input, separator) match
            case Right(problem) => problem
            case Left(message) =>
                msgbox(message)
                empty_mcdm_problem()

    /*
    Pure version of parseCSV: returns either an error message or the problem.
    Every cell must be a finite number and every row must have one value per criterion.
     */
    def validateCSV(input: String, separator: String = ","): Either[String, MCDMProblem] =
        if input == null || input.trim.isEmpty then
            return Left("The input is empty.")
        val lines = input.split("\n").map(_.trim).filter(_.nonEmpty)
        if lines.length < 2 then
            return Left("The input needs a header row with the criteria names and at least one row of data.")
        val criteria = lines.head.split(separator).map(_.trim).filter(_.nonEmpty)
        if criteria.isEmpty then
            return Left("The first row must contain the criteria names.")
        val rows = lines.tail.map(_.split(separator).map(_.trim))
        val alternatives = rows.map(_.head)
        def isFinite(cell: String): Boolean =
            cell.toDoubleOption.exists(x => !x.isNaN && !x.isInfinite)
        def rowError(row: Array[String], i: Int): Option[String] =
            if row.head.isEmpty then
                Some(s"Row ${i + 2}: the alternative name is missing.")
            else if row.length - 1 != criteria.length then
                Some(s"Row ${i + 2} (${row.head}): ${row.length - 1} values found, but there are ${criteria.length} criteria.")
            else
                row.tail.zipWithIndex.find((cell, _) => !isFinite(cell)).map((cell, j) =>
                    s"Row ${i + 2} (${row.head}), criterion ${criteria(j)}: '$cell' is not a number. Use a dot (.) as the decimal separator.")
        rows.zipWithIndex.iterator.map(rowError.tupled).collectFirst { case Some(m) => m } match
            case Some(message) => Left(message)
            case None =>
                val data = rows.map(_.tail.map(_.toDouble))
                val weights = Array.fill(criteria.length)(1.0 / criteria.length)
                val directions = Array.fill(criteria.length)("max")
                Right(MCDMProblem(alternatives, criteria, data, weights, directions))

    /*
    Parses a comma separated list of directions; every entry must be "min" or "max".
     */
    def validateDirections(input: String, ncriteria: Int): Either[String, Array[String]] =
        val tokens = input.split(",").map(_.trim.toLowerCase).filter(_.nonEmpty)
        val invalid = tokens.filterNot(x => x == "min" || x == "max")
        if invalid.nonEmpty then
            Left(s"Invalid direction(s): ${invalid.mkString(", ")}. Use only min or max.")
        else if tokens.length != ncriteria then
            Left(s"Invalid number of directions. ${ncriteria} criteria found, ${tokens.length} directions found.")
        else
            Right(tokens)

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