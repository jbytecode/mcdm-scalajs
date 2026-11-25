import munit.Assertions as A

import org.expr.mcdm.parser.Parser
import org.expr.mcdm.MCDMProblem
import org.expr.mcdm.Direction.{Maximize, Minimize}
import org.expr.mcdm.Matrix

class TestParser extends munit.FunSuite {
  test("Parser Example - 1") {
    val input = 
        """
                    Criteria 1, Criteria 2, Criteria 3, Criteria 4
        Alternative 1, 0.5, 0.3, 0.2, 0.4
        Alternative 2, 0.6, 0.7, 0.8, 0.9
        Alternative 3, 0.1, 0.2, 0.3, 0.4
        Alternative 4, 0.5, 0.6, 0.7, 0.8
        """
    val parsed = Parser.parseCSV(input, ",")

    val expected_alternatives = Array("Alternative 1", "Alternative 2", "Alternative 3", "Alternative 4")

    val expected_criteria = Array("Criteria 1", "Criteria 2", "Criteria 3", "Criteria 4")

    val expected_data = Array(
      Array(0.5, 0.3, 0.2, 0.4),
      Array(0.6, 0.7, 0.8, 0.9),
      Array(0.1, 0.2, 0.3, 0.4),
      Array(0.5, 0.6, 0.7, 0.8)
    )

    parsed.alternatives.zip(expected_alternatives).foreach { case (a, e) =>
      A.assertEquals(a, e)
    }

    parsed.criteria.zip(expected_criteria).foreach { case (c, e) =>
      A.assertEquals(c, e)
    }

    A.assert(Matrix.elementwise_equal(parsed.data, expected_data, 1e-05), 
    "Parsed data does not match expected values")

    parsed.weights.foreach { w =>
      A.assertEquals(w, 0.25)
    }

    parsed.directions.foreach { d =>
      A.assertEquals(d, "max")
    }

  }

  test("validateCSV rejects a non-numeric cell and names it") {
    val input = "C1,C2\nA1,1.0,2.0\nA2,1.0,abc"
    val r = Parser.validateCSV(input, ",")
    A.assert(r.isLeft)
    A.assert(r.left.toOption.get.contains("abc"))
    A.assert(r.left.toOption.get.contains("A2"))
  }

  test("validateCSV rejects a comma used as decimal separator") {
    val input = "C1;C2\nA1;1,5;2.0\nA2;1.0;3.0"
    A.assert(Parser.validateCSV(input, ";").isLeft)
  }

  test("validateCSV rejects rows with a wrong number of values") {
    val input = "C1,C2,C3\nA1,1.0,2.0,3.0\nA2,1.0,2.0"
    val r = Parser.validateCSV(input, ",")
    A.assert(r.isLeft)
    A.assert(r.left.toOption.get.contains("2 values found"))
  }

  test("validateCSV rejects empty input and a header without data") {
    A.assert(Parser.validateCSV("", ",").isLeft)
    A.assert(Parser.validateCSV("   \n  ", ",").isLeft)
    A.assert(Parser.validateCSV("C1,C2", ",").isLeft)
  }

  test("validateCSV accepts a leading comma, semicolons and Windows line endings") {
    val r1 = Parser.validateCSV(",C1,C2\r\nA1,1,2\r\nA2,3,4\r\n", ",")
    A.assert(r1.isRight)
    A.assertEquals(r1.toOption.get.criteria.toList, List("C1", "C2"))
    A.assert(Matrix.elementwise_equal(r1.toOption.get.data, Array(Array(1.0, 2.0), Array(3.0, 4.0)), 1e-12))
    val r2 = Parser.validateCSV("C1;C2\nA1;1;2", ";")
    A.assert(r2.isRight)
  }

  test("validateDirections accepts min and max only, one per criterion") {
    A.assertEquals(Parser.validateDirections("max, MIN ,max", 3).toOption.get.toList, List("max", "min", "max"))
    A.assert(Parser.validateDirections("max,mni,max", 3).left.toOption.get.contains("mni"))
    A.assert(Parser.validateDirections("max,min", 3).isLeft)
  }
}