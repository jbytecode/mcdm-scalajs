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
}