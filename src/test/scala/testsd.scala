import munit.Assertions as A

import org.expr.mcdm.Matrix
import org.expr.mcdm.Direction._
import org.expr.mcdm.critic
import org.expr.mcdm.sd

class TestSd extends munit.FunSuite {

  test("Sd Example - 1") {
    val mat = Array(
      Array(391152.0, 251165, 2063102, 912, 18784, 0.009, 0.049, 0.196),
      Array(181681.0, 118972, 1310114, 525, 12087, 0.009, 0.042, 0.157),
      Array(156478.0, 105801, 993245, 708, 12279, 0.01, 0.041, 0.177),
      Array(57145.0, 34707, 339417, 210, 3733, 0.013, 0.055, 0.268),
      Array(34947.0, 17568, 340159, 77, 2015, 0.014, 0.043, 0.204),
      Array(32667.0, 19308, 201372, 48, 1091, 0.008, 0.029, 0.217),
      Array(28945.0, 18033, 117762, 48, 886, 0.007, 0.021, 0.178),
      Array(18893.0, 13816, 139431, 35, 943, 0.01, 0.035, 0.213),
      Array(18191.0, 9088, 47664, 43, 731, 0.01, 0.021, 0.186),
      Array(12852.0, 4185, 64770, 3, 376, 0.011, 0.075, 0.285),
      Array(10878.0, 7107, 11200, 1, 78, 0.003, 0.033, 0.198),
      Array(4958.0, 1730, 4656, 7, 274, 0.017, 0.053, 0.215),
      Array(3901.0, 2318, 15598, 17, 357, 0.023, 0.001, 0.155),
      Array(2742.0, 1042, 52632, 1, 106, 0.022, 0.1, 0.384),
      Array(1734.0, 771, 1894, 1, 33, 0.011, 0.125, 0.709),
      Array(1677.0, 568, 1941, 1, 39, 0.011, 0.129, 0.633)
    )

    val directions = Array(
      Maximize,
      Maximize,
      Maximize,
      Minimize,
      Minimize,
      Minimize,
      Maximize,
      Maximize
    )

    val result = sd(mat, directions)

    val expectedWeights =
      Array(0.1161, 0.1175, 0.1254, 0.1376, 0.1334, 0.1136, 0.1251, 0.1308)

    A.assert(Matrix.elementwise_equal(result.weights, expectedWeights, 1e-03))
  }

}
