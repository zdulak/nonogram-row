import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks._

class NonogramRowSpec extends AnyFlatSpec with should.Matchers{
  behavior of "The nonogramRow method"

  it should "return empty list for sequences without ones" in {
    val args = Table(
      "binary sequence",
      List(),
      List(0,0,0,0,0)
      )

    forAll(args) {
      binarySeq => NonogramRow.nonogramRow(binarySeq) shouldBe Seq.empty[Int]
    }
  }

  it should "return an sequence of the lengths of the sets of consecutive 1's in the input sequence" in {
    val args = Table(
      ("binary sequence", "result"),
      (List(1,1,1,1,1), List(5)),
      (List(0,1,1,1,1,1,0,1,1,1,1), List(5, 4)),
      (List(1,1,0,1,0,0,1,1,1,0,0), List(2, 1, 3)),
      (List(0,0,0,0,1,1,0,0,1,0,1,1,1), List(2, 1, 3)),
      (List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1), List(1, 1, 1, 1, 1, 1, 1, 1))
      )

    forAll(args) {
      (binarySeq, result) => NonogramRow.nonogramRow(binarySeq) shouldBe result
    }
  }

}
