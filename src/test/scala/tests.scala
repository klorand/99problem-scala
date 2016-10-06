import org.scalatest.prop.PropertyChecks
import org.scalatest.{ Matchers, PropSpec }


class P10Spec extends PropSpec
  with PropertyChecks
  with Matchers  {

  property("#rle works on sample input") {
    assert(P10.rle(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

}
