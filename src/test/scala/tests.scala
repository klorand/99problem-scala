import org.scalatest.prop.PropertyChecks
import org.scalatest.{ Matchers, PropSpec }


class P10Spec extends PropSpec
  with PropertyChecks
  with Matchers  {

  property("#rle works on sample input") {
    assert(P10.rle(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  property("#rle returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
        assert(P10.rle(a).map( _._1).sum === a.size)
    }
  }

}

class P11Spec extends PropSpec
  with PropertyChecks
  with Matchers  {

  property("#rleModified works on sample input") {
    assert(
      P11.rleModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).map(_.merge)
      === List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    )
  }

  property("#rleModified returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
      assert(P11.rleModified(a).map( {
        case Left((nr,elem)) => nr
        case Right(elem) => 1
      } ).sum === a.size)
    }
  }

}

class P12Spec extends PropSpec
  with PropertyChecks
  with Matchers  {

  property("#rleDecode works on sample input") {
    assert(
      P12.rleDecodeFlatMap(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
        === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    )
  }

  property("#rleDecode is the inverse of rle") {
    forAll { (a: List[String]) =>
      assert(P12.rleDecodeFlatMap(P10.rle(a)) === a)
    }
  }

}

class P13Spec extends PropSpec
  with PropertyChecks
  with Matchers  {

  property("#rle works on sample input") {
    assert(P13.rleDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
      === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  property("#rle returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
      assert(P13.rleDirect(a).map( _._1).sum === a.size)
    }
  }

}


