import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import Arbitrary.arbitrary


class P10Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#rle works on sample input") {
    assert(P10.rle(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  property("#rle returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
      assert(P10.rle(a).map(_._1).sum === a.size)
    }
  }

}

class P11Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#rleModified works on sample input") {
    assert(
      P11.rleModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).map(_.merge)
        === List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    )
  }

  property("#rleModified returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
      assert(P11.rleModified(a).map({
        case Left((nr, elem)) => nr
        case Right(elem) => 1
      }).sum === a.size)
    }
  }

}

class P12Spec extends PropSpec
  with PropertyChecks
  with Matchers {

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
  with Matchers {

  property("#rle works on sample input") {
    assert(P13.rleDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  property("#rle returned list has basically the same elements as the original if you sum up the numbers") {
    forAll { (a: List[Int]) =>
      assert(P13.rleDirect(a).map(_._1).sum === a.size)
    }
  }

}


class P14Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#duplicate works on sample input") {
    assert(P14.duplicate(List('a, 'b, 'c, 'c, 'd))
      === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  property("#duplicate has twice the size") {
    forAll { (a: List[Int]) =>
      assert(P14.duplicate(a).size === a.size * 2)
    }
  }

}

class P15Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#duplicateN works on sample input") {
    assert(P15.duplicatenN_FlatMap(3, List('a, 'b, 'c, 'c, 'd))
      === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  property("#duplicateN has N times the size") {
    forAll { (a: List[Int]) =>
      assert(P15.duplicatenN_FlatMap(3, a).size === a.size * 3)
    }
  }

}

class P16Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#dropN works on sample input") {
    assert(P16.dropN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  property("#duplicateN has every N element missing in size") {
    forAll { (a: List[Int]) =>
      assert(P16.dropN(3, a).size === a.size - (a.size / 3))
    }
  }

}

class P17Spec extends PropSpec
  with PropertyChecks
  with Matchers {

  property("#split works on sample input") {
    assert(
      P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        ===(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }

  val listWithSize = for {
    l <- arbitrary[List[Int]]
    m <- Gen.choose(0, l.size - 1)
  } yield (l, m)

  property("#concatenate splits result in the original") {
    forAll(listWithSize) { (p) => {
      val splitRes = P17.split(p._2,p._1)
      assert(splitRes._1 ::: splitRes._2 === p._1)
    }
    }
  }
}





