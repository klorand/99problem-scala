import scala.annotation.tailrec

object P01 {
  @tailrec
  def last[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }
}

object P02 {
  @tailrec
  def penultimate[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case x :: xs => penultimate(xs)
  }


  def stripLast[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: _ :: Nil => List(x)
    case x :: xs => x :: stripLast(xs)
  }

  def penultimate2[A](l: List[A]): Option[A] = P01.last(stripLast(l))

}

object P03 {

  @tailrec
  def nth[A](n: Int, l: List[A]): Option[A] = (n, l) match {
    case (0, x :: _) => Some(x)
    case (k, _ :: xs) if k > 0 => nth(k - 1, xs)
    case _ => None
  }

}

object P04 {


  def length[A](l: List[A]): Int = l match {
    case _ :: xs => 1 + length(xs)
    case Nil => 0
  }

  def lengthTailRec[A](l: List[A]): Int = {
    @tailrec
    def lengthRec(l: List[A], current: Int): Int = l match {
      case x :: xs => lengthRec(xs, current + 1)
      case Nil => current
    }
    lengthRec(l, 0)
  }

  def lenghtFunc[A](l:List[A]):Int = l.foldLeft(0){ (acc, elem) => acc + 1 }

}

object P05 {


  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  def reverseFunc[A](l: List[A]): List[A] = l.foldLeft[List[A]](Nil){(sum, current) => current :: sum}

  def reverseTailrec[A](l: List[A]): List[A] = {
    @tailrec
    def reverseTailrecInner(l: List[A], ret: List[A]): List[A] = l match {
      case Nil => ret
      case x :: xs => reverseTailrecInner(xs, x :: ret)
    }
    reverseTailrecInner(l, Nil)
  }


}

object P06 {

  def isPalindrome[A](l: List[A]): Boolean = l == P05.reverseFunc(l)

  @tailrec
  def isPalindromeWeirdAndSlow[A](l:List[A]): Boolean = l match {
    case Nil => true
    case x :: xs => P04.length(xs)==0 || (P01.last(xs).contains(x) &&  isPalindromeWeirdAndSlow(P02.stripLast(xs)))
  }
}

object P07 {

  def flatten(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case (x:List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case x :: xs => x :: flatten(xs)
  }

  def flattenFold(l: List[Any]):List[Any] = l.foldLeft[List[Any]](Nil)( (it,current) => current match {
    case Nil => it
    case x:List[Any] => flattenFold(x) ::: it
    case x => x :: it
  } )


}

object P08 {

  def compress[A](l:List[A]): List[A] = l match  {
    case x :: y :: xs if x == y => compress(y :: xs)
    case x :: xs => x :: compress(xs)
    case Nil => Nil
  }
}

object P09 {

  def pack[A](l:List[A]): List[List[A]] = l match  {
    case x :: y :: xs if x == y => {
      val rest =  pack(y :: xs)
      (x :: rest.head) :: rest.tail
    }
    case x :: xs => List(x) :: pack(xs)
    case Nil => Nil
  }
}

object P10 {
  def rle[A](l: List[A]): List[(Int,A)] = P09.pack(l).map( l=> (l.size, l.head))
}



