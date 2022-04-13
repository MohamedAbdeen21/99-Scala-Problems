object Scala99 extends App {
  def P01[T](list: List[T]): T = {
    /* Find the last element of a list */
    list match {
      case h :: Nil => h
      case _ :: _ => P01(list.tail)
      case _ => throw new NoSuchElementException
    }
  }

  def P02[T](list: List[T]): T = {
    /* Find the second to last element of a list */
    list match {
      case h :: _ :: Nil => h
      case _ :: tail => P02(tail)
      case _ => throw new NoSuchElementException
    }
  }

  def P03[T](nth: Int, list: List[T]): T = {
    /* Find the nth element of a list */
    (nth, list) match {
      case (0, h :: _) => h
      case (_, _ :: tail) => P03(nth - 1, tail)
      case (_, Nil) => throw new IndexOutOfBoundsException
    }
  }

  def P04[T](list: List[T], size: Int = 0): Int = {
    /* Find the size/length of a list */
    // Added the size parameter to make the function tail recursive not just recursive
    list match {
      case Nil => size
      case _ :: tail => P04(tail, size + 1)
    }
  }

  def P05[T](list: List[T]): List[T] = {
    /* Reverse a list */
    list match {
      case Nil => Nil
      case h :: _ => P05(list.tail) ::: List(h)
    }
  }

  def P06[T](list: List[T]): Boolean = {
    /* Check if list is a palindrome */
    list.take(list.size / 2) == P05(list.takeRight(list.size / 2))
    // Or easily ```list == list.reverse```
  }

  def P07(list: List[Any]): List[Any] = {
    /* Flatten a nested lists structure */
    list.flatMap { element =>
      element match {
        case element: List[_] => P07(element)
        case _ => List(element)
      }
    }
  }

  def P08[T](list: List[T]): List[T] = {
    /* Eliminate consecutive duplicates of list elements */
    list match {
      case Nil => Nil
      case h :: tail => h :: P08(tail.dropWhile(_ == h))
    }
  }

  def P09[T](list: List[T]): List[Any] = {
    /* Pack consecutive duplicates of list elements into sub-lists. */
    list match {
      case Nil => Nil
      case _ => list.takeWhile(_ == list.head) :: P09(list.dropWhile(_ == list.head))
    }
  }

  def P10[T](list: List[T]): List[(Int, T)] = {
    /* Run-length encoding of a List */
    list match {
      case Nil => Nil
      case _ => (list.takeWhile(_ == list.head).size, list.head) :: P10(list.dropWhile(_ == list.head))
    }
  }

  def P11[T](list: List[T]): List[Either[T, (Int, T)]] = {
    /* Modified run-length encoding of a List */
    P10(list).map { e =>
      if (e(0) == 1) Left(e(1))
      else Right(e)
    }
  }

  def P12[T](list: List[(Int, T)]): List[T] = {
    /* Decode a run-length encoded List */
    list.flatMap(e => List.fill(e(0))(e(1)))
  }

  def P13[T](list: List[T]): List[(Int, T)] = P10(list)
  /* Same as P10 */

  def P14[T](list: List[T]): List[T] = {
    /* Duplicate the elements of a List */
    list.flatMap(e => List(e, e))
  }

  def P15[T](times: Int, list: List[T]): List[T] = {
    /* Duplicate the elements of a List n times */
    list.flatMap(List.fill(times)(_))
  }

  def P16[T](nth: Int, list: List[T]): List[T] = {
    /* Remove every nth element from a List. */
    list.filter(e => (list.indexOf(e) + 1) % nth != 0)
  }

  def P17[T](size: Int, list: List[T]): (List[T], List[T]) = {
    /* Split a List into two parts */
    list.span(list.indexOf(_) <= size - 1)
  }

  def P18[T](i: Int, k: Int, list: List[T]): List[T] = {
    /* Extract a slice from a List. */
    list.drop(i).take(k - i)
    // Scala compiler recommends replacing the .drop().take() chain with a slice(,)
  }
}