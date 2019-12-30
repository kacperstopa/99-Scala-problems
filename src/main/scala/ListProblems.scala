import scala.util.{Random, Try}

object ListProblems extends App {
  //P01 Find the last element of a list.
  def last[A](list: List[A]): Option[A] = {
    list.lastOption
  }

  //P02 Find the last but one element of a list.
  def penultimate[A](list: List[A]): Option[A] = {
    list.reverse.tail.headOption
  }

  //P03 Find the Kth element of a list.
  def nth[A](position: Int, list: List[A]): Option[A] = {
    Try(list.apply(position)).toOption
  }

  //P04 Find the number of elements of a list.
  def length(list: List[_]): Int = {
    list.length
  }

  //P05 Reverse a list.
  def reverse[A](list: List[A]): List[A] = {
    list.reverse
  }

  //P06 Find out whether a list is a palindrome.
  def isPalindrome[A](list: List[A]): Boolean = {
    list.reverse == list
  }

  //P07 Flatten a nested list structure.
  def flatten[A](list: List[Any]): List[A] = {
    list.flatMap {
      case list: List[A] => flatten(list)
      case a: A => List(a)
    }
  }

  //P08 Eliminate consecutive duplicates of list elements.
  def compress[A](list: List[A]): List[A] = {
    list.foldLeft[List[A]](List.empty) { (list, element) =>
      if(list.lastOption.contains(element)) {
        list
      } else {
        list :+ element
      }
    }
  }

  //P09 Pack consecutive duplicates of list elements into sublists.
  def pack[A](list: List[A]): List[List[A]] = {
    list.foldLeft[List[List[A]]](List.empty) {(list, element) =>
      if(list.lastOption.flatMap(_.headOption).contains(element)) {
        list.init :+ (list.last :+ element)
      } else {
        list :+ List(element)
      }
    }
  }

  //P10 Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] = {
    pack(list).map(list => (list.length, list.head))
  }

  //P11 Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[Any] = {
    pack(list).map {
      case List(a) => a
      case list => (list.length, list.head)
    }
  }

  //P12 Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]): List[A] = {
    list.flatMap { case (length, a) => List.fill(length)(a)}
  }

  //P13 Run-length encoding of a list (direct solution).
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    list.foldLeft[List[(Int, A)]](List.empty) {(list, element) =>
      if(list.lastOption.exists(_._2 == element)) {
        list.init :+ (list.last._1 + 1, element)
      } else {
        list :+ (1, element)
      }
    }
  }


  //P14 Duplicate the elements of a list.
  def duplicate[A](list: List[A]): List[A] = {
    list.flatMap(List.fill(2)(_))
  }

  //P15 Duplicate the elements of a list a given number of times.
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    list.flatMap(List.fill(n)(_))
  }

  //P16 Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A]): List[A] = {
    list.grouped(n).flatMap(_.take(n-1)).toList
  }

  //P17 Split a list into two parts.
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = {
    list.splitAt(n)
  }

  //P18 Extract a slice from a list.
  def slice[A](from: Int, to: Int, list: List[A]): List[A] = {
    list.slice(from, to)
  }

  //P19 Rotate a list N places to the left.
  def rotate[A](n: Int, list: List[A]): List[A] = {
      list.takeRight(list.length - ((n % list.length) + list.length) % list.length) ++ list.take(((n % list.length) + list.length) % list.length)
  }

  //P20 Remove the Kth element from a list.
  def removeAt[A](i: Int, list: List[A]): (List[A], A) = {
    (list.patch(i, Nil, 1), list(i))
  }

  //P21 Insert an element at a given position into a list.
  def insertAt[A](element: A, i: Int, list: List[A]): List[A] = {
    list.patch(i, List(element), 0)
  }

  //P22 Create a list containing all integers within a given range.
  def range(from: Int, to: Int): List[Int] = {
    (from to to).toList
  }

  //P23 Extract a given number of randomly selected elements from a list.
  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    Random.shuffle(list).take(n)
  }

  //P24 Lotto: Draw N different random numbers from the set 1..M.
  def lotto(n: Int, to: Int): List[Int] = {
    Random.shuffle((1 to to).toList).take(n)
  }

  //P25 Generate a random permutation of the elements of a list.
  def randomPermute[A](list: List[A]): List[A] = {
    Random.shuffle(list)
  }

  //P26 Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[A](n: Int, list: List[A]): List[List[A]] = {
    list.toSet.subsets(n).map(_.toList).toList
  }

  //P27 Group the elements of a set into disjoint subsets.
  def group[A](groupSizes: List[Int], list: List[A]): List[List[List[A]]] = {
    groupSizes match {
      case Nil => List(Nil)
      case size :: sizes => combinations(size, list).flatMap { c =>
        group(sizes, list.filterNot(c.contains)) map {c :: _}
      }
    }
  }

  //P28 Sorting a list of lists according to length of sublists.
  //a) We suppose that a list contains elements that are lists themselves.
  //   The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
  def lsort[A](list: List[List[A]]): List[List[A]] = {
    list.sortBy(_.length)
  }

  //b) Again, we suppose that a list contains elements that are lists themselves.
  // But this time the objective is to sort the elements according to their length frequency; i.e. in the default,
  // sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
  def lsortFreq[A](list: List[List[A]]): List[List[A]] = {
    list.sortBy(l => list.count(_.size == l.size) )
  }
}