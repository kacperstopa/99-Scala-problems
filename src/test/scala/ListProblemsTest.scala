import ListProblems.{drop, last, pack}
import ListProblems._
import org.scalatest.funsuite.AnyFunSuite

class ListProblemsTest extends AnyFunSuite {
  test("P1") {
    assert(last(List(1, 1, 2, 3, 5, 8)).contains(8))
  }

  test("P2") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)).contains(5))
  }

  test("P3") {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)).contains(2))
  }

  test("P4") {
    assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  }

  test("P5") {
    assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  test("P6") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("P7") {
    assert(
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5,
        8)
    )
  }

  test("P8") {
    assert(
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(
          'a, 'b, 'c, 'a, 'd, 'e)
    )
  }

  test("P9") {
    assert(
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(
        List('a, 'a, 'a, 'a),
        List('b),
        List('c, 'c),
        List('a, 'a),
        List('d),
        List('e, 'e, 'e, 'e)
      )
    )
  }

  test("P10") {
    assert(
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(
        (4, 'a),
        (1, 'b),
        (2, 'c),
        (2, 'a),
        (1, 'd),
        (4, 'e)
      )
    )
  }

  test("P11") {
    assert(
      encodeModified(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    )
  }

  test("P12") {
    assert(
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List(
          'a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    )
  }

  test("P13") {
    assert(
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(
        (4, 'a),
        (1, 'b),
        (2, 'c),
        (2, 'a),
        (1, 'd),
        (4, 'e)
      )
    )
  }

  test("P14") {
    assert(
      duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c,
        'c, 'd, 'd)
    )
  }

  test("P15") {
    assert(
      duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b,
        'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    )
  }

  test("P16") {
    assert(
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b,
        'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  test("P17") {
    assert(
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List(
        'a,
        'b,
        'c
      ), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }

  test("P18") {
    assert(
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List(
        'd,
        'e,
        'f,
        'g
      )
    )
  }

  test("P19") {
    assert(
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d,
        'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    )
    assert(
      rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j,
        'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    )
  }

  test("P20") {
    assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
  }

  test("P21") {
    assert(
      insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd)
    )
  }

  test("P22") {
    assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
  }

  test("P26") {
    assert(
      combinations(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l)).length == 220
    )
  }

  test("P27") {
    assert(
      lsort(
        List(
          List('a, 'b, 'c),
          List('d, 'e),
          List('f, 'g, 'h),
          List('d, 'e),
          List('i, 'j, 'k, 'l),
          List('m, 'n),
          List('o)
        )
      ) == List(
        List('o),
        List('d, 'e),
        List('d, 'e),
        List('m, 'n),
        List('a, 'b, 'c),
        List('f, 'g, 'h),
        List('i, 'j, 'k, 'l)
      )
    )
  }

  test("P28") {
    assert(
      lsortFreq(
        List(
          List('a, 'b, 'c),
          List('d, 'e),
          List('f, 'g, 'h),
          List('d, 'e),
          List('i, 'j, 'k, 'l),
          List('m, 'n),
          List('o)
        )
      ) == List(
        List('i, 'j, 'k, 'l),
        List('o),
        List('a, 'b, 'c),
        List('f, 'g, 'h),
        List('d, 'e),
        List('d, 'e),
        List('m, 'n)
      )
    )
  }
}
