package objsets


class PostSetSuite extends munit.FunSuite {

  val set1 = new Empty()
  val set2: PostSet = set1.incl(new Post("a", "a body", 20))
  val set3: PostSet = set2.incl(new Post("b", "b body", 20))
  val c = new Post("c", "c body", 7)
  val d = new Post("d", "d body", 9)
  val set4c: PostSet = set3.incl(c)
  val set4d: PostSet = set3.incl(d)
  val set5: PostSet = set4c.incl(d)

  def asSet(posts: PostSet): Set[Post] = {
    var res = Set[Post]()
    posts.foreach(res += _)
    res
  }

  def size(set: PostSet): Int = asSet(set).size



  test("filter: on empty set") {
    assertEquals(size(set1.filter(tw => tw.user == "a")), 0)
  }

  test("filter: a on set5") {
    assertEquals(size(set5.filter(tw => tw.user == "a")), 1)
  }

  test("filter: twenty on set5") {
    assertEquals(size(set5.filter(tw => tw.likes == 20)), 2)
  }

  test("union: set4c and set4d") {
    assertEquals(size(set4c.union(set4d)), 4)
  }

  test("union: with empty set1") {
    assertEquals(size(set5.union(set1)), 4)
  }

  test("union: with empty set2") {
    assertEquals(size(set1.union(set5)), 4)
  }

  test("descending: set5") {
    val trends = set5.descendingByLikes
    assert(!trends.isEmpty)
    assert(trends.head.user == "a" || trends.head.user == "b")
  }

  test("mostLiked: with empty set1") {
    try {
      set1.mostLiked
      assert(cond = false)
    }
    catch {
      case _: java.util.NoSuchElementException => assert(cond = true)
    }
  }

  import scala.concurrent.duration._

  override val munitTimeout: FiniteDuration = 1.seconds

}
