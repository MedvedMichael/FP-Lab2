package objsets

import java.lang.StringBuffer

object PostReader {
  object Parseposts {
    def regexParser(s: String): List[Map[String, Any]] = {

      // In real life. you would use an actual JSON library...
      val postRegex = """^\{ .*"user": "([^"]+)", "text": "([^"]+)", "likes": ([\\.0-9]+) \},?$""".r
      s.split("\r?\n").toList.tail.init.map {
        case postRegex(user, text, likes) => Map("user" -> user, "text" -> text, "likes" -> likes.toDouble)
      }
    }

    def getposts(user: String, json: String): List[Post] = {
      for {
        map <- regexParser(json)}
      yield {
        val text = map("text")
        val likes = map("like_count")
        new Post(user, text.toString, likes.toString.toDouble.toInt)
      }
    }

    def getPostData(user: String, json: String): List[Post] = {
      // is list
      val l = regexParser(json)
      for {
        map <- l
      } yield {
        val text = map("text")
        val likes = map("likes")
        new Post(user, text.toString, likes.toString.toDouble.toInt)
      }
    }
  }

  def topostset(l: List[Post]): PostSet =
    l.foldLeft(new Empty(): PostSet)(_.incl(_))

  def unparseToData(tws: List[Post]): String = {
    val buf = new StringBuffer()
    tws.foreach { tw =>
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
        tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"likes\": " +
        tw.likes + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  private val gizmodoposts = PostReader.Parseposts.getPostData("gizmodo", PostData.gizmodo)
  private val techCrunchposts = PostReader.Parseposts.getPostData("TechCrunch", PostData.TechCrunch)
  private val engadgetposts = PostReader.Parseposts.getPostData("engadget", PostData.engadget)
  private val amazondealsposts = PostReader.Parseposts.getPostData("amazondeals", PostData.amazondeals)
  private val cnetposts = PostReader.Parseposts.getPostData("CNET", PostData.CNET)
  private val gadgetlabposts = PostReader.Parseposts.getPostData("gadgetlab", PostData.gadgetlab)
  private val mashableposts = PostReader.Parseposts.getPostData("mashable", PostData.mashable)

  private val sources = List(gizmodoposts, techCrunchposts, engadgetposts, amazondealsposts, cnetposts, gadgetlabposts, mashableposts)

  val postMap: Map[String, List[Post]] =
    Map() ++ Seq((sites(0) -> gizmodoposts),
      (sites(1) -> techCrunchposts),
      (sites(2) -> engadgetposts),
      (sites(3) -> amazondealsposts),
      (sites(4) -> cnetposts),
      (sites(5) -> gadgetlabposts),
      (sites(6) -> mashableposts))

  val postsets: List[PostSet] = sources.map(posts => topostset(posts))

  private def unionOfAllpostsets(curSets: List[PostSet], acc: PostSet): PostSet =
    if (curSets.isEmpty)
      acc
    else
      unionOfAllpostsets(curSets.tail, acc.union(curSets.head))

  val allposts: PostSet = unionOfAllpostsets(postsets, new Empty())
}
