package objsets

/**Do not change signatures*/
trait PostSetInterface {
  def incl(post: Post): PostSet

  def remove(post: Post): PostSet

  def contains(post: Post): Boolean

  def foreach(f: Post => Unit): Unit

  def union(that: PostSet): PostSet

  def mostLiked: Post

  def descendingByLikes: PostList
}
