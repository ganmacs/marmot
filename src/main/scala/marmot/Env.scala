package marmot

object Env {
  def empty[T] = new Env[T]
  def build[T](parent: Env[T], t: Tuple2[String, T]) =
    new Env[T](Some(parent), Map(t._1 ->  t._2))
}

case class Env[T](
  var parent: Option[Env[T]] = None,
  var e: Map[String, T] = Map.empty[String, T]
) {
  def apply(key: String) = e.apply(key)
  def put(k: String, v: T) = { e = Map(k -> v) ++ e; this }
  def getLocal(k: String) = e.get(k)
  def get(k: String): Option[T] = e.get(k) match {
    case None => parent match {
      case None => None
      case Some(m) => m.get(k)
    }
    case x => x
  }
  def updateWithPairs(keys: List[String], values: List[T]) = {
    val ne = keys.zip(values).toMap
    e ++= ne
    this
  }
}
