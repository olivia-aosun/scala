package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = union(s3, union(s1, s2))  // 1, 2, 3
  val s5 = singletonSet(1000)
  val s6 = union(s4, s5)  // 1, 2, 3, 1000
  val s7 = union(s5, union(s1, s3))  // 1, 3, 1000
  printSet(s4)
  print(forall(s4, x => x < 10000))
  val s8 = map(s4, x => x + 5)
  printSet(s8)
}
