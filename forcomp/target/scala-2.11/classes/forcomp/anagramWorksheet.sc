def wordOccurrences(w: String): List[(Char, Int)] = w.toLowerCase.groupBy(char => char).map(ltr => (ltr._1, ltr._2.size)).toList

val y = wordOccurrences("hell")
val x = wordOccurrences("helloworld")
val xn = wordOccurrences("helloworld").toMap

y.foldLeft(x.toMap)({ case (map, (char, count)) => {
  val newCount = map(char) - count
  if (newCount == 0) map - char
  else map updated(char, newCount)
  }
}).toList.sorted
//
//
//{ case (map, (char, count)) =>

//  }


