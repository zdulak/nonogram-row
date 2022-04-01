object NonogramRow extends App {
  println(nonogramRow(List()))
  println(nonogramRow(List(0,0,0,0,0)))
  println(nonogramRow(List(1,1,1,1,1)))
  println(nonogramRow(List(0,1,1,1,1,1,0,1,1,1,1)))
  println(nonogramRow(List(1,1,0,1,0,0,1,1,1,0,0)))
  println(nonogramRow(List(0,0,0,0,1,1,0,0,1,0,1,1,1)))
  println(nonogramRow(List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)))

  def nonogramRow(binarySeq: Seq[Int]): Seq[Int] = {
    (binarySeq :+ 0).foldLeft((List.empty[Int], 0)) {
      case ((result, counter), elem) =>
        if (elem == 1) (result, counter + 1)
        else if (elem == 0 && counter != 0) (result :+ counter, 0)
        else (result, 0)
    }._1
  }
}
