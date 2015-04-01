package google.cdpackage

object CdPacker {

  val MAX_FILE_COUNT = 2

  def pack(count: Int, size: Int, files: Int*): Int = {

    def findNextSuitableThing(max: Int, things: List[Int]): Option[(Int, List[Int])] = {
      def find1(max: Int, thing: Int, thingsBefore: List[Int], thingsAfter: List[Int]): Option[(Int, List[Int])] = {
        if(thing <= max) Some((thing, thingsBefore ++ thingsAfter))
        else if(thingsAfter.isEmpty) None
        else find1(max, thingsAfter.head, thingsBefore ++ List(thing), thingsAfter.tail)
      }
      if(things.isEmpty) None
      else find1(max, things.head, Nil, things.tail)

    }

    def pack1(size: Int, files: List[Int]): Int = {

      def fillDisk(currentCount: Int, remainingSize: Int, files: List[Int]): List[Int] = {
        val findResult = findNextSuitableThing(remainingSize, files)
        findResult match {
          case Some(thing) if currentCount < MAX_FILE_COUNT =>
            fillDisk(1 + currentCount, remainingSize - thing._1, thing._2)
          case _ => files
        }
      }

      def packFiles(size: Int, remainingFiles: List[Int], filledDiskCount: Int): Int = {
        remainingFiles.size match {
          case 0 => filledDiskCount
          case _ =>
            val fillResult = fillDisk(0, size, remainingFiles)
            packFiles(size, fillResult, filledDiskCount + 1)
        }
      }
      packFiles(size, files, 0)
    }
    pack1(size, files.toList.sorted.reverse)
  }
}
