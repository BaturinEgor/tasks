import scala.annotation.tailrec
import scala.runtime.Nothing$

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }

  def compress(list: List[Symbol]): List[Symbol] = {
    list match {
      case Nil => return Nil
      case _ => {
        list.size match {
          case 1 => return list
          case _ => {
            val result: List[Symbol] = compress(list.slice(1, list.size))
            if (list.head == result.head) result else list.head :: result
          }
        }
      }
    }
  }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    list match {
      case Nil => return Nil
      case _ => {
        list.size match {
          case 1 => return List((1, list.head))
          case _ => {
            val result: List[(Int, Symbol)] = encode(list.slice(1, list.size))
            if (list.head == result.head._2) {
              val tuple: (Int, Symbol) = (result.head._1 + 1, result.head._2)
              tuple :: result.slice(1, result.size)
            } else {
              val tuple: (Int, Symbol) = (1, list.head)
              tuple :: result
            }
          }
        }
      }
    }
  }

  def pack(list: List[Symbol]): List[List[Symbol]] = {
    list match {
      case Nil => return Nil
      case _ => {
        list.size match {
          case 1 => return List(list)
          case _ => {
            val result: List[List[Symbol]] = pack(list.slice(1, list.size))
            if (list.head == result.head.head) {
              (list.head :: result.head) :: result.slice(1, result.size)
            } else {
              List(list.head) :: result
            }
          }
        }
      }
    }
  }

  def encodeModified(list: List[Symbol]): List[Any] = {
    list match {
      case Nil => return Nil
      case _ => {
        list.size match {
          case 1 => return list
          case _ => {
            val result: List[Any] = encodeModified(list.slice(1, list.size))
            if (result.head.isInstanceOf[Tuple2[Int, Symbol]]) {
              val resultHead: Tuple2[Int, Symbol] = result  .head.asInstanceOf[Tuple2[Int, Symbol]]
              if (list.head == resultHead._2) {
                val tuple: (Int, Symbol) = (resultHead._1 + 1, resultHead._2)
                tuple :: result.slice(1, result.size)
              } else {
                list.head :: result
              }
            } else {
              if (list.head == result.head) {
                val tuple: (Int, Symbol) = (2, list.head)
                tuple :: result.slice(1, result.size)
              } else {
                list.head :: result
              }
            }
          }
        }
      }
    }
  }
}