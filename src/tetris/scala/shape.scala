/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project tetris と入力し、return を押す
4. run と入力し、return を押す
5. コンパイルが成功したら、tetris.ShapeTest を選択（2 と入力）し、return を押す
6. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package tetris

import scala.collection.immutable.Range
import scala.util.Random
import scala.math.max

import sdraw._

// テトロミノを操作するための関数
object ShapeLib {
  // 色とブロックの表現
  type ColorSymbol = Char

  val blockSymbols = List('I', 'J', 'T', 'O', 'Z', 'L', 'S')
  val blockColors = {
    val n = blockSymbols.length
    for (i <- Range(0, n)) yield (HSB(360f * i / n, 0.3f, 1))
  }
  val colorSymbols = blockSymbols ++ List('G', 'g')
  val colors = blockColors ++ List(DarkGray, LightGray)
  val Color2Sym = colors.zip(colorSymbols).toList

  val Sym2Color: List[(ColorSymbol, Color)] =
    Color2Sym.map(cn => (cn._2, cn._1))

  // テトロミノの表現
  type Block = Color
  type Row = List[Block]
  type Shape = List[Row]
  type ShapeSpec = List[String]

  // テトロミノの表示（テスト用）
  def show(shape: Shape): Unit = println(showShape(shape))

  def showShape(shape: Shape): String = shape.map(showRow).mkString("\n")

  def showRow(row: Row): String = row.map(showBlock).mkString

  def showBlock(block: Block): Char = {
    Color2Sym.find(_._1 == block) match {
      case Some((_, sym)) => sym
      case None => '.'
    }
  }

  // テトロミノの定義
  val shapeSpecs: List[ShapeSpec] =
    List(
      List("I", "I", "I", "I"),
      List(" J", " J", "JJ"),
      List("TTT", " T "),
      List("OO", "OO"),
      List("ZZ ", " ZZ"),
      List("L ", "L ", "LL"),
      List(" SS", "SS "))

  def make(spec: ShapeSpec): Shape = {

    def color(c: ColorSymbol): Color =
      Sym2Color.find(p => p._1.equals(c)) match {
        case Some((_, c)) => c
        case _ => Transparent
      }

    spec.map((row: String) => row.toList.map(color))
  }

  // 7種類のテトロミノが入ったリスト
  val allShapes: List[Shape] = shapeSpecs.map(make)
  val List(shapeI, shapeJ, shapeT, shapeO, shapeZ, shapeL, shapeS) = allShapes

  // 次のテトロミノの選択
  val r = new Random()

  def random(): Shape = allShapes(r.nextInt(allShapes.length))

  // 1. duplicate
  // 目的：n 個の a からなるリストを作る
  // 定義
  def duplicate[A](n : Int, a: A): List[A] = {
    if (n <= 0) Nil
    else a :: duplicate(n-1, a)
  }


  // 2. empty
  // 目的：rows行 cols列の空の shape を作る
  // 定義
  def empty(rows: Int, cols: Int): Shape = {
    duplicate(rows, duplicate(cols, Transparent))
  }


  // 3. size
  // 目的：shape のサイズを（行数、列数）の形で返す

  // 補助関数：shape の列数の最大値
  def colsize(shape: Shape): Int = {
    shape match {
      case Nil => 0
      case x :: xs => max(x.length, colsize(xs))
    }
  }

  // 定義
  def size(shape: Shape): (Int, Int) = {
    (shape.length, colsize(shape))
  }


  // 4. blockCount
  // 目的：shape に含まれる空でないブロックの数を返す
  // 定義
  def blockCount(shape: Shape): Int = {
    shape match {
      case Nil => 0
      case x :: xs => {
        x match {
          case Nil => blockCount(xs)
          case Transparent :: ys => blockCount(ys :: xs)
          case y :: ys => 1 + blockCount(ys :: xs)
        }
      }
    }
  }


  // 5. wellStructured
  // 目的：shape がまっとうであるかを判断する
  // 定義
  def wellStructured(shape: Shape): Boolean = {
    if (shape.length != 0 && colsize(shape) != 0) {
      shape match {
        case Nil => false
        case x :: Nil => true
        case x :: xs => {
            if (wellStructured(xs) && x.length == colsize(xs)) true
              else false
            }
      }
    }
    else false
  }


  // 6. rotate
  // 目的：shapeを反時計回りに90度回転させる
  // 契約：引数のshapeはまっとうである
  // 定義
  def rotate(shape: Shape): Shape = {
    assert(wellStructured(shape) == true)

    // 補助関数：shapeの転置を取る
    def transpose(shape: Shape): Shape = {
      // 補助関数：tailの左側にheadを加える
      def cons_all(head: Row, tail: Shape): Shape = {
        head match {
          case Nil => tail
          case h :: hs => {
            tail match {
              case Nil => (h :: Nil) :: cons_all(hs, Nil)
              case t :: ts => (h :: t) :: cons_all(hs, ts)
            }
          }
        }
      }
      shape match {
        case Nil => Nil
        case x :: xs => cons_all(x, transpose(xs))
      }
    }

    // 補助関数：shapeのリストの中身をそれぞれ逆順にする
    def allreverse(shape: Shape): Shape = {
      shape match {
        case Nil => Nil
        case x :: xs => x.reverse :: allreverse(xs)
      }
    }

    transpose(allreverse(shape))
  }


  // 7. shiftSE
  // 目的：shapeを右にx、下にyずらす
  // 定義
  def shiftSE(shape: Shape, x: Int, y: Int): Shape = {

    // 補助関数：shapeを下にyずらす
    def shiftS(shape: Shape, y: Int): Shape = {
      if (y == 0) shape
      else fTransparent(Nil, colsize(shape)) :: shiftS(shape, y-1)
    }

    // 補助関数：Transparentをリストの前方にn回加える
    def fTransparent(list: Row, n: Int): Row = {
      if (n == 0) list
      else Transparent :: fTransparent(list, n-1)
    }

    if (x == 0) shiftS(shape, y)
    else {
      shiftS(shape, y) match {
        case Nil => Nil
        case head :: tl => fTransparent(head, x) :: shiftSE(tl, x, 0)
      } 
    }

  }


  // 8. shiftNW
  // 目的：shapeを左にx、上にyずらす
  // 定義
  def shiftNW(shape: Shape, x: Int, y: Int): Shape = {

    // 補助関数：shapeを上にyずらす
    def shiftN(shape: Shape, y: Int): Shape = {
      if (y == 0) shape
      else shiftN(shape, y-1) ++ List(bTransparent(Nil, colsize(shape)))
    }

    // 補助関数：Transparentをリストの後方にn回加える
    def bTransparent(list: Row, n: Int): Row = {
      if (n == 0) list
      else bTransparent(list, n-1) ++ List(Transparent)
    }

    if (x == 0) shiftN(shape, y)
    else {
      shiftN(shape, y) match {
        case Nil => Nil
        case head :: tl => bTransparent(head, x) :: shiftNW(tl, x, 0)
      }
    }
  }


  // 9. padTo
  // 目的：shapeをrows行cols列に拡大する
  // 契約：rows、colsはshapeの行数・列数以上
  def padTo(shape: Shape, rows: Int, cols: Int): Shape = {
    assert(rows >= shape.length && cols >= colsize(shape))
    shiftNW(shape, cols - colsize(shape), rows - shape.length)
  }


  // 10. overlap
  // 目的：２つのshapeが重なりを持つかを判断する
  // 定義
  def overlap(shape1: Shape, shape2: Shape): Boolean = {

    // 補助関数：２つのRowが重なりを持つか判断する
    def roverlap(row1: Row, row2: Row): Boolean = {
      (row1, row2) match {
        case (x :: xs, y :: ys) => {
          if (x != Transparent && y != Transparent) true
          else roverlap(xs, ys)
        }
        case _ => false
      }
    }

    (shape1, shape2) match {
      case (x :: xs, y :: ys) => {
        if (roverlap(x, y) == true) true
        else overlap(xs, ys)
      }
      case _ => false
    }
  }


  // 11. combine
  // 目的：２つのshapeを結合する
  // 契約：引数のshapeは重なりを持たない
  // 定義
  def combine(shape1: Shape, shape2: Shape): Shape = {
    assert(overlap(shape1, shape2) == false)
    val mrows = max(shape1.length, shape2.length)
    val mcols = max(colsize(shape1), colsize(shape2))
    val sshape2 = padTo(shape2, mrows, mcols)

    // 補助関数：２つのRowを結合する
    def rcombine(row1: Row, row2: Row): Row = {
      (row1, row2) match {
        case (x :: xs, y :: ys) => {
          if (x != Transparent) x :: rcombine(xs, ys)
          else if (y != Transparent) y :: rcombine(xs, ys)
          else Transparent :: rcombine(xs, ys)
        }
        case (Nil, y :: ys) => row2
        case (x :: xs, Nil) => row1
        case (Nil, Nil) => Nil
      }
    }

    (shape1, sshape2) match {
      case (x :: xs, y :: ys) => rcombine(x, y) :: combine(xs, ys)
      case (Nil, y :: ys) => sshape2
      case (x :: xs, Nil) => shape1
      case (Nil, Nil) => Nil
    }

  }


}
