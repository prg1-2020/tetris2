/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project tetris と入力し、return を押す
4. run と入力し、return を押す
5. コンパイルが成功したら、tetris.A を選択（1 と入力）し、return を押す
6. ゲーム画面を閉じたら、手動で java を終了する
7. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package tetris

import scala.util.Random

import sgeometry.Pos
import sdraw.{World, Color, Transparent, HSB}

import tetris.{ShapeLib => S}

// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape, count: Int, hold: S.Shape, minos: List[S.Shape], next: S.Shape) extends World() {

  // マウスクリックは無視
  def click(p: sgeometry.Pos): World = this

  // ブロックの描画
  def drawRect(x: Int, y: Int, w: Int, h: Int, c: Color): Boolean = {
    canvas.drawRect(Pos(A.BlockSize * x, A.BlockSize * y), A.BlockSize * w, A.BlockSize * h, c)
  }

  // shape の描画（与えられた位置）
  def drawShape(pos: (Int, Int), shape: S.Shape): Boolean = {
    val pos_colors = shape.zipWithIndex.flatMap(row_i => {
      val (row, i) = row_i
      row.zipWithIndex.map(box_j => {
        val (color, j) = box_j
        (j, i, color)
      })
    })

    val (x, y) = pos
    pos_colors.forall(pos_color => {
      val (dx, dy, color) = pos_color
      drawRect(x + dx, y + dy, 1, 1, color)
    })
  }

  // shape の描画（原点）
  def drawShape00(shape: S.Shape): Boolean = drawShape((0, 0), shape)

  // ゲーム画面の描画
  val CanvasColor = HSB(0, 0, 0.1f)
  val BorderColor = HSB(0, 0, 1.0f)

  def draw(): Boolean = {
    val (pos, shape) = piece
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    canvas.drawRect(Pos(canvas.width * 10/16, 0), canvas.width* 1/16 , canvas.height, BorderColor) &&
    canvas.drawRect(Pos(canvas.width * 10/16, canvas.height * 7/15), canvas.width * 6/16 , canvas.height * 1/15, BorderColor) &&
    drawShape00(pile) &&
    drawShape(pos, shape)
    val a = S.maxCols(hold)
    drawShape((13 - a/2, 10), hold) // 機能その５
    val b = S.maxCols(next)
    drawShape((13 - b/2, 2), next) // 機能その６
  }

  // 機能７
  // 8-3. newminos
  // 目的：新しいminosを生成する
  def newminos(m: List[S.Shape]): List[S.Shape] = {
    if (m == Nil) {
      scala.util.Random.shuffle(S.allShapes)
    } else {
      scala.util.Random.shuffle(m)
    }
  }

  // 1, 4, 7. tick
  def tick(): World = {
    val ((x, y), shape) = piece
    if (!collision(TetrisWorld(((x, y+1), shape), pile, count, hold, minos, next))) {
      TetrisWorld(((x, y+1), shape), pile, count, hold, minos, next)
    } else {
      val newPile = S.combine(pile, S.shiftSE(shape, x, y))
      val minos2 = newminos(minos)
      if (collision(TetrisWorld(((4, 0), next), newPile, count, hold, minos2.tail, minos2.head))) endOfWorld("Game Over")
      val pile2 = eraseRows(newPile)
      if (count == 0) {
        TetrisWorld(((4, 0), next), bottomUp(pile2), 9, hold, minos2.tail, minos2.head) 
      } else {
        TetrisWorld(((4, 0), next), pile2, count - 1, hold, minos2.tail, minos2.head)
      }
    }
  }

  // 2, 5. keyEvent
  def keyEvent(key: String): World = {
    val ((x, y), shape) = piece
    key match {
      case ("RIGHT") => {
        if(!collision(TetrisWorld(((x+1, y), shape), pile, count, hold, minos, next))) {
          TetrisWorld(((x+1, y), shape), pile, count, hold, minos, next)
        } else {
          TetrisWorld(piece, pile, count, hold, minos, next)
        }
      }
      case ("LEFT") => {
        if(!collision(TetrisWorld(((x-1, y), shape), pile, count, hold, minos, next))) {
          TetrisWorld(((x-1, y), shape), pile, count, hold, minos, next)
        } else {
          TetrisWorld(piece, pile, count, hold, minos, next)
        }
      }
      // 機能その１
      case ("DOWN") => {
        if(!collision(TetrisWorld(((x, y+1), shape), pile, count, hold, minos, next))) {
          TetrisWorld(((x, y+1), shape), pile, count, hold, minos, next)
        } else {
          TetrisWorld(piece, pile, count, hold, minos, next)
        }
      }
      case ("SPACE") => {
        if(!collision(TetrisWorld(((x, y), S.rotate(shape)), pile, count, hold, minos, next))) {
          TetrisWorld(((x, y), S.rotate(shape)), pile, count, hold, minos, next)
        } else {
          TetrisWorld(piece, pile, count, hold, minos, next)
        }
      }
      // 機能その２
      case ("UP") => {
        // 8-1. Lowest
        //目的：限界まで落下させる
        def Lowest(n: Int, m: Int, s: S.Shape, p: S.Shape, c: Int, h: S.Shape, mi: List[S.Shape], ne: S.Shape): World  = {
          collision(TetrisWorld(((n, m+1), s), p, c, h, mi, ne)) match {
            case true => TetrisWorld(((n, m), s), p, c, h, mi, ne)
            case false => Lowest(n, m+1, s, p, c, h, mi, ne)
          }
        }
        Lowest(x, y, shape, pile, count, hold, minos, next)
      }
      // 機能その４
      case ("f") => {
        if (hold == Nil) {
          val minos2 = newminos(minos)
          if (collision(TetrisWorld(((4, 0), minos2.head), pile, count, shape, minos2.tail, next))) endOfWorld("Game Over")
          TetrisWorld(((4, 0), minos2.head), pile, count, shape, minos2.tail, next)
        } else {
          if (!collision(TetrisWorld(((x, y), hold), pile, count, shape, minos, next))) {
            TetrisWorld(((4, 0), hold), pile, count, shape, minos, next)
          } else {
            TetrisWorld(((x, y), shape), pile, count, hold, minos, next)
          }
        }
      }
    }
  }

  // 3. collision
  def collision(world: TetrisWorld): Boolean = {
    val TetrisWorld(((x, y), shape), pile, count, hold, minos, next) = world
    val (n, m) = S.size(shape)
    if (x < 0 | x + m > 10 | y < 0 | y + n > 15) true
    else {
      val s = S.shiftSE(shape, x, y)
      S.overlap(s, pile)
    }
  }

  // 6. eraseRows
  def eraseRows(pile: S.Shape): S.Shape = {
    def eraser(p: S.Shape): S.Shape = {
      def checkFullRow(row: S.Row): Boolean = {
        row match {
          case Nil => true
          case r :: rs => (r != Transparent) && checkFullRow(rs)
        }
      }
      p match {
        case Nil => Nil
        case x :: xs => {
          if (checkFullRow(x)) eraser(xs)
          else x :: eraser(xs)
        }
      }
    }
    val pileX = eraser(pile)
    S.shiftSE(pileX, 0, 15 - pileX.length)
  }

  // 機能その３
  // 8-2. bottomUp 
  // 目的：pileを一つ天井に迫り、底に一列追加される
  def bottomUp(pile: S.Shape): S.Shape = {
    val Color = HSB(0, 0, 0.5f)
    val row = List(Color, Color, Color, Color, Color, Color, Color, Color, Transparent, Transparent)
    pile match{
      case Nil => Nil
      case p :: ps => ps :+ scala.util.Random.shuffle(row)
    }
  }
}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 15
  val BlockSize = 30

  // 新しいテトロミノの作成
  val r = new Random()

/*
  def newPiece(): ((Int, Int), S.Shape) = {
    val pos = (WellWidth / 2 - 1, 0)
    (pos,
     List.fill(r.nextInt(4))(0).foldLeft(S.random())((shape, _) => shape))
  }

  // 最初のテトロミノ
  val piece = newPiece()
*/

  val init_minos = scala.util.Random.shuffle(S.allShapes)
  val first_shape = init_minos.head
  val second_shape = (init_minos.tail).head

  // ゲームの初期値
  val world = TetrisWorld(((4,0), first_shape), List.fill(WellHeight)(List.fill(WellWidth)(Transparent)), 9, Nil, (init_minos.tail).tail, second_shape)

  // ゲームの開始
  world.bigBang(BlockSize * (WellWidth + 6), BlockSize * WellHeight, 1)
}