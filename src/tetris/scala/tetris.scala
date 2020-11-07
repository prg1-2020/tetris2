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
import sdraw.{World, Color, Transparent, HSB, White}

import tetris.{ShapeLib => S}

// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape, finished:Boolean, elapsed:Int, erasedCount:Int) extends World() {

def roundBy(num: Double)(dp: Int): String = s"%1.${dp}f".format(num)
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

  def draw(): Boolean = {
    val (pos, shape) = piece
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    drawShape00(pile) &&
    drawShape(pos, shape) &&
    (canvas.drawString(Pos(10,10), roundBy(elapsed*0.1)(1) + "sec", White))&&
    (canvas.drawString(Pos(10,30), (if(20-erasedCount >= 0)20-erasedCount else 0) .toString() + "lines left", White))&&
    (if(finished) canvas.drawString(Pos(canvas.width/2-50, canvas.height/2), if(erasedCount>20)"Game Over"else"Clear!", White) else true)&&
    (if(finished) canvas.drawString(Pos(canvas.width/2-50, canvas.height/2+20), "TIME:" + roundBy(elapsed*0.1)(1)+"sec", White) else true)
  }

  // 1, 4, 7. tick
  // 目的：
  def tick(): World = {
    var ne = elapsed + 1
    if(elapsed % 3 != 0 || finished) {
      TetrisWorld(piece, pile, finished,if(finished) elapsed else ne,erasedCount)
    }
    else{
      val ((x,y),shape) = piece
      val newworld = TetrisWorld(((x,y+ 1), shape), pile, finished, ne,erasedCount)
      if(collision(newworld)) {
        var beferase = S.combine(S.shiftSE(shape,x,y),pile)
        val (erased, combined) = eraseRows(beferase)

        val ((nx,ny),nshape) = A.newPiece()
        if(erasedCount + erased >= 20 || S.overlap(S.shiftSE(nshape,nx,ny),combined)) {
          println("Game Over")
          TetrisWorld(piece, pile, true, elapsed,erasedCount + erased)
        }
        else TetrisWorld(((nx,ny),nshape), combined,finished, ne,erasedCount + erased)
      } 
      else newworld
    }
  }

  // 2, 5. keyEvent
  // 目的：
  def keyEvent(key: String): World = {
    val ((x,y),shape) = piece
    var ret = key match {
      case "RIGHT" => TetrisWorld(((piece._1._1 + 1,piece._1._2), piece._2), pile,finished, elapsed,erasedCount)
      case "LEFT" => TetrisWorld(((piece._1._1 - 1,piece._1._2), piece._2), pile,finished, elapsed,erasedCount)
      case "DOWN" => {
        if(!collision(TetrisWorld(((x,y+1), shape), pile, finished, elapsed,erasedCount))) TetrisWorld(((x,y+1), shape), pile, finished, elapsed,erasedCount)
        else TetrisWorld(((x,y), shape), pile, finished, elapsed,erasedCount)
      }
      case "UP" => TetrisWorld(((piece._1._1,piece._1._2), S.rotate(S.rotate(S.rotate(piece._2)))), pile,finished, elapsed,erasedCount)
      case "x" => TetrisWorld(((piece._1._1,piece._1._2), S.rotate(S.rotate(S.rotate(piece._2)))), pile,finished, elapsed,erasedCount)
      case "z" => TetrisWorld(((piece._1._1,piece._1._2), S.rotate(piece._2)), pile,finished, elapsed,erasedCount)
      case "SPACE" => {
        var yd = 1
        while(!collision(TetrisWorld(((x,y+yd), shape), pile, finished, elapsed,erasedCount))){
          yd= yd+1
        }
        TetrisWorld(((x,y+yd-1), shape), pile, finished, elapsed,erasedCount)
      }
      case _ => TetrisWorld(piece, pile,finished,elapsed,erasedCount)
    }
    if(finished || collision(ret)) TetrisWorld(piece, pile, finished, elapsed,erasedCount) else ret
  }

  // 3. collision
  // 目的：
  def collision(world: TetrisWorld): Boolean = {
    val ((x,y),shape) = world.piece
    val (ph,pw) = S.size(shape)
    val (h,w) = (A.WellHeight,A.WellWidth)
    x < 0 || x + pw-1 >= w || y + ph-1 >= h || S.overlap(S.shiftSE(shape,x,y), world.pile)
  }

  // 6. eraseRows
  // 目的：
  def eraseRows(pile: S.Shape): (Int, S.Shape) = {
    var buf = pile.foldLeft((0,Nil):(Int,S.Shape))((ret, row) => if(row.count(_!=Transparent) == A.WellWidth) (ret._1 + 1, ret._2) else (ret._1, row::ret._2))
    (buf._1, S.padTo(buf._2,A.WellHeight,A.WellWidth).reverse)
  }
}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 20
  val BlockSize = 30

  // 新しいテトロミノの作成
  val r = new Random()

  def newPiece(): ((Int, Int), S.Shape) = {
    val pos = (WellWidth / 2 - 1, 0)
    (pos,
     List.fill(r.nextInt(4))(0).foldLeft(S.random())((shape, _) => shape))
  }

  // 最初のテトロミノ
  val piece = newPiece()

  // ゲームの初期値
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)), false, 0, 0)

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 0.1)
}
