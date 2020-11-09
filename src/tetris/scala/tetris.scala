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
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape) extends World() {

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
    drawShape(pos, shape)
  }

  // 1, 4, 7. tick
  // 目的：時間の経過に応じて世界を更新する
  /* 課題1
  def tick(): World = {
    val ((x,y), shape) = piece
    TetrisWorld(((x,y+1),shape), pile)
  }
  
  課題4
  def tick(): World = {
    val ((x,y), shape) = piece
    val (h, w) = S.size(shape)
    if (y + h > 9) this
    else TetrisWorld(((x,y+1), shape), pile)
  }
  */
  //　課題7
    def tick(): World = {
    val ((x, y), shape) = piece
    val maybeNextWorld = TetrisWorld(((x, y+1), shape), pile)
    if (collision(maybeNextWorld)) {
      val newpile = eraseRows(S.combine(S.shiftSE(shape, x, y), pile))
      if (collision(TetrisWorld(A.newPiece(), newpile)) == false) {
        TetrisWorld(A.newPiece(), newpile)
      }
      else {
        println("Game Over")
        TetrisWorld(piece, pile)
      }
      
    }
    else maybeNextWorld
  }

  // 2, 5. keyEvent
  // 目的：キー入力に従って世界を更新する
  //課題2
  /*
  def keyEvent(key: String): World = {
    val ((x,y), shape) = piece
    val (h, w) = S.size(shape)
    println(key)
    key match {
      case "RIGHT"=> TetrisWorld(((x+1,y),shape), pile)
      case "LEFT" => TetrisWorld(((x-1,y),shape), pile)
      case "UP" => TetrisWorld(((x,y),S.rotate(shape)), pile)
      case _ => this
    } 
  }
  */
  def keyEvent(key: String): World = {
    val ((x,y), shape) = piece
    val (h, w) = S.size(shape)
    println(key)
    val maybeNextWorld = {
      key match {
      case "RIGHT" =>{
        TetrisWorld(((x+1,y),shape), pile)
      }
      case "LEFT" =>{
        TetrisWorld(((x-1,y),shape), pile)
      }
      case "UP" =>{
        TetrisWorld(((x,y),S.rotate(shape)), pile)
      }
      case "DOWN" =>{
        TetrisWorld(((x,y+1),shape), pile)
      }
      case _ => TetrisWorld(((x,y),shape), pile)
    } 
    }
    if (collision(maybeNextWorld)) return this
    else maybeNextWorld
    
  }

  // 3. collision
  // 目的：受け取った世界で衝突が起きているかを判定する
 
 

  def collision(world: TetrisWorld): Boolean = {
    val (piece, pile) = (world.piece, world.pile)
    val (ph, pw) = S.size(pile)
    val (h, w) = S.size(piece._2)
    val ((x, y), shape) = piece
    
    x < 0 || x + w > pw || y + h > ph || S.overlap(S.shiftSE(piece._2, x, y), pile)
  }

  // 6. eraseRows
  // 目的：pileを受け取ったら、揃った行を削除する
  def eraseRows(pile: S.Shape): S.Shape = {
    val p = pile.filter(n => n.contains(Transparent))
    val x = 10 - p.length
    if (x == 0) p
    else {
      if (x == 1) println("シングル")
      else if (x == 2) println("ダブル")
      else if (x == 3) println("トリプル")
      else println("テトリス")
      println(score)
      List.fill(x)(List.fill(10)(Transparent)) ++ p
    }
  }
}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 10
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
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)))

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
}
