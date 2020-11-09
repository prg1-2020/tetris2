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
    val (gPos, gShape) = getGhost()
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    drawShape00(pile) &&
    drawShape(gPos, gShape) &&
    drawShape(pos, shape)
  }

  def getGhost(): ((Int, Int), S.Shape) = {
    
    val (pos, shape) = this.hardDrop().piece
    def changeCol(c: Color): Color = {
     c match{
       case Transparent => c
       case _ => sdraw.DarkGray
     }
    }
    val grayShape = shape.map(_.map(changeCol))

    (pos, grayShape)
  }

  def tick(): World = {
    val ((x, y), s) = piece
    val (h, w) = S.size(s)
    val nextW = TetrisWorld(((x, y+1), s), pile)

    if(y+h == A.WellHeight || collision(nextW)){
      val newW = TetrisWorld(A.newPiece(), eraseRows(S.combine(S.shiftSE(s,x,y), pile)))
      if(collision(newW)){
        //endOfWorld("Game Over")
        println("GameOver")
      }
      newW
      
    }else{
      nextW
    }
  }

  def hardDrop(): TetrisWorld = {
    val ((x, y), s) = piece
    val (h, w) = S.size(s)
    val nextW = TetrisWorld(((x, y+1), s), pile)
    if(collision(nextW)) this
    else nextW.hardDrop()
  }

  def keyEvent(key: String): World = {
    var ((x, y), s) = piece
    
    val nextW =
      key match{
        case "RIGHT" => TetrisWorld(((x+1, y), s), pile)
        case "LEFT" =>  TetrisWorld(((x-1, y), s), pile)
        case "UP" =>  TetrisWorld(((x, y), S.rotate(s)), pile)
        case "DOWN" => hardDrop()
      }

    if(collision((nextW))){
      this
    }else{
      nextW
    }
    
  }



  def collision(world: TetrisWorld): Boolean = {
    val ((x, y), s) = world.piece
    val (h, w) = S.size(s)
    val absS = S.shiftSE(s,x,y)

    x < 0 || A.WellWidth < x+w || A.WellHeight < y+h || S.overlap(absS, world.pile)
  }


  def eraseRows(pile: S.Shape): S.Shape = {

    //揃った行(けすべき) => false, 揃ってない行(けさないべき) => true
    def thisFilter[A](row:List[A]):Boolean = {
      row.map(x => if(x!=Transparent) false else true).foldLeft(false)(_||_)
    }
    def padToButOnlyToSouth(s: S.Shape, rows: Int): S.Shape = {
      val sr = s.length
      assert(rows >= sr)
      S.shiftSE(s, 0, rows-sr)
    }

    val newPile = pile.filter(thisFilter)
    val formattedPile = padToButOnlyToSouth(newPile, A.WellHeight)

    formattedPile
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
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)))

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
}
