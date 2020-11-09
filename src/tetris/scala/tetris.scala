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
//点数を記憶しておくscoreとゲームが開始されたかを記録する(開始後なら"Y"、開始前なら"N")startedを追加
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape, score: Int, started: String) extends World() {

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
    drawScore(score) &&
    drawStart &&
    drawEnding
  }

  // 1, 4, 7. tick
  // 目的：ゲームが更新されるたびに行われていく処理を定める
  def tick(): World = {
    val ((x, y), shape) = piece
    val (a, b) = S.size(shape)
    //課題１
    //TetrisWorld(((x, y+1), shape), pile)
    //課題4
    //if (y+a==A.WellHeight) TetrisWorld(((x, y), shape), shape) else TetrisWorld(((x, y+1), shape), pile)
    //課題７をもとに追加機能を実装
    //ゲームオーバーはゲームが見かけ上停止する形式での実装
    //shapeがList(List(Transparent))なpieceであることをゲームオーバーの現れとしている
    if (started == "N") TetrisWorld(piece, pile, score, "N")
    else if (shape == List(List(Transparent))) TetrisWorld(piece, pile, score, "Y") 
    else if ((collision(TetrisWorld(((x, y+1), shape), pile, score, "Y")))) {
      val (afterpile, addscore) = eraseRows(S.combine(S.shiftSE(shape, x, y), pile))
      //表示行数の都合でscoreの最大値は9999999とした
      val newscore = if(9999999 <= score + addscore) 9999999
                     else score + addscore
      val nextPiece = A.newPiece()
      val nextWorld = TetrisWorld(nextPiece, afterpile, newscore, "Y")
      if (collision(TetrisWorld(nextPiece, afterpile, newscore, "Y"))) {
        //指示に沿ってendOfWorldを入れているが効果はない
        if(true) endOfWorld("Game Over")
        //ゲームオーバーを示すやり方として、shapeがList(List(Transparent))なpieceを使い、見かけ上動かなくしている
        TetrisWorld(((1, 1), List(List(Transparent))), afterpile, newscore, "Y")
      } 
      else nextWorld
    }
    else TetrisWorld(((x, y+1), shape), pile, score, "Y")
    
  }

  // 2, 5. keyEvent
  // 目的：ゲームのキー操作を定める
  def keyEvent(key: String): World = {
    val ((x, y), shape) = piece
    //課題２
    /*
    key match{
      case "RIGHT" =>TetrisWorld(((x+1, y), shape), pile)
      case "LEFT"  =>TetrisWorld(((x-1, y), shape), pile)
      case "UP"    =>TetrisWorld(((x, y), S.rotate(shape)), pile)
    }
    */
    //課題５をもとに追加機能を実装
    //下キーを押すと下がれるところまで下がれる機能のための関数goDown
    //下がった後も動かせると操作性が広がるので、pileへの結合はしない方針で実装
    def goDown(world: TetrisWorld): TetrisWorld ={
      val ((xD, yD), shapeD) = world.piece
      val (aD, bD) = S.size(shapeD)
      if (!collision(TetrisWorld(((xD, yD+1), shapeD), world.pile, world.score, "Y"))) 
        goDown(TetrisWorld(((xD, yD+1), shapeD), world.pile, world.score, "Y"))
      else TetrisWorld(world.piece, world.pile, world.score, "Y")
    }
    if (started == "N") {
      key match{
        case "UP" => TetrisWorld(piece, pile, score, "Y")
        case _    => TetrisWorld(piece, pile, score, "N")
      }
    }
    else key match {
      case "RIGHT" => if (collision(TetrisWorld(((x+1, y), shape), pile, score, "Y"))) TetrisWorld(piece, pile, score, "Y") 
                      else TetrisWorld(((x+1, y), shape), pile, score, "Y")
      case "LEFT"  => if (collision(TetrisWorld(((x-1, y), shape), pile, score, "Y"))) TetrisWorld(piece, pile, score, "Y") 
                      else TetrisWorld(((x-1, y), shape), pile, score, "Y")
      case "UP"    =>if (collision(TetrisWorld(((x, y), S.rotate(shape)), pile, score, "Y"))) TetrisWorld(piece, pile, score, "Y")
                     else TetrisWorld(((x, y), S.rotate(shape)), pile, score, "Y")
      case "DOWN"  => goDown(TetrisWorld(piece, pile, score, "Y"))
    }
  }

  // 3. collision
  // 目的：受け取ったworldでpieceが下・右・左ではみ出しているか、また、pileとpieceが重なっているかをBooleanで返す(追加機能を実装)
  def collision(world: TetrisWorld): Boolean = {
    val ((x, y), shape) = world.piece
    val (a, b) = S.size(shape)
    (x<0)||((x+b)>A.WellWidth)||((y+a)>A.WellHeight)||S.overlap(S.shiftSE(shape, x, y), world.pile)
  }

  // 6. eraseRows
  // 目的：pileを受け取り、そろった行を削徐したpileを返す(追加機能実装)
  def eraseRows(pile: S.Shape): (S.Shape, Int) = {
    def reverseRow[A](list:List[A]):List[A] = {
      list match {
        case Nil   => Nil
        case x::xs => reverseRow(xs)++List(x)
      }
    }
    def findFilled(revpile: S.Shape):S.Shape = {
      revpile match{
        case Nil   => Nil
        case x::xs => if(x.foldLeft(true){(acc, x) => acc&&(x!=Transparent)}) findFilled(xs)++List(S.duplicate(A.WellWidth, Transparent))
                      else  x::findFilled(xs)
      }
    }
    val newpile = reverseRow(findFilled(reverseRow(pile)))
    val vanishrow = (S.blockCount(pile)-S.blockCount(newpile))/A.WellWidth
    val addscore = if (vanishrow==0) 0
                   else if (vanishrow==1) 40
                   else if (vanishrow==2) 100
                   else if (vanishrow==3) 300
                   else if (vanishrow==4) 1200
                   else 0
    (newpile, addscore)
  }
  //追加機能のための描画関数たち
  def drawScore(sc:Int):Boolean ={
    canvas.drawRect(Pos(0, 0), 100, 15, HSB(0, 0, 1)) &&
    canvas.drawString(Pos(1, 10), s"score:${sc}")
  }

  def drawStart:Boolean={
    if (started == "N") { 
      canvas.drawRect(Pos(100, 120), 80, 30, HSB(0, 0, 1)) &&
      canvas.drawString(Pos(107, 130), "PRESS UP") &&
      canvas.drawString(Pos(107, 145), "TO START")
    }
    else true
  }
  def drawEnding:Boolean ={
    val ((x, y), shape) = piece
    if (shape == List(List(Transparent))){
      canvas.drawRect(Pos(100, 120), 80, 15, HSB(0, 0, 1)) &&
      canvas.drawString(Pos(102, 130), "GAME OVER")
    }
    else true
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
  val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)), 0, "N")

  // ゲームの開始
  world.bigBang(BlockSize * WellWidth, BlockSize * WellHeight, 1)
}
