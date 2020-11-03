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

import scala.math.min

import sgeometry.Pos
import sdraw.{World, Color, Transparent, HSB}

import tetris.{ShapeLib => S}

// テトリスを動かすための関数
case class TetrisWorld(piece: ((Int, Int), S.Shape), pile: S.Shape, nexts: List[S.Shape], hold: S.Shape, sp: Int, isTitle: Boolean, status: (String, Int)) extends World() {

  // マウスクリックは無視
  def click(p: sgeometry.Pos): World = this

  // ブロックの描画
  // 座標は、x,yはそのまま、dx,dyはdivideを考慮する
  def drawRect(x: Int, dx: Int, y: Int, dy: Int, w: Int, h: Int, c: Color, divide: Int): Boolean = {
    canvas.drawRect(Pos(A.BlockSize * x + A.BlockSize/divide * dx, A.BlockSize * y + A.BlockSize/divide * dy), A.BlockSize/divide * w, A.BlockSize/divide * h, c)
  }

  // shape の描画（与えられた位置）
  // 1/divideの大きさにする
  def drawShape(pos: (Int, Int), shape: S.Shape, divide: Int): Boolean = {
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
      drawRect(x, dx, y, dy, 1, 1, color, divide)
    })
  }

  // shape の描画（原点）
  def drawShape00(shape: S.Shape): Boolean = drawShape((0, 0), shape, 1)

  // ステージとメニューをわける壁の描画
  // ステージの右端外に大きさ1/2,幅1,高さ40の白い壁
  // スキルポイントを赤で表示
  def drawWall(sp: Int): Boolean = {
    drawRect(A.WellWidth, 0, 0, 0, 1, A.WellHeight*2, sdraw.White, 2) &&
    Range(0, min(sp, A.WellHeight*2)).foldLeft(true)(
      (bool: Boolean, i: Int) => bool && canvas.drawRect(
        Pos(A.BlockSize*A.WellWidth +1, 
        A.BlockSize*A.WellHeight -i*A.BlockSize/2 -A.BlockSize/2 +1), 
        A.BlockSize/2 -2, 
        A.BlockSize/2 -2, 
        sdraw.Red)
    )
  }

  // 予告の描画
  // x=11マス,大きさ1/3,上から高さ2マスずつ最大2*7=14マス弱まで使う
  def drawNexts(nexts: List[S.Shape]): Boolean = {
    // indexを保持して頭から描画する
    def drawNextSub(nexts: List[S.Shape], index: Int): Boolean = {
      nexts match {
        case Nil => true
        case s :: ss => drawShape((A.WellWidth +1, index * 2),s,3) &&
          drawNextSub(ss, index+1)
      }
    }
    drawNextSub(nexts, 0)
  }

  // ゴーストブロックの描画
  def drawGhostBlock(piece: ((Int, Int), S.Shape), pile: S.Shape) = {
    val ((x,y),s) = piece
    //val searchY = binarySearch(true, false)((checkY: Int) => collision(((x, checkY), s), pile))_
    def searchY(checkY: Int): Int = if(collision(((x, checkY+1), s), pile)) checkY else searchY(checkY+1)
    //二分探索だと空洞があったときうまくいかないので線形探索に変更 
    val ghostY = searchY(y)
    val ghostS = s.map(_.map((b: S.Block) => if(b==Transparent) b else sdraw.DarkGray))
    drawShape((x, ghostY), ghostS, 1)
  }

  /*
  // 二分探索
  //(大きい方はTかFか, 大きい方を返すか)(関数)(小さい方, 大きい方)
  def binarySearch(ubIs: Boolean, retUb: Boolean)(func: Int => Boolean)(lb: Int, ub: Int): Int = {
    if(ub - lb <= 1){
      if(retUb) ub else lb
    }else{
      val mid = (lb + ub) / 2
      if(func(mid) == ubIs){
        binarySearch(ubIs, retUb)(func)(lb, mid)
      }else{
        binarySearch(ubIs, retUb)(func)(mid, ub)
      }
    }
  }
  */

  // タイトル画面の描画
  def drawTitle(isTitle: Boolean): Boolean = {
    if(isTitle){
      canvas.drawRect(Pos(0, A.BlockSize*A.WellHeight/2 -20), A.BlockSize*A.WellWidth, 30, sdraw.White)
      canvas.drawString(Pos(A.BlockSize*A.WellWidth/3, A.BlockSize*A.WellHeight/2), "Press Any Key")
    }else true
  }

  // スキルの状態の描画
  def drawStatus(str: String, time: Int) = {
    if(time > 0){
      val top = min(A.BlockSize*(2*7), A.BlockSize*(A.WellHeight-4)-10)
      val height = A.BlockSize*(A.WellHeight-4) - top
      // 予告の下かholdの少し上
      canvas.drawRect(Pos(A.BlockSize*A.WellWidth + A.BlockSize/2, top), A.BlockSize*A.SideWidth, height, HSB(1f/6, time*1f/A.MaxTime, 1))
      // 黄色からだんだん白色になる
      canvas.drawString(Pos(A.BlockSize*(A.WellWidth +1), A.BlockSize*(A.WellHeight-4) - height/3), s"(${str}, ${time})")
    }else true
  }

  // ゲーム画面の描画
  val CanvasColor = HSB(0, 0, 0.1f)

  def draw(): Boolean = {
    val (pos, shape) = piece
    canvas.drawRect(Pos(0, 0), canvas.width, canvas.height, CanvasColor) &&
    drawGhostBlock(piece, pile) && //GhostBlockは本体より下に描画する
    drawShape00(pile) &&
    drawShape(pos, shape, 1) &&
    drawWall(sp) &&
    drawNexts(nexts) &&
    drawShape((A.WellWidth +1, A.WellHeight -4), hold, 1) &&
    drawTitle(isTitle) &&
    drawStatus(status._1, status._2)
  }

  // 1, 4, 7. tick
  // 目的：時間の経過に応じて世界を更新する
  def tick(): World = {
    /*
    if(isTitle) TetrisWorld(piece, pile, nexts, hold, sp, isTitle)
    else{
      val ((x, y), s) = piece
      val newWorld = TetrisWorld(((x, y+1), s), pile, nexts, hold, sp, isTitle)
      if(collision(newWorld.piece, newWorld.pile)){
        //落下してぶつかったら
        val (newPile, addSp) = eraseRows(S.combine(S.shiftSE(s, x, y), pile)) //堆積
        val (newPiece, newNexts) = A.popNewPiece(nexts)
        if(collision(newPiece, newPile)){
          //新しいpieceがぶつかっていたら
          println("Game Over")
          TetrisWorld(piece, pile, nexts, hold, sp, true)
        }else{
          val newSp = sp + addSp
          if(newSp >= A.WellHeight*2){
            println("Game Clear")
            TetrisWorld(newPiece, newPile, newNexts, hold, A.WellHeight*2, true)
          }else{
            TetrisWorld(newPiece, newPile, newNexts, hold, newSp, isTitle)
          }
        }
      }else
        newWorld
    }
    */
    /*
    もしtitleなら何もしない
    もしfなら
      1つ下が空いていればそのまま
      でなければ着地
    もしgなら
      3つ下が空いていれば移動
      1つ下が空いていれば下げる
      でなければ着地
    もしその他なら
      1つ下が空いていれば下げる
      でなければ着地
    着地とは
      堆積して消す
      次のミノ持ってくる
      新しいミノぶつかっていたらGameOver
      点を加算
      目標点になったらGameClear
      処理したものを返す
    */
    if(isTitle) return TetrisWorld(piece, pile, nexts, hold, sp, isTitle, status)
    val remainTime = if(status._2 >= 1) status._2 -1 else 0 
    val newStatus = (if(remainTime>0) status._1 else "", remainTime)
    // statusの時間を1減らすがこのtickでは減らす前の状態を参照する
    val ((x, y), s) = piece
    if(status._1=="g" && !collision(((x, y+3), s), pile)) return TetrisWorld(((x, y+3), s), pile, nexts, hold, sp, isTitle, newStatus)
    if(!collision(((x, y+1), s), pile)){
      if(status._1=="f") return TetrisWorld(piece, pile, nexts, hold, sp, isTitle, newStatus)
      else return TetrisWorld (((x, y+1), s), pile, nexts, hold, sp, isTitle, newStatus)
    }
    val (newPile, addSp) = eraseRows(S.combine(S.shiftSE(s, x, y), pile)) //堆積
    val (newPiece, newNexts) = A.popNewPiece(nexts)
    if(collision(newPiece, newPile)){
      //新しいpieceがぶつかっていたら
      println("Game Over")
      return TetrisWorld(piece, pile, nexts, hold, sp, true, status)
    }
    val newSp = sp + addSp
    if(newSp >= A.WellHeight*2){
      println("Game Clear")
      return TetrisWorld(newPiece, newPile, newNexts, hold, A.WellHeight*2, true, newStatus)
    }
    TetrisWorld(newPiece, newPile, newNexts, hold, newSp, isTitle, newStatus)
  }

  // 2, 5. keyEvent
  // 目的：キー入力に従って世界を更新する
  def keyEvent(key: String): World = {
    if(isTitle) {
      val (piece, nexts) = A.popNewPiece(Nil)
      TetrisWorld(piece, S.empty(A.WellHeight, A.WellWidth), nexts, Nil, 0, false, ("", 0))
    }else{
      val ((x, y), s) = piece
      val newWorld = (key match {
        case "RIGHT" => {
          if(status._1=="o") TetrisWorld(piece, pile.map((row: S.Row) => row.tail ++ List(row.head)), nexts, hold, sp, isTitle, status)
          else TetrisWorld(((x+1, y), s), pile, nexts, hold, sp, isTitle, status)
          }
        case "LEFT" => {
          if(status._1=="o") TetrisWorld(piece, pile.map((row: S.Row) => row.last :: row.init), nexts, hold, sp, isTitle, status)
          else TetrisWorld(((x-1, y), s), pile, nexts, hold, sp, isTitle, status)
        }
        case "UP" => TetrisWorld(((x, y), S.rotate(s)), pile, nexts, hold, sp, isTitle, status)
        case "DOWN" => TetrisWorld(((x, y+1), s), pile, nexts, hold, sp, isTitle, status)
        case "SPACE" => {
            if(hold == Nil){
              val (newPiece, newNexts) = A.popNewPiece(nexts)
              TetrisWorld(newPiece, pile, newNexts, s, sp, isTitle, status)
            } else {
              TetrisWorld(((A.WellWidth / 2 - 1, 0), hold), pile, nexts, s, sp, isTitle, status)
            }
          } // 他のkeyと同じくhold操作によってpieceが重なってしまう場合はhold操作が無視される
        case "f" => TetrisWorld(piece, pile, nexts, hold, sp-1, isTitle, ("f", A.MaxTime))
        case "g" => TetrisWorld(piece, pile, nexts, hold, sp-1, isTitle, ("g", A.MaxTime))
        case "o" => TetrisWorld(piece, pile, nexts, hold, sp-1, isTitle, ("o", A.MaxTime))
        case "c" => {
          val newNexts = if(hold == Nil) Nil else S.duplicate(7, hold)
          TetrisWorld(piece, pile, newNexts, hold, sp-1, isTitle, status)
        }
        // skillのcostが払えないときは無視される
        case "1" => TetrisWorld(piece, pile, nexts, hold, sp+1, isTitle, status)
        case _ => TetrisWorld(piece, pile, nexts, hold, sp, isTitle, status)
      })
      if((newWorld.sp <0) || collision(newWorld.piece, newWorld.pile)) TetrisWorld(piece, pile, nexts, hold, sp, isTitle, status)
      else newWorld
    }
  }

  // 3. collision
  // 目的：受け取ったpieceがpileや枠と衝突しているか判定する
  def collision(w_piece: ((Int, Int), S.Shape), w_pile: S.Shape): Boolean = {
    val ((x, y), s) = w_piece
    val (r, c) = S.size(s)
    (x < 0) ||
    (x+c > A.WellWidth) ||
    (y+r > A.WellHeight) ||
    (S.overlap(S.shiftSE(s, x, y), w_pile))
  }

  // 6. eraseRows
  // 目的：pileを受け取ったら、揃った行を削除する、消した行数も返す
  def eraseRows(pile: S.Shape): (S.Shape, Int) = {
    val pileTemp = pile.foldRight(Nil: S.Shape)(
        (row: S.Row, temp: S.Shape) => 
          if(row.foldLeft(true)((ans: Boolean, block: S.Block) => ans && (block != Transparent))) //全マス色付きだったら
            temp
          else
            row :: temp
      )
    (S.empty(A.WellHeight-pileTemp.length, A.WellWidth) ++ pileTemp, A.WellHeight-pileTemp.length)
  }
}

// ゲームの実行
object A extends App {
  // ゲームウィンドウとブロックのサイズ
  val WellWidth = 10
  val WellHeight = 20
  val SideWidth = 5
  val BlockSize = 30

  val MaxTime = 10

  // 新しいテトロミノの作成
  val r = new Random()

  def newPiece(): ((Int, Int), S.Shape) = {
    val pos = (WellWidth / 2 - 1, 0)
    (pos,
     List.fill(r.nextInt(4))(0).foldLeft(S.random())((shape, _) => shape))
  }

  // 7ミノをシャッフルして返す
  def newPieceList(): List[S.Shape] = {
    r.shuffle(S.allShapes)
  }

  // 次のミノを取り出し、(piece, nexts)を返す
  // nextsが空になったら補充する
  def popNewPiece(nexts: List[S.Shape]): (((Int, Int), S.Shape), List[S.Shape]) = {
    try {
      val newShape = nexts.head
      val newNexts = if(nexts.tail.isEmpty) newPieceList() else nexts.tail
      val pos = (WellWidth / 2 - 1, 0)
      ((pos, newShape), newNexts)
    } catch {
      case e: java.util.NoSuchElementException => popNewPiece(newPieceList())
      //最初からnextsが空だったときは仕方なく補充してから取り出す
    }
  }

  // 最初のテトロミノ
  //val piece = newPiece()
  val (((x, y), s), nexts) = popNewPiece(Nil)

  // ゲームの初期値
  //val world = TetrisWorld(piece, List.fill(WellHeight)(List.fill(WellWidth)(Transparent)), nexts, Nil, 0, true)
  val pile = S.combine(S.shiftSE(s, x, WellHeight-S.size(s)._1), S.empty(WellHeight, WellWidth))
  val (piece, nexts2) = popNewPiece(nexts)
  val world = TetrisWorld(piece, pile, nexts2, Nil, 0, true, ("", 0))

  // ゲームの開始
  world.bigBang(BlockSize * (WellWidth + SideWidth), BlockSize * WellHeight, 1)
}
