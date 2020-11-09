package sdraw

import draw.{World => _World}

abstract class World() { world =>
  object _world extends _World {
    override def onClick(p: geometry.Posn): _World =
      world.click(sgeometry.Pos(p.x, p.y))._world
    def onTick(): _World =
      world.finishWorld(world.tick()._world)
    def onKeyEvent(key: String): _World =
      world.keyEvent(key)._world
    def draw(): Boolean = world.draw()
  }

  var theCanvas: Option[Canvas] = None
  def canvas: Canvas = {
    if (theCanvas.isEmpty) theCanvas = Some(Canvas(_world.theCanvas))
    theCanvas.get
  }

  private var finish = false
  private var finishstr = "default"

  def bigBang(width: Int, height: Int, t: Double): Boolean = _world.bigBang(width, height, t)
  def finishWorld(_world: _World): _World = { if(finish) _world.endOfWorld(finishstr) else _world }
  def endOfWorld(s: String): World = {
    this.finish = true;
    this.finishstr = s;
    this
  }

  def draw(): Boolean

  def click(p: sgeometry.Pos): World
  def tick(): World
  def keyEvent(key: String): World
}
