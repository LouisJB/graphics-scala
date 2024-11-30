import java.awt.{Color, Graphics2D, Point, geom}
import java.awt.Dimension
import java.awt.Image

trait SpriteType {
  def x: Int
  def y: Int

  def height: Int
  def width: Int

  def move(size: Dimension): Unit
  def draw(g: Graphics2D): Unit
}

class Sprite(var x: Int, var y: Int, var height: Int, var width: Int, img: Image) extends SpriteType {
  def move(size: Dimension): Unit = {}
  def draw(g: Graphics2D): Unit =
    g.drawImage(img, x, y, null)
}

class SpriteManager() {
  private var sprites: List[SpriteType] = Nil

  def add(sprite: SpriteType) =
    sprites = sprite :: sprites
  def move(size: Dimension): Unit =
    sprites.foreach(_.move(size))
  def draw(g: Graphics2D): Unit =
    sprites.foreach(_.draw(g))
}
