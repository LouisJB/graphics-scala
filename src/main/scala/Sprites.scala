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

  def collision(s: SpriteType): Boolean
  def collided(s: SpriteType): SpriteType
}

class Sprite(var x: Int, var y: Int, var height: Int, var width: Int, img: Image) extends SpriteType {
  def move(size: Dimension): Unit = {}
  def draw(g: Graphics2D): Unit =
    g.drawImage(img, x, y, null)

  def size: Int = 0

  def collision(s: SpriteType): Boolean = false
  def collided(s: SpriteType): SpriteType = this
}

class SpriteManager() {
  private var sprites: List[SpriteType] = Nil

  def add(sprite: SpriteType) =
    sprites = sprite :: sprites
  def move(size: Dimension): Unit =
    sprites.foreach(_.move(size))
  def collision() = {
    0.until(sprites.length).map { i =>
        (i + 1).until(sprites.length).map { j =>
          val s1 = sprites(i)
          val s2 = sprites(j)
          if (s1.collision(s2)) {
            s2.collided(s1.collided(s2))
            true
          } else false
      }
    }
  }
  def draw(g: Graphics2D): Unit =
    sprites.foreach(_.draw(g))
}
