package sprites

import utils._
import MathUtils._
import GraphicUtils._
import ShapesUtils._

import Math._
import javax.swing.ImageIcon
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.AlphaComposite
import scala.swing.Font


trait Mode
case object Ball extends Mode
case object Koch extends Mode
case object Tri extends Mode
case object StarFlake extends Mode
case object Text extends Mode
case class CatImage(n: Int) extends Mode

object BouncingSprite {
  private val collidedFrameLen = 5
  private val allowStickyCollisions = false
  private val randColour = true
}

case class BouncingSprite(id: String, var x: Int, var y: Int, var height: Int, var width: Int, mode: Mode = Ball) extends SpriteType {
  import BouncingSprite._
  protected var color = Color.white
  var speed = XY(1, 1)
  protected var collided = 0
  protected val opacity = (randInt(5, 0) + 3) / 10.0f
  private val startAngle = randInt(120, 0)
  private val numberSides = randInt(4, 2)
  private val showLegend = false
  private val legend = s"$id mode: $mode, ns: $numberSides"

  def loadImage(path: String) = {
    println("loading image: " + path)
    val imageIcon = new ImageIcon(path)
    val tmpImage = imageIcon.getImage()
    val image = new BufferedImage(imageIcon.getIconWidth(), imageIcon.getIconHeight(), BufferedImage.TYPE_INT_ARGB)
    image.getGraphics().drawImage(tmpImage, 0, 0, null)
    tmpImage.flush()
    image
  }

  val ks = KockSnowflake(width, height, randInt(5, 1))
  override def draw(g: Graphics2D) = {
    val c = g.getColor()
    g.setColor(color)
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
    mode match {
    case Ball =>
        (width to 1).by(-10).map { w =>
        g.setColor(brighter(g.getColor()))
        g.fillOval(x+(width-w)/2, y+(height-w)/2, w, w)
        }
    case Koch =>
        ks.draw(g, x, y)
    case Tri =>
        (startAngle to startAngle + 100 by 20).map { angle =>
        val (xs, ys, len) =
            toArrayXYPair(polyEx(x + width / 2, y + height / 2, width / 2, 360 / numberSides, angle.toInt))
        g.drawPolygon(xs, ys, len)
        }
    case StarFlake =>
        (startAngle to startAngle + 180 by 10).map { angle =>
        val (xs, ys, len) =
            toArrayXYPair(polyEx(x + width / 2, y + height / 2, randInt(width, 1) / 2, 180, angle.toInt))
        g.drawPolygon(xs, ys, len)
        }
    case Text =>
        val originalFont = g.getFont()
        g.setFont(new Font(originalFont.getName(), Font.Bold.id, 16))
        g.drawString("Meowy Christmas", x, y)
        g.setFont(originalFont)
    case CatImage(n) =>
        import javax.imageio.ImageIO
        val imagePath = if (n == 1)
        "/images/cat1.png"
        else
        "/images/cat2.png"
        val image = ImageIO.read(getClass().getResource(imagePath))
        g.drawImage(image, x, y, width, height, null)
    }
    if (showLegend)
    g.drawString(legend, x, y)
    g.setColor(c)
  }

  def move(size: Dimension) = {
    val sx =
    if (x <= 0)
        abs(speed.x)
    else if (x + width >= size.width)
        abs(speed.x) * -1
    else speed.x

    val sy =
    if (y <= 0)
        abs(speed.y)
    else if (y + height >= size.height)
        abs(speed.y) * -1
    else speed.y

    speed = XY(sx, sy)

    x = min(size.width - width, x + speed.x)
    y = min(size.height - height, y + speed.y)

    if (collided > 0) {
    collided = collided - 1
    if (collided == 0)
        if (randColour)
        color = rndColor
        else
        color = Color.white
    }
  }

  def collision(s: SpriteType): Boolean =
    sqrt(sqr(center._1 - centerOf(s)._1) + sqr(center._2 - centerOf(s)._2)) < size.x + sizeOf(s).x

  def collided(other: SpriteType): SpriteType = {
    if (other.isInstanceOf[BouncingSprite]) {
    val s = other.asInstanceOf[BouncingSprite]
    speed = XY(
        (abs(speed.x) + abs(s.speed.x)) / 2 * speed.x.sign * s.speed.x.sign,
        (abs(speed.y) + abs(s.speed.y)) / 2 * speed.y.sign * s.speed.y.sign
    )
    if (!allowStickyCollisions) {
        x = x + speed.x.sign * (size.x / 2)
        y = y + speed.y.sign * (size.y / 2)
        collided = collidedFrameLen
        color = Color.white
    }
    }
    this
  }

  def size = sizeOf(this)
  def sizeOf(s: SpriteType): XY =
    XY(width / 2, height / 2)

  def center = centerOf(this): (Int, Int)
  def centerOf(s : SpriteType): (Int, Int) =
    (s.x + s.width / 2, s.y + s.height / 2)
}

case class KockSnowflake(width: Int, height: Int, depth: Int = 3) {
  val level = depth
  val border = 0

  def draw(g: Graphics2D, x: Int, y: Int) = {
    val xStart = x + width/2 - height/2

    drawFlake(g, level, xStart + border, y + height - border, xStart + height - border, y + height - border)
    drawFlake(g, level, xStart + height - border, y + height - border, xStart + height/2, y + border)
    drawFlake(g, level, xStart + height/2, y + border, xStart + border, y + height - border)
  }

  def drawFlake(g: Graphics2D, lev: Int, x1: Int, y1: Int, x5: Int, y5: Int): Unit = {
    if (lev == 0) {
      g.drawLine(x1, y1, x5, y5)
    }
    else {
      val deltaX = x5 - x1
      val deltaY = y5 - y1

      val x2 = x1 + deltaX / 3
      val y2 = y1 + deltaY / 3

      val x3 = (0.5 * (x1 + x5) + Math.sqrt(3) * (y1 - y5) / 6).toInt
      val y3 = (0.5 * (y1 + y5) + Math.sqrt(3) * (x5 - x1) / 6).toInt

      val x4 = x1 + 2 * deltaX / 3
      val y4 = y1 + 2 * deltaY / 3

      drawFlake(g,lev-1, x1, y1, x2, y2)
      drawFlake(g,lev-1, x2, y2, x3, y3)
      drawFlake(g,lev-1, x3, y3, x4, y4)
      drawFlake(g,lev-1, x4, y4, x5, y5)
    }
  }
}
