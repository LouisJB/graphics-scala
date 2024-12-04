
import Math._
import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Color, Graphics2D, Point, geom}
import java.awt.{event => jae}
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import scala.util.Random
import java.awt.Dimension
import scala.swing.BorderPanel
import javax.swing.JFrame
import javax.swing.SwingUtilities
import MathUtils._
import java.awt.Rectangle
import java.awt.AlphaComposite

case class XY(x: Int, y: Int)

object Sprites {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Sprites"
 
  private val frameRate = 20
  private val defaultDelayMs = 1000.0 / frameRate
  private val delayMs = defaultDelayMs
  private val noObjects = 20
  private val maxSpeed = 10
  private val objectSize = 100
  private val trailLen = 2
  private val collisionEnabled = false
  private val collidedFrameLen = 5
  private val allowStickyCollisions = false
  private val randColour = true

  case class BouncingSprite(var x: Int, var y: Int, var height: Int, var width: Int) extends SpriteType {
    protected var color = Color.white
    protected var speed = XY(1, 1)
    protected var collided = 0
    protected val opacity = (randInt(5, 0) + 3) / 10.0f

    val ks = KockSnowflake(width, height, randInt(4, 1))
    override def draw(g: Graphics2D) = {
      val c = g.getColor()
      g.setColor(color)
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
      g.fillOval(x, y, width, height)
      //ks.draw(g, x, y)
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

  private val sprites = new SpriteManager()
  
  def mkUi(frame: Frame): Panel = new Panel {
    background = Color.black
    val panelSize = defaultSize
    preferredSize = (panelSize, panelSize)
    focusable = true

    // this is actually going to control frame redraw
    val timer = new Timer(delayMs.toInt, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    })
    timer.start()

    def setDelay(delayTimeMs: Double) = {
      frame.peer.setTitle(s"$titleMsg - delay: ${delayTimeMs}ms")
      timer.setDelay(delayTimeMs.toInt)
    }

    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) => key match {
        case Key.P =>
          if (timer.isRunning) timer.stop else timer.start
        case Key.C =>
          sprites.clear()
        case Key.N =>
          mkObjects()
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    def mkObjects() = {
      (1 to noObjects).foreach { a =>
        val xDir = if (randInt(2, 0) > 0) 1 else -1
        val yDir = if (randInt(2, 0) > 0) 1 else -1

        val sprite = new BouncingSprite(randInt(panelSize, 1), randInt(panelSize, 1), objectSize, objectSize) {
          if (randColour)
            color = rndColor
          speed = XY(randInt(maxSpeed, 1) * xDir, randInt(maxSpeed, 1) * yDir)
        }
        sprites.add(sprite)
      }
    }
    mkObjects()

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      
      (1 to trailLen).map { z =>
        val opacity = z/trailLen.toFloat
        g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
        sprites.move(size)
        if (collisionEnabled)
          sprites.collision()
        sprites.draw(g2d)
      }

      sprites.draw(g2d)

      // draw out the offline screen buffer to the displayed graphics screen, to display it
      val g2dr = g.asInstanceOf[Graphics2D]
      g2dr.drawImage(buffImg, 0, 0, null)
      g2d.dispose()
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Starting: $titleMsg")
    val winCloser = new WindowCloser()
    def createAndOpen(): Unit = {
      println(s"Creating GUI for: $titleMsg")
      val topFrame = new MainFrame()
      topFrame.peer.setUndecorated(false)
      topFrame.peer.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
      winCloser.addClosingHandler(topFrame)
      topFrame.bounds = new Rectangle(0, 0, defaultSize, defaultSize)
      topFrame.title = titleMsg
      topFrame.contents = new BorderPanel { add(mkUi(topFrame), BorderPanel.Position.Center) }
      topFrame.pack().centerOnScreen()
      topFrame.open()
      println("Open completed")
    }
    SwingUtilities.invokeAndWait { new Runnable {
      override def run(): Unit = createAndOpen()
    }}
    winCloser.waitOnClose()
    println("Ended")
  }
}

case class KockSnowflake(width: Int, height: Int, depth: Int = 3) {

  val level = depth
  val border = 0

  def draw(g: Graphics2D, x: Int, y: Int) = {
    val xStart = x + width/2 - height/2

    drawSnow(g, level, xStart + border, y + height - border, xStart + height - border, y + height - border)
    drawSnow(g, level, xStart + height - border, y + height - border, xStart + height/2, y + border)
    drawSnow(g, level, xStart + height/2, y + border, xStart + border, y + height - border)
  }

  def drawSnow(g: Graphics2D, lev: Int, x1: Int, y1: Int, x5: Int, y5: Int): Unit = {
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

      drawSnow(g,lev-1, x1, y1, x2, y2)
      drawSnow(g,lev-1, x2, y2, x3, y3)
      drawSnow(g,lev-1, x3, y3, x4, y4)
      drawSnow(g,lev-1, x4, y4, x5, y5)
    }
  }
}