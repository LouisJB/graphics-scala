
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

case class XY(x: Int, y: Int)

object Sprites {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Sprites"
 
  private val borderSize = 10
  private val frameRate = 25
  private val defaultDelayMs = 1000.0 / frameRate
  private val delayMs = defaultDelayMs
  private val noObjects = 20
  private val maxSpeed = 20

  case class BouncingSprite(var x: Int, var y: Int, var height: Int, var width: Int) extends SpriteType {
    protected var color = Color.white
    protected var speed = XY(1, 1)
    override def draw(g: Graphics2D) = {
      val c = g.getColor()
      g.setColor(color)
      g.fillOval(x, y, width, height)
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
        x + speed.x.sign * size.x
        y + speed.y.sign * size.y
        color = rndColor
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
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    (1 to noObjects).foreach { a =>
      val xDir = if (randInt(2, 0) > 0) 1 else -1
      val yDir  = if (randInt(2, 0) > 0) 1 else -1

      val sprite = new BouncingSprite(randInt(panelSize, 1), randInt(panelSize, 1), 20, 20) {
        color = rndColor
        speed = XY(randInt(maxSpeed, 1) * xDir, randInt(maxSpeed, 1) * yDir)
      }
      sprites.add(sprite)  
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      
      sprites.move(size)
      sprites.collision()
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
    println("awaitng frame close")
    winCloser.waitOnClose()
    println("Ended")
  }
}
