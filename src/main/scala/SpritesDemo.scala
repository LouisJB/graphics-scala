
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
import scala.swing.Font
import javax.swing.ImageIcon

case class XY(x: Int, y: Int)

object Sprites {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Sprites"
 
  private var frameRate = 20
  private def delayMs = 1000.0 / frameRate
  private var collisionEnabled = true
  private var showStats = false

  private val noObjects = 10
  private val maxSpeed = 10
  private val objectSize = 75
  private val trailLen = 2
  private val collidedFrameLen = 5
  private val allowStickyCollisions = false
  private val randColour = true
  private val randomSize = true
  private val fgColor = Color.white


  trait Mode
  case object Ball extends Mode
  case object Koch extends Mode
  case object Tri extends Mode
  case object StartFlake extends Mode
  case object Text extends Mode
  case object CatImage1 extends Mode
  case class CatImage(n: Int) extends Mode

  case class BouncingSprite(id: String, var x: Int, var y: Int, var height: Int, var width: Int, mode: Mode = Ball) extends SpriteType {
    protected var color = Color.white
    protected var speed = XY(1, 1)
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
        case StartFlake =>
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
          g.drawImage(image, x, y, null)
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

  private val sprites = new SpriteManager(true, collisionEnabled)
  
  def mkUi(frame: Frame): Panel = new Panel {
    background = Color.black
    val panelSize = defaultSize
    preferredSize = (panelSize, panelSize)
    focusable = true

    // this is actually going to control frame redraw
    val timer = new Timer(delayMs.toInt, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit =
        repaint()
    })
    timer.start()

    def setDelay(delayTimeMs: Double) = {
      frame.peer.setTitle(s"$titleMsg - delay: ${delayTimeMs}ms")
      timer.setDelay(delayTimeMs.toInt)
    }

    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) => key match {
        case Key.Up =>
          frameRate = min(frameRate + 1, 25)
          timer.setDelay(delayMs.toInt)
          setDelay(delayMs)
        case Key.Down =>
          frameRate = max(frameRate - 1, 1)
          setDelay(delayMs)
        case Key.P =>
          if (timer.isRunning) timer.stop else timer.start
        case Key.R =>
          sprites.clear()
        case Key.C =>
          collisionEnabled = !collisionEnabled
          sprites.setCollisionEnabled(collisionEnabled)
        case Key.N =>
          mkObjects()
        case Key.S =>
          showStats = !showStats
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    def mkObjects() = {
      (1 to noObjects).foreach { id =>
        val xDir = if (randInt(2, 0) > 0) 1 else -1
        val yDir = if (randInt(2, 0) > 0) 1 else -1

        val mode = randInt(7, 0) match {
          case 0 => Ball
          case 1 => Koch
          case 2 => Tri
          case 3 => StartFlake
          case 4 => Text
          case 5 => CatImage(1)
          case 6 => CatImage(2)
        }
        val newSize = if (randomSize)
          randInt(objectSize, 20)
        else objectSize
        val sprite = new BouncingSprite(id.toString, randInt(panelSize, 1), randInt(panelSize, 1), newSize, newSize, mode) {
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

      val originalColor = g2d.getColor()
      val originalFont = g2d.getFont()
      g2d.setColor(rndColor)
      g2d.setFont(new Font(originalFont.getName(), Font.Bold.id, 48))
      g2d.drawString("Meowy Christmas 2024", 100, 100)
      ChristmasTree.draw(g, size.width/4, size.height/4, size.width/2, size.height/2)
      g2d.setFont(originalFont)

      (1 to trailLen).map { z =>
        val opacity = z/trailLen.toFloat
        g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
        sprites.update(g2d, size)
      }

      if (showStats) {
        g.setColor(fgColor)
        g.drawString(s"frameRate: $frameRate", 10, 20)
        g.drawString(s"noObjects: ${sprites.length}", 10, 40)
      }

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

object ChristmasTree {
  private val ballSize = 20

  def draw(g: Graphics2D, x: Int, y: Int, width: Int, height: Int): Unit = {
    val originalColour = g.getColor()
    g.setColor(Color.green)
    val cX = width / 2
    val cY = height / 2

    def drawBall(x: Int, y: Int) =
      g.fillOval(x - ballSize / 2, y - ballSize / 2, ballSize, ballSize)

    val colours = Array(
      (Color.green.brighter, Color.red),
      (Color.green, Color.orange),
      (Color.green.darker, Color.blue),
      (Color.green.darker.darker, Color.pink),
      (Color.green.darker.darker.darker, Color.cyan),
      (Color.green.darker.darker.darker.darker, Color.magenta)
    )
    val noSegments = colours.length
    val points = colours.zipWithIndex.foreach { case ((c, cb), i) =>
      val yy = y + ((i + 1) * height) / noSegments
      val x1 = x + (noSegments - i) * width / 15
      val x2 = x + width - (noSegments - i) * width / 15
      val ps = Array(
        (x + cX, y + i * height / noSegments),
        (x1, yy),
        (x2, yy)
      )

      g.setColor(c)
      val xs = ps.map(_._1)
      val ys = ps.map(_._2)
      g.fillPolygon(xs, ys, ps.length)

      g.setColor(cb)
      drawBall(x1, yy)
      drawBall(x2, yy)
    }

    g.setColor(Color(92, 44, 6))
    g.fillRect(x + cX - 10, y + height - 10, 20, 40)

    g.setColor(Color.red.darker)
    g.fillRect(x + cX - 30, y + height + 20, 60, 60)

    g.setColor(Color.yellow)
    drawBall(x + cX, y)

    g.setColor(originalColour)
  }
}
