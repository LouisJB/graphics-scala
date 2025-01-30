package spritemove

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

import sprites._
import utils._
import GraphicUtils._
import ShapesUtils._
import MathUtils._
import java.awt.Rectangle
import java.awt.AlphaComposite
import scala.swing.Font
import javax.swing.ImageIcon
import java.awt.Toolkit


object SpriteMove {
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
  private val randColour = true
  private val allowStickyCollisions = false
  private val fgColor = Color.white


  case class MovingSprite(id: String, var x: Int, var y: Int, var height: Int, var width: Int, mode: Mode, collideFn: () => Unit) extends SpriteType {
    protected var color = Color.white
    var speed = XY(10, 10)
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

    override def draw(g: Graphics2D) = {
      val c = g.getColor()
      g.setColor(color)
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
      mode match {
        case CatImage(n) =>
          import javax.imageio.ImageIO
          val imagePath = if (n == 1)
            "/images/cat1.png"
          else
            "/images/cat2.png"
          val image = ImageIO.read(getClass().getResource(imagePath))
          g.drawImage(image, x, y, width, height, null)
      }
      g.setColor(c)
    }
    def move(size: Dimension) = {
      x = max(min(size.width - width, x), 0)
      y = max(min(size.height - height, y), 0)

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
        collideFn()
      }
      this
    }

    def moveRight(s: Int = speed.x) =
      this.x += s

    def moveLeft(s: Int = speed.x) =
      this.x -= s

    def moveUp(s: Int = speed.y) =
      this.y -= s
    
    def moveDown(s: Int = speed.y) =
      this.y += s
    
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

    var backgroundFlashCount = 0
    def flashBackground() = {
      backgroundFlashCount = 2
      (1 to 50).foreach { _ =>
        repaint()
      }
    }

    val mainSprite = new MovingSprite("main", randInt(panelSize, 1), randInt(panelSize, 1), 100, 100, CatImage(1), flashBackground)
    val otherSprite = new BouncingSprite("main", randInt(panelSize, 1), randInt(panelSize, 1), 100, 100, StarFlake)
  
    sprites.add(mainSprite)
    sprites.add(otherSprite)

    // this is actually going to control frame redraw
    val timer = new Timer(delayMs.toInt, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit =
        if (backgroundFlashCount > 0) {
          backgroundFlashCount -= 1
          background = rndColor
          Toolkit.getDefaultToolkit().beep()
        }
        else
          background = Color.BLACK

        repaint()
    }).start()

    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) => key match {
      case Key.Up =>
        mainSprite.moveUp()
      case Key.Down =>
        mainSprite.moveDown()
      case Key.Left =>
        mainSprite.moveLeft()
      case Key.Right =>
        mainSprite.moveRight()
      case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_ARGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      (1 to trailLen).map { z =>
        val opacity = z/trailLen.toFloat
        g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity))
        sprites.update(g2d, size)
      }

      // draw out the offline screen buffer to the displayed graphics screen, to display it
      g.drawImage(buffImg, 0, 0, null)
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
