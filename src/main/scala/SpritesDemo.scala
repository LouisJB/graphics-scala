
import Math._
import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Color, Graphics2D, Point, geom}
import java.awt.{event => jae}
import java.awt.Polygon
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import scala.util.Random
import java.awt.Dimension
import java.awt.BasicStroke
import scala.swing.BorderPanel
import javax.swing.JFrame
import java.awt.Rectangle
import java.util.concurrent.atomic.AtomicBoolean
import java.awt.event.WindowAdapter
import javax.swing.SwingUtilities


object Sprites {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Sprites"
 
  private val borderSize = 10
  private val frameRate = 25
  private val defaultDelayMs = 1000.0 / frameRate
  private var delayMs = defaultDelayMs

  class BouncingSprite(var x: Int, var y: Int, var height: Int, var width: Int) extends SpriteType {
    val color = Color.white
    val xSpeed = 1
    val ySpeed = 1
    override def draw(g: Graphics2D) = {
      val c = g.getColor()
      g.setColor(color)
      g.fillOval(x, y, width, height)
      g.setColor(c)
    }
    protected var xDir = 1
    protected var yDir = 1
    def move(size: Dimension) = {
      if (x <= 0)
        xDir = 1
      else if (x + width >= size.width)
        xDir = -1
      
      if (y <= 0)
        yDir = 1
      else if (y + height >= size.height)
        yDir = -1

      x = min(size.width - width, x + xDir * xSpeed)
      y = min(size.height - height, y + yDir * ySpeed)
    }
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

    (1 to 200).foreach { a => 
      val sprite = new BouncingSprite(randInt(panelSize, 1), randInt(panelSize, 1), 20, 20) {
        override val color = rndColor
        override val xSpeed = randInt(20, 1)
        override val ySpeed = randInt(20, 1)
        xDir = if (randInt(2, 0) > 0) 1 else -1 
        yDir = if (randInt(2, 0) > 0) 1 else -1
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
