
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
import java.awt.event.WindowAdapter
import javax.swing.SwingUtilities


object Shapes {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Shapes"
 
  private val borderSize = 10
  private val delayMs = 500
  private var fgColor = Color.white

  def mkUi(frame: Frame): Panel = new Panel {
    background = Color.black
    val panelSize = defaultSize
    preferredSize = (panelSize, panelSize)
    focusable = true

    // this is actually going to control frame redraw
    val timer = new Timer(delayMs, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    })
    timer.start()

    def rndNoOfSides = randInt(20, 1)
    def rndLineWidth = randInt(150, 2) / 10

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val maxSize = min(size.height, size.width)

      g2d.setColor(background)
      g2d.fillRect(0, 0, size.width, size.height)
      g2d.setColor(fgColor)

      // some fun with randomised parameters
      val noOfSides = rndNoOfSides
      val lineWidth = rndLineWidth
      val minorScale = randInt(9, 3)
      val rotateStep = randInt(30, 2)
      val arcSize = randInt(340, 20)
      val startAngle = System.currentTimeMillis() % 359

      g2d.setStroke(new BasicStroke(lineWidth.toFloat))

      (startAngle to startAngle + arcSize by rotateStep).map { angle =>
        g2d.setColor(rndColor)
        val (xs, ys, len) =
          toArrayXYPair(polyEx(size.width / 2, size.height / 2, maxSize / 2 - borderSize, 360/noOfSides, angle.toInt, Some(maxSize / minorScale - borderSize)))
        g2d.drawPolygon(xs, ys, len)
      }
/*
      // basic example 'torus'
      g2d.setStroke(new BasicStroke(1f))
      (a to a + 360 by 7).map { angle =>
        g2d.setColor(rndColor)
        val (xs, ys, len) = toArrayXYPair(polyEx(size.width / 2, size.height / 2, maxSize / 2 - borderSize, 1, angle.toInt, Some(maxSize/5)))
        g2d.drawPolygon(xs, ys, len)
      }
*/
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

object ShapesUtils {
  // create array of x-y points
  // non-rotated polygon around a circle
  def poly(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0) = {
    (0 to 360 by arc).map ( a =>
      val r = ((a + initialAngle) * 2 * PI) / 360
      (x + (sin(r) * size).toInt, y + (cos(r) * size).toInt)
    ).toArray
  }

  // polygon with rotated axis/coordinate system and minor axis scaling
  def polyEx(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0, maybeMinorAxis: Option[Int] = None) = {
    val minorAxis = maybeMinorAxis.getOrElse(size)
    (0 to 360 by arc).map ( a =>
      val phi = (a * 2 * PI) / 360
      val theta = (initialAngle * 2 * PI) / 360
      val dx = sin(phi) * size
      val dy = cos(phi) * minorAxis
      val ddx = dx * cos(theta) + dy * sin(theta)
      val ddy = dx * -1.0 * sin(theta) + dy * cos(theta)
      
      (x + ddx.toInt, y + ddy.toInt)
    ).toArray
  }

  def polyDistorted(x: Int, y: Int, size: Int, arc: Int = 1, initialAngle: Int = 0, maybeMinorAxis: Option[Int] = None) = {
    val minorAxis = maybeMinorAxis.getOrElse(size)
    
    val width = size - sin(initialAngle) * minorAxis
    val height =  minorAxis + cos(initialAngle) * size
    
    (0 to 360 by arc).map ( a =>
      val r = ((a + initialAngle) * 2 * PI) / 360
      (x + (sin(r) * width).toInt, y + (cos(r) * height).toInt)
    ).toArray
  }

  def toArrayXYPair(points: Array[(Int, Int)]) =
    (points.map(_._1), points.map(_._2), points.length)
}

object Utils {
  lazy val rand = new Random()
  def randInt(range: Int, min: Int = 0) =
    (abs(rand.nextInt()) % range) + min
}
