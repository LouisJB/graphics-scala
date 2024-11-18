
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
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.awt.BasicStroke
import scala.swing.Font
import scala.swing.BorderPanel
import javax.swing.JFrame
import scala.runtime.stdLibPatches.language.experimental.modularity
import java.awt.Rectangle
import java.util.concurrent.atomic.AtomicBoolean
import java.awt.event.WindowAdapter
import javax.swing.SwingUtilities


object Shapes {
  import GraphicUtils._
  import ShapesUtils._
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

    val rand = new Random()
    def rndNoOfSides = (abs(rand.nextInt()) % 20) + 1
    def rndLineWidth = ((abs(rand.nextInt()) % 150) / 10 + 0.2).toFloat

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render of
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
      val minorScale = (abs(rand.nextInt()) % 9) + 3
      val a = System.currentTimeMillis() % 359
      val rotateStep = (abs(rand.nextInt()) % 20) + 2
      val arcSize = (abs(rand.nextInt()) % 340) + 20

      g2d.setStroke(new BasicStroke(lineWidth))

      (a to a + arcSize by rotateStep).map { angle =>
        g2d.setColor(rndColor)
        val (xs, ys, len) = toArrayXYPair(polyEx(size.width / 2, size.height / 2, maxSize / 2 - borderSize, 360/noOfSides, angle.toInt, Some(maxSize / minorScale - borderSize)))
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

  private def mkPolygon(points : List[(Int, Int)]) =
    points.foldRight(new Polygon())((p, pgn) => { pgn.addPoint.tupled(p); pgn })

  private def currentTimeStr =
    LocalTime.now().format(DateTimeFormatter.ofPattern("hh:mm:ss"))

  def main(args: Array[String]): Unit = {
    println("Sierpinski starting")
    // to avoid modal dialog to block main we can use a boolean signal
    val closed = new AtomicBoolean(false)
    def addClosingHandler(frame: Frame): Unit = {
      frame.peer.addWindowListener(new WindowAdapter() {
        override def windowClosed(ev: java.awt.event.WindowEvent): Unit = {
          closed.synchronized {
            closed.set(true)
            closed.notify()
          }
          super.windowClosed(ev)
        }
      })
    }
    def createAndOpen(): Unit = {
      println(s"Creating GUI for: $titleMsg")
      val topFrame = new MainFrame()
      topFrame.peer.setUndecorated(false)
      topFrame.peer.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
      addClosingHandler(topFrame)
      topFrame.bounds = new Rectangle(0, 0, defaultSize, defaultSize)
      topFrame.title = titleMsg
      topFrame.contents = new BorderPanel { add(mkUi(topFrame), BorderPanel.Position.Center) }
      topFrame.pack().centerOnScreen()
      topFrame.open()
      println("Sierpinski open completed")
    }
    SwingUtilities.invokeAndWait { new Runnable {
      override def run(): Unit = createAndOpen()
    }}
    println("awaitng frame close")
    closed.synchronized {
      while (!closed.get()) {
        closed.wait()
      }
    }
    println("Sierpinski ended")
  }
}

object ShapesUtils {
  // create array of x-y points
  //non-rotated polygon around a circle
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
