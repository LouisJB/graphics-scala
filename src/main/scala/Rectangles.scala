
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


object Rectangles {
  import GraphicUtils._
  import ShapesUtils._
  import Utils._
  private val defaultSize = 800
  private val titleMsg = "Rectangles"
 
  private val borderSize = 10
  private val defaultDelayMs = 1000.0
  private var delayMs = defaultDelayMs
  private val maxShapes = 50

  private var rects: List[(g2d: Graphics2D) => Unit] = Nil

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
        case Key.Up =>
          delayMs = max(delayMs / 2, 7.8125)
          timer.setDelay(delayMs.toInt)
          setDelay(delayMs)
        case Key.Down =>
          delayMs = min(delayMs * 2, 4 * 1000)
          setDelay(delayMs)
        case Key.P =>
          if (timer.isRunning) timer.stop else timer.start
        case Key.C =>
          rects = Nil
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ => 
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setColor(background)
      g2d.fillRect(0, 0, size.width, size.height)

      val x = randInt(size.width, 0)
      val y = randInt(size.height, 0)
      val w = randInt(size.width - x)
      val h = randInt(size.height - y)
      val lineWidth = randInt(200, 2) / 10
      val rc = rndColor
      val style = randInt(4, 0)
      val arcWidth = randInt(w/4, 5)
      val arcHeight = randInt(h/4, 5)
      
      val rect = (g2d: Graphics2D) => {
        g2d.setColor(rc)
        g2d.setStroke(new BasicStroke(lineWidth.toFloat))
        style match {
          case 0 => g2d.drawRect(x, y, w, h)
          case 1 => g2d.drawRoundRect(x, y, w, h, arcWidth, arcHeight)
          case 2 => g2d.drawOval(x, y, w, h)
          case 3 => g2d.drawLine(x, y, w, h)
        }
      }
      if (rects.length < maxShapes)
        rects = rect :: rects
      else
        rects = rect :: Nil

      rects.reverse.foreach(r => r(g2d))

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
