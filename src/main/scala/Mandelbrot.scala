
import java.awt.{Color, Graphics2D, Point, geom}
import java.awt.Dimension
import java.awt.Image
import java.awt.Color
import Math._
import scala.swing.MainFrame
import javax.swing.SwingUtilities
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}
import scala.swing.BorderPanel
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.RenderingHints


object MandelbrotDemo {
  private val defaultSize = 800
  private val titleMsg = "Mandelbrot"
 
  def mkUi(frame: Frame): Panel = new Panel {
    val panelSize = defaultSize
    preferredSize = Dimension(panelSize, panelSize)
    focusable = true

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      Mandelbrot.draw(g2d, size)
      
      // draw out the offline screen buffer to the displayed graphics screen, to display it
      val g2dr = g // g.asInstanceOf[Graphics2D]
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

object Mandelbrot {
  val iterMax = 1000

  def draw(g: Graphics2D, size: Dimension) = {
    val xLoc = - 2.5
    val yLoc = - 1.5
    val xScale = 300.0
    val yScale = 300.0

    for (i <- 0 to size.width) {
      for (j <- 0 to size.height) {
        val x = i / xScale + xLoc
        val y = j / yScale + yLoc
        val c = new Complex(x, y)

        var z = c
        var iterations = 0
        var inSet = false

        while (z.abs < 2 && !inSet) {         
          z = z.squared.plus(c)
          iterations += 1
          if (iterations == iterMax)
            inSet = true
        }

        val colour = if (inSet)
          Color.black
        else 
          new Color(
            pow(iterations, 5).toInt % 255,
            pow(iterations, 7).toInt % 255,
            pow(iterations, 11).toInt % 255)

        g.setColor(colour)
        g.drawRect(i, j, 1, 1)
      }
    }
  }

  class Complex(val real: Double, val imag: Double) {
    def squared =
      new Complex(real * real - imag * imag, 2 * real * imag)
    def abs =
      sqrt(real * real + imag * imag)
    def plus(other: Complex) =
      new Complex(real + other.real, imag + other.imag)
  }
}
