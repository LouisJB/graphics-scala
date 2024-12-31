
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
import scala.swing.event.Key
import scala.swing.event._
import scala.swing.Component
import javax.swing.Timer
import java.awt.{event => jae}


object MandelbrotDemo {
  private val defaultSize = 800
  private val titleMsg = "Mandelbrot"
  private val zoomFactor = 2.0
  private val pixSize = 1
  private val defaultScale = 250.0
  private val defaultXloc = - 2.25
  private val defaultYloc = - 1.5
  private val defaultDepth = 15
  private val delayMs = 500.0

  // event and draw from awt so actually safe mutability here
  private var xScale = defaultScale
  private var yScale = defaultScale
  private var xLoc = defaultXloc
  private var yLoc = defaultYloc
  private var depth = defaultDepth
  private var showStats = false
  private var showDragRect = false

  private var mouseLoc = new Point(0, 0)
  private var mouseDownPoint = new Point(0, 0)
  private var draggedToPoint = new Point(0, 0)

  private var colourOffset = 0

  def rescale(upPoint: Point, size: Dimension) = {
    xLoc = xLoc + mouseDownPoint.x / xScale
    yLoc = yLoc + mouseDownPoint.y / yScale
    xScale  = xScale * size.width / abs(upPoint.x - mouseDownPoint.x)
    yScale  = yScale * size.height / abs(upPoint.y - mouseDownPoint.y)
  }

  def reset() = {
    xScale = defaultScale
    yScale = defaultScale
    xLoc = defaultXloc
    yLoc = defaultYloc
    depth = defaultDepth
    colourOffset = 0
  }

  def mkUi(frame: Frame): Panel = new Panel {
    val panelSize = defaultSize
    preferredSize = Dimension(panelSize, panelSize)
    focusable = true

    val timer = new Timer(delayMs.toInt, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        colourOffset += 1
        repaint()
      }
    })

    listenTo(keys)
    listenTo(mouse.clicks, mouse.moves)
    reactions += {
      case MousePressed(ource: Component, point: Point, modifiers: Key.Modifiers,
                        clicks: Int, triggersPopup: Boolean) =>
        mouseDownPoint = point
      case MouseMoved(source: Component, point: Point, modifiers: Key.Modifiers) =>
        mouseLoc = point
        repaint()
      case  MouseDragged(source: Component, point: Point, modifiers: Key.Modifiers) =>
        showDragRect = true
        draggedToPoint = point
        repaint()
      case MouseReleased(source: Component, point: Point, modifiers: Key.Modifiers,
                         clicks: Int, triggersPopup: Boolean) =>
        showDragRect = false
        rescale(point, size) // apply the zoom and new location rescaling on end drag rect
        repaint()
      case KeyPressed(_, key, modifiers, _) => key match {
        case Key.Plus | Key.I =>
          xScale = xScale * zoomFactor
          yScale = yScale * zoomFactor
        case Key.Minus =>
          xScale = xScale / zoomFactor
          yScale = yScale / zoomFactor
        case Key.Up => if (modifiers == Key.Modifier.Shift)
          yLoc *= 1.01
        else
          yLoc *= 1.1
        case Key.Down => if (modifiers == Key.Modifier.Shift)
          yLoc *= 0.99
        else
          yLoc *= 0.9
        case Key.Left => if (modifiers == Key.Modifier.Shift)
          xLoc *= 1.01
        else
          xLoc *= 1.1
        case Key.Right => if (modifiers == Key.Modifier.Shift)
          xLoc *= 0.99
        else
          xLoc *= 0.9
        case Key.D =>
          depth = min(depth * 1.2 + 1, Mandelbrot.maxDepth.toDouble).toInt
        case Key.E =>
          depth = max(depth / 1.2, 3).toInt - 1
        case Key.R =>
          reset()
        case Key.S =>
          showStats = !showStats
        case Key.C =>
          if (timer.isRunning) timer.stop else timer.start
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, _, _, _) =>
      repaint()
    }

    private val mamdelbrot = Mandelbrot(pixSize)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      
      // set up an offscreen screen buffer to render off
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      mamdelbrot.draw(g2d, size, xLoc, yLoc, xScale, yScale, depth, colourOffset)

      if (showDragRect) {
        g2d.setColor(Color.white)
        g2d.drawRect(
          min(mouseDownPoint.x, draggedToPoint.x), min(mouseDownPoint.y, draggedToPoint.y),
          abs(mouseDownPoint.x - draggedToPoint.x), abs(mouseDownPoint.y - draggedToPoint.y)
        )
      }
  
      if (showStats) {
        g2d.setColor(Color.white)
        g2d.drawString(s"xScale: $xScale, yScale: $yScale, depth: $depth", 10, 20)
        g2d.drawString(s"xLoc: $xLoc, yLoc: $yLoc", 10, 40)
        g2d.drawString(s"mouse: ${mouseLoc.x}, ${mouseLoc.y}", 10, 60)
      }

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

case class Mandelbrot(pixSize: Int) {
  import Mandelbrot._
  import Complex._
  val colourPallet = (0 to maxDepth).map{ i =>
    i -> new Color(
        pow(i, 3).toInt % 255,
        pow(i, 7).toInt % 255,
        pow(i, 5).toInt % 255
      )
  }.toMap

  def draw(g: Graphics2D, size: Dimension, xLoc: Double, yLoc: Double, xScale: Double, yScale: Double, depth: Int, colourOffset: Int = 0) = {
    for (i <- 0 to size.width by pixSize) {
      for (j <- 0 to size.height by pixSize) {
        val x = i / xScale + xLoc
        val y = j / yScale + yLoc
        val z0 = new Complex(x, y)
       
        var z = z0
        var iterations = 0
        var inSet = false

        // Mandelbrot looks for divergence up to max depth interations
        while (z.r < 2 && !inSet) {
          z = square(z) plus z0 // Mandelbrot
          //z = z mult sin(z)
          iterations += 1
          inSet = iterations == depth
        }
/*
        var isRoot = false
        while (!isRoot && !inSet) {
          val num = z.toPolar.pow(3).toCartesian minus 1
          val den = square(z) mult 3
          val znew = z minus (num divide den) plus z0
          iterations += 1
          isRoot = nearlyZero(num.r) || nearlyZero((znew minus z).r)
          z = znew
          inSet = iterations == depth
        }
*/
        val colour = if (inSet)
          Color.black
        else
          colourPallet((iterations + colourOffset) % (maxDepth + 1))

        g.setColor(colour)
        g.drawRect(i, j, pixSize, pixSize)
      }
    }
    print(".")
  }

  def nearlyZero(x: Double, tol: Double = 0.001) = abs(x) < tol
}

object Mandelbrot {
  val maxDepth = 125
}

case class Complex(val re: Double, val im: Double) {
  def r =
    sqrt(re * re + im * im)
  infix def plus(other: Complex) =
    Complex(re + other.re, im + other.im)
  infix def minus(other: Complex) =
    Complex(re - other.re, im - other.im)
  infix def minus(otherRe: Double) =
    Complex(re - otherRe, im)
  // ((a+ib)(c+id)=(ac-bd)+i(ad+bc)\)
  infix def mult(other: Complex) =
    Complex(re * other.re - this.im * other.im, re * other.im + im * other.re)
  infix def mult(scalar: Double) =
    Complex(re *  scalar, im * scalar)
  infix def divide(other: Complex) = {
    val d = pow(other.re, 2.0) + pow(other.im, 2.0)
    Complex(
      (re * other.re + im * other.im) / d,
      (im * other.re - re * other.im) / d
    )
  }

  def toPolar = Polar(r, atan(im/re))
}
case object Complex {
  def square(c: Complex) =
    Complex(c.re * c.re - c.im * c.im, 2.0 * c.re * c.im)

  // sin(𝑎 + 𝑏𝑖) = sin 𝑎 cosh 𝑏 + 𝑖cos𝑎 sinh 𝑏
  def sin(c: Complex) =
    Complex(Math.sin(c.re) * cosh(c.im), cos(c.re) * sinh(c.im))
}

case class Polar(r : Double, theta: Double) {
  def pow(n: Double) = Polar(Math.pow(r, n), n * theta)
  def toCartesian = Complex(r * cos(theta), r * Math.sin(theta))
}
