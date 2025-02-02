
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

import utils._
import GraphicUtils._
import ShapesUtils._
import MathUtils._
import scala.swing.Component


/**
  * Simple Game of life
  * Note - initially stopped, press p to pause/unpause to run/stop after an initial drawing pixel selection with mouse+click
  */
object GaneOfLife {
  private val defaultSize = 1000
  private val title = "Game of life"
 
  private val noOfCellsWide = 80
  private val noOfCellsHeigh = 80

  private val borderSize = 10
  private val frameRate = 10
  private var fgColor = Color.white

  val cells = Cells(noOfCellsWide, noOfCellsHeigh)

  def mkUi(frame: Frame): Panel = new Panel {
    background = Color.black
    val panelSize = defaultSize
    preferredSize = (panelSize, panelSize)
    focusable = true

    // this is actually going to control frame redraw
    val timer = new Timer(1000 / frameRate, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    })
    timer.stop()

    listenTo(keys)
    listenTo(mouse.clicks, mouse.moves)
    reactions += {
      case MousePressed(source: Component, point: Point, modifiers: Key.Modifiers, clicks: Int, triggersPopup: Boolean) =>
        cells.clickAt(point, size)
        repaint()
      case KeyPressed(_, key, modifiers, _) => key match {
        case Key.P =>
          if (timer.isRunning) {
            timer.stop
            frame.title = title + " - Stopped"
          }
          else {
            timer.start
            frame.title = title + " - Running"
          }
        case Key.X | Key.Q | Key.Escape =>
          System.exit(0)
        case _ =>
      }
      case KeyReleased(_, _, _, _) =>
      repaint()
    }

    override def paintComponent(g: Graphics2D): Unit = {
      val clipBounds = g.getClipBounds()
      // set up an offscreen screen buffer to double-buffer render on to
      val buffImg = new BufferedImage(clipBounds.width, clipBounds.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setClip(clipBounds)
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      super.paintComponent(g2d)
      
      val maxSize = min(size.height, size.width)

      g2d.setColor(Color.white)
      if (timer.isRunning())
        cells.update()
      cells.draw(g2d)

      // draw out the offline screen buffer to the displayed graphics screen, to display it
      g.drawImage(buffImg, 0, 0, null)
      g2d.dispose()
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Starting: $title")
    val winCloser = new WindowCloser()
    def createAndOpen(): Unit = {
      println(s"Creating GUI for: $title")
      val topFrame = new MainFrame()
      topFrame.peer.setUndecorated(false)
      topFrame.peer.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
      winCloser.addClosingHandler(topFrame)
      topFrame.bounds = new Rectangle(0, 0, defaultSize, defaultSize)
      topFrame.title = title
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

case class Cells(cellCols: Int, cellRows: Int) {
  import Cells._
  var cells = Array.tabulate(cellCols, cellRows)((i, j) => Cell(i, j))

  def calcCellSize(width: Int, height: Int): Int =
    Math.min(width / cellCols, height / cellRows)

  def clickAt(point: Point, size: Dimension) = {
    val cellSize = calcCellSize(size.width, size.height)
    val i = point.x / cellSize
    val j = point.y / cellSize
    cells(i)(j).flip
  }

  def valid(i: Int, j: Int): Option[(Int, Int)] =
    if ((i >= 0) && (i < cellCols) && (j >= 0) && (j < cellRows))
      Some((i, j))
    else None
  
  def adjacents(cell: Cell): Seq[Cell] = {
    val offsets = (-1 to 1)
    for
      xOffset <- offsets
      yOffset <- offsets
      if !((xOffset == 0) && (yOffset == 0))
      (i, j) <- valid(cell.i + xOffset, cell.j + yOffset)
    yield
      cells(i)(j)
  }

  def clonedCells =
    Array.tabulate(cellCols, cellRows)((i, j) => (cells(i)(j)).copy())
  
  def update(): Unit = {
    val newCells =  clonedCells
    cells.foreach(_.foreach { (c: Cell) =>
      // rules for canonical game of life
      val newFill =
        (c.fill, adjacents(c).filter(_.fill).length) match {
          case (true, n) if n >= 2 && n <= 3 => true // survives
          case (true, _) => false // death due to over or under population
          case (false, n) if n == 3 => true // reproduction
          case _ => false
        }
      newCells(c.i)(c.j) = c.copy(fill = newFill)
    })
    cells = newCells
  }

  case class Cell(i: Int, j: Int, var fill: Boolean = false) {
    def draw(g: Graphics2D) = {
      val bounds = g.getClipBounds()
      val cellSize = calcCellSize(bounds.width, bounds.height)
      val x = bounds.x + i * cellSize
      val y = bounds.y + j * cellSize
      if (fill)
        g.fillRect(x, y, cellSize, cellSize)
      else
        g.drawRect(x, y, cellSize, cellSize)
    }

    def flip = fill = !fill
  }

  def draw(g: Graphics2D) =
    cells.foreach(_.foreach(_.draw(g)))
}
