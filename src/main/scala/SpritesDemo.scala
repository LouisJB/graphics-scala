package spritedemo

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


object SpriteDemo {
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
  private val randomSize = true
  private val randColour = true
  private val fgColor = Color.white

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
          case 3 => StarFlake
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
