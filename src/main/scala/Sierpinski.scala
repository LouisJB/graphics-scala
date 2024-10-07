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


object Sierpinski extends SimpleSwingApplication {
  import GraphicUtils._
  private val defaultSize = 800
  private val maxDepth = 7 // how many levels to go down, given you cannot see sub-pixel going too far in is not useful
  private val borderSize = 5
  private val defaultDelayMs = 1000 // refresh timer period
  private val colorChangePerDepth = 1 // how may recolorings to do per refresh cycle (in 'r' random colour mode)
  private val timeFont = new Font("Digital-7", Font.Plain.id, 48)
  private var delayMs = defaultDelayMs
  private var randomColor = false // default random colour mode 'r' is disabled
  private var showTime = false
  private val startTime = System.currentTimeMillis() // just so it starts nicely at 0 depth
  private def currMaxDepth = {
    val x = (System.currentTimeMillis() - startTime) / (delayMs * colorChangePerDepth) % (maxDepth * 2)
    min(x , 2 * maxDepth - x)
  }

  lazy val ui: Panel = new Panel with SettableFgColor {
    background = Color.yellow
    val panelSize = defaultSize + 2 * borderSize
    preferredSize = (panelSize, panelSize)
    focusable = true

    private var fgColor = Color.black
    def setFgColor(c : Color) = {
      fgColor = c
      repaint()
    }

    val timer = new Timer(delayMs / colorChangePerDepth, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    })
    timer.start()

    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) => key match {
        case Key.C =>
          new ColorChooserFrame(this)
        case Key.T =>
          showTime = !showTime
        case Key.R =>
          randomColor = !randomColor
        case Key.B =>
          fgColor = Color.BLACK
        case Key.G =>
          fgColor = Color.GREEN
        case Key.Up =>
          delayMs = max(delayMs / 2, 10)
          timer.setDelay(delayMs)
          println(delayMs)
        case Key.Down =>
          delayMs = min(delayMs * 2, 10 * 1000)
          timer.setDelay(delayMs)
        case Key.P =>
          if (timer.isRunning) timer.stop else timer.start
        case Key.X | Key.Escape =>
          System.exit(0)
        case _ => 
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      // set up an offscreen screen buffer to render of
      val buffImg = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB)
      val g2d = buffImg.createGraphics()
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      g2d.setColor(background)
      g2d.fillRect(0, 0, size.width, size.height)
      def tri(x: Int, y: Int, s: Int, depth: Int = 0): Unit = {
        val midX = x + s / 2
        val midY = y + s / 2
        if (depth == 0) {
          g2d.setColor(fgColor)
          g2d.fillPolygon(
            mkPolygon(List(
              (midX, y),
              (x, y+s),
              (x+s, y+s)
            ))
          )
        }
        if (randomColor)
          g2d.setColor(rndColor())
        else
          g2d.setColor(Color.WHITE)
        g2d.fillPolygon(
          mkPolygon(List(
            (x+s/4, midY),
            (x + 3*s/4, midY),
            (midX, y+s)
          ))
        )
        
        if (depth < currMaxDepth && s > 5) {
          tri(x+s/4, y, s/2, depth + 1)
          tri(x, midY, s/2, depth + 1)
          tri(midX, midY, s/2, depth + 1)
        }
      }
      val currentSize = Math.min(ui.size.width, ui.size.height) - 2 * borderSize
      tri((size.width - currentSize) / 2, (size.height - currentSize) / 2, currentSize)

      if (showTime) {
        renderTime(g2d, size)
      }

      // draw out the offline screen buffer to the displayed graphics screen, to display it
      val g2dr = g.asInstanceOf[Graphics2D]
      g2dr.drawImage(buffImg, 0, 0, null)
      g2d.dispose()
    }
  }

  def top: Frame = new MainFrame {
    title = "Sierpinski's Triangle"
    contents = new BorderPanel { add(ui, BorderPanel.Position.Center) }
    pack().centerOnScreen()
    open()
  }

  def renderTime(g2d: Graphics2D, size: Dimension) = {
    g2d.setColor(Color.BLACK)
    g2d.setFont(timeFont)
    val timeStr = currentTimeStr
    val metrics = g2d.getFontMetrics(timeFont)
    val rect = metrics.getStringBounds(timeStr, g2d)
    val tx = (size.width - rect.getWidth.toInt) / 2
    g2d.drawString(timeStr, tx , size.height / 2 + rect.getHeight.toInt + metrics.getAscent)
  }

  private def mkPolygon(points : List[(Int, Int)]) =
    points.foldRight(new Polygon())((p, pgn) => { pgn.addPoint.tupled(p); pgn })
  
  private def currentTimeStr =
    LocalTime.now().format(DateTimeFormatter.ofPattern("hh:mm:ss"))
}

trait SettableFgColor {
  def setFgColor(c: Color): Unit
}

object GraphicUtils {
  val rand = new Random()
  def rndColor() = new Color(rand.nextFloat, rand.nextFloat, rand.nextFloat)
}
