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


object Sierpinski extends SimpleSwingApplication {
  import GraphicUtils._
  private val defaultSize = 800
  private val maxDepth = 7 // how many levels to go down, given you cannot see sub-pixel going too far in is not useful
  private val borderSize = 5
  private val delayMs = 1000 // refresh timer period
  private val colorChangePerDepth = 1 // how may recolorings to do per refresh cycle (in 'r' random colour mode)
  private var randomColor = false // default random colour mode 'r' is disabled
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

    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) => key match {
        case Key.C =>
          new ColorChooserFrame(this)
        case Key.R =>
          randomColor = !randomColor
      }
      case KeyReleased(_, Key.Space, _, _) =>
    }
    
    val timer = new Timer(delayMs / colorChangePerDepth, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    }).start()

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
      val currentSize = Math.min(ui.size.width, ui.size.height)
      tri(borderSize, borderSize, currentSize - 2 * borderSize)

      // draw out the offline screen buffer to the displayed graphics screen, to display it
      val g2dr = g.asInstanceOf[Graphics2D]
      g2dr.drawImage(buffImg, 0, 0, null)
      g2d.dispose()
    }
  }

  def top: Frame = new MainFrame {
    title = "Sierpinski's Triangle"
    contents = ui
    pack().centerOnScreen()
    open()
  }

  def mkPolygon(points : List[(Int, Int)]) =
    points.foldRight(new Polygon())((p, pgn) => { pgn.addPoint.tupled(p); pgn })
}

trait SettableFgColor {
  def setFgColor(c: Color): Unit
}

object GraphicUtils {
  val rand = new Random()
  def rndColor() = new Color(rand.nextFloat, rand.nextFloat, rand.nextFloat)
}
