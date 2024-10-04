import java.awt.{Color, Graphics2D, Point, geom}

import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}
import java.awt.{event => jae}
import java.awt.Polygon
import java.awt.RenderingHints
import javax.swing.Timer

object Sierpinski extends SimpleSwingApplication {
  private val defaultSize = 600
  private val maxDepth = 10
  private val borderSize = 5
  private val delay = 1000
  private val startTime = System.currentTimeMillis() // just so it starts nicely at 0
  private def currMaxDepth = (System.currentTimeMillis() - startTime) / 1000 % maxDepth

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
      case KeyPressed(_, Key.C, _, _) =>
        new ColorChooserFrame(this)
      case KeyReleased(_, Key.Space, _, _) =>
    }
    
    val timer = new Timer(delay, new jae.ActionListener() {
      def actionPerformed(e: jae.ActionEvent): Unit = {
        repaint()
      }
    })
    timer.start()

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      g.setColor(fgColor)
      
      def tri(x: Int, y: Int, s: Int, depth: Int = 0): Unit = {
        val midX = x + s / 2
        val midY = y + s / 2
        
        g2d.setColor(fgColor)
        val triangle = new Polygon()
        triangle.addPoint(midX, y)
        triangle.addPoint(x, y+s)
        triangle.addPoint(x+s, y+s)
        g2d.fillPolygon(triangle)
   
        g2d.setColor(Color.white)
        val triangleInner = new Polygon()
        triangleInner.addPoint(x+s/4, midY)
        triangleInner.addPoint(x + 3*s/4, midY)
        triangleInner.addPoint(midX, y+s)
        g2d.fillPolygon(triangleInner)
        
        if (depth < currMaxDepth && s > 10) {
          tri(x+s/4, y, s/2, depth + 1)
          tri(x, midY, s/2, depth + 1)
          tri(midX, midY, s/2, depth + 1)
        }
      }
      val currentSize = Math.min(ui.size.width, ui.size.height)
      tri(borderSize, borderSize, currentSize - 2 * borderSize)
    }
  }

  def top: Frame = new MainFrame {
    title = "Sierpinski's Triangle"
    contents = ui
    pack().centerOnScreen()
    open()
  }
}

trait SettableFgColor {
  def setFgColor(c : Color): Unit
}
