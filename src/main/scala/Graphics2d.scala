import java.awt.{Color, Graphics2D, Point, geom}

import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}
import java.awt.{event => jae}
import java.awt.Polygon
import java.awt.geom.Path2D
import java.awt.GradientPaint
import java.awt.RenderingHints

object Graphics2D extends SimpleSwingApplication {
  private val defaultSize = 600
  private val borderSize = 5

  private val GRADIENT_PAINT1 = new GradientPaint(0, 0, Color.YELLOW, 20, 20, Color.RED, true)

  lazy val ui: Panel = new Panel with SettableFgColor {
    background = Color.white
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

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val w = ui.size.width - 2 * borderSize
      val h = ui.size.height - 2 * borderSize
      val path = {
        val thePath = new Path2D.Double()
        thePath.moveTo(w / 2, borderSize)
        thePath.lineTo(w, h)
        thePath.lineTo(borderSize, h)
        thePath.closePath()
        thePath
      }

      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setPaint(GRADIENT_PAINT1)
      g2d.fill(path)

      g2d.draw3DRect(10, 10, 10, 10, true)
      g2d.setPaint(Color.CYAN)
      g2d.fillOval(w/3, h/3, w/3, h/3)
      g2d.setPaint(Color.YELLOW)
      g2d.fillOval(w/5, h/5, w/5, h/5)
    }
  }

  def top: Frame = new MainFrame {
    title = "Graphics2D"
    contents = ui
    pack().centerOnScreen()
    open()
  }
}
