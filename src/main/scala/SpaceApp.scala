
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Graphics2D, RenderingHints}
import scala.swing._

object SpaceApp extends SimpleSwingApplication {

  val width      = 600
  val height     = 600
  val fullHeight = 610

  def top = new MainFrame {

    title = "Solar System simulator"
    resizable = false

    minimumSize   = new Dimension(width,fullHeight)
    preferredSize = new Dimension(width,fullHeight)
    maximumSize   = new Dimension(width,fullHeight)

    val arena = new Panel {


      override def paintComponent(g: Graphics2D) = {
         g.setColor(new Color(80, 180, 235))
         g.fillRect(0, 0, width, fullHeight)

         g.setColor(Color.white)
         Space.draw(g)
       }



    }

    contents = arena

    var count = 0

    val listener = new ActionListener() {
      def actionPerformed(e: ActionEvent) = {
        Space.step()
        arena.repaint()
        println(Space.distance(Space.aurinko, Space.merkurius))
        println(count)
        count+=1
      }
    }

    val timer = new javax.swing.Timer(50, listener)
    timer.start()
  }
}
