
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Graphics2D}
import scala.swing.BorderPanel.Position._
import scala.swing._


object SpaceApp extends SimpleSwingApplication {

  val width      = 800
  val height     = 800
  val fullHeight = 810

  def top = new MainFrame {

    title = "Solar System simulator"
    resizable = false

    minimumSize   = new Dimension(width,fullHeight)
    preferredSize = new Dimension(width,fullHeight)
    maximumSize   = new Dimension(width,fullHeight)

    val arena = new Panel {
      override def paintComponent(g: Graphics2D) = {
         g.setColor(new Color(0, 0, 40))
         g.fillRect(0, 0, width, fullHeight)

         Space.draw(g)
       }
    }



    contents = new BorderPanel {
      layout += new BorderPanel {
        layout += new GridPanel(2,1) {
          contents += new BorderPanel {
            layout += new Label("new satellite") -> West
            layout += new TextField() -> Center
            layout += new Button("Submit") -> East
          }
          contents += new BorderPanel {
            layout += new Label("in form of: startingpos (x,y,z); startingvel (x,y,z); mass") -> Center
          }
        } -> North
        layout += arena -> Center
      } -> Center
    }



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

    val timer = new javax.swing.Timer(5, listener)
    timer.start()
  }
}
