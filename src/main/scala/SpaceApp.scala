
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Graphics2D}
import scala.collection.mutable.Buffer
import scala.math._
import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event._
import java.text.DecimalFormat


object SpaceApp extends SimpleSwingApplication {

  val AU = (1.4960*pow(10,11))
  var starter = 0
  var dayCount = 0
  val formatter = new DecimalFormat("0.000")

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

    val submitButton = new Button("Submit")
    val textField = new TextField()
    val viewAngle = new Button("View (x,y)/(x,z)")
    val dropDown = new ComboBox(List[String]("Create a new satellite", "Distance", "Position", "Velocity"))
    val scaleButton = new Button("Scale")
    val informationLabel = new Label("")
    val startStop = new Button("Start/Stop")
    val resetButton = new Button("Reset")
    val dayLabel = new Label("Days from start:")
    val helpButton = new Button("Help?")



    val all = new BorderPanel {
      layout += new BorderPanel {
        layout += new GridPanel(2,1) {
          contents += new BorderPanel {
            layout += dropDown -> West
            layout += textField -> Center
            layout += submitButton -> East
          }
          contents += new BorderPanel {
            layout += new FlowPanel {
              contents += startStop
              contents += resetButton
            } -> West
            layout += dayLabel -> Center
            layout += informationLabel -> East
          }
        } -> North
        layout += arena -> Center
        layout += new GridPanel(1,1) {
            contents += new FlowPanel {
              contents += scaleButton
              contents += viewAngle
              contents += helpButton
            }
        } -> South
      } -> Center
    }
    contents = all

    listenTo(submitButton)
    listenTo(viewAngle)
    listenTo(dropDown)
    listenTo(scaleButton)
    listenTo(startStop)
    listenTo(resetButton)
    listenTo(helpButton)


    reactions += {
      case painoi: ButtonClicked => {
        val lahde = painoi.source
        if (lahde.text == "Submit") {
          if (dropDown.item == "Create a new satellite") {
            val inputs = textField.text.split(";").map(_.trim)
            if (inputs.length == 4) {
              val name = inputs(0).trim
              val posCoords = inputs(1).split(",").map(_.trim)
              val velCoords = inputs(2).split(",").map(_.trim)
              if (posCoords.length == 3 && velCoords.length == 3) {
                if (Space.spaceObjects.forall(_.name!=name)) {
                  try {
                    Space.spaceObjects += new Spaceobject(name, new Vector3D(posCoords(0).toDouble*AU,posCoords(1).toDouble*AU,posCoords(2).toDouble*AU), new Vector3D(velCoords(0).toDouble*1000,velCoords(1).toDouble*1000,velCoords(2).toDouble*1000), inputs(3).toDouble, 2.7, new Color(200,0,255), true)
                    arena.repaint()
                  } catch {
                    case e:Exception => Dialog.showMessage(all,"Error, check the form.")
                  }
                } else Dialog.showMessage(all,"An object with this name already exists")
              } else Dialog.showMessage(all,"Error, check the form.")
            } else Dialog.showMessage(all,"Error, check the form.")
          } else if (dropDown.item == "Distance") {
            val inputs = textField.text.split(";").map(_.trim)
            if (inputs.length==2) {
              val objects = Buffer[Spaceobject]()
              for (i <- Space.spaceObjects) {
                inputs(0) match {
                  case i.name => objects+=i
                  case _ =>
                }
                inputs(1) match {
                  case i.name=> objects+=i
                  case _ =>
                }
              }
              try informationLabel.text = "The distance between " + objects.head.name + " and " + objects(1).name + ": " + formatter.format(Space.distance(objects.head, objects(1))/AU) + " AU "
              catch {
                case e: Exception => Dialog.showMessage(all,"Objects not found")
              }
            } else {
              Dialog.showMessage(all,"Error, check the form.")
            }
          } else if (dropDown.item == "Position") {
            val input = textField.text.trim
            for (i <- Space.spaceObjects) {
               input match {
                case i.name => informationLabel.text = "The AU-scaled position (x,y,z): " + formatter.format(i.position.x/AU)  + ", " +  formatter.format(i.position.y/AU) + ", " +  formatter.format(i.position.z/AU) + " "
                case _ =>
              }
            }
          } else if (dropDown.item == "Velocity") {
            val input = textField.text.trim
            for (i <- Space.spaceObjects) {
               input match {
                case i.name => informationLabel.text = "The velocity: " + formatter.format(i.velocity.length/1000) + " km/s "
                case _ =>
              }
            }
          }
        } else if (lahde.text == "View (x,y)/(x,z)") {
          Space.viewAngle += 1
          arena.repaint()
        } else if (lahde.text == "Scale") {
          Space.viewScale += 1
          arena.repaint()
        } else if (lahde.text == "Start/Stop") starter += 1
        else if (lahde.text == "Reset")  {
          dayCount = 0
          dayLabel.text = "Days from start: " + floor(dayCount/24).toInt.toString
        }
        else if (lahde.text == "Help?") {
          Dialog.showMessage(all, "Welcome to the Solar System simulator!\n" +
            "Commands to write in the textline:\n" +
            "Create a new satellite; In form of: name; AU-scaled position (x,y,z);\n" +
            "velocity in km/s (x,y,z); mass in kg. e.g. satellite1; 0,0,1; 0,30,0; 4000\n" +
            "Distance; in form of; objectname1; objectname2. e.g. Sun; Earth\n" +
            "Position; in form of; objectname. e.g. Earth\n" +
            "Velocity; in form of; objectname. e.g. Earth\n")
        }
      }
    }




    val listener = new ActionListener() {
      def actionPerformed(e: ActionEvent) = {
        if (starter%2 == 1) {
          Space.step()
          arena.repaint()
          dayCount += 1
          dayLabel.text = "Days from start: " + floor(dayCount/24).toInt.toString
        }
      }
    }

    val timer = new javax.swing.Timer(5, listener)
    timer.start()
  }

}
