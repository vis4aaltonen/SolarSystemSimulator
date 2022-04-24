import scala.collection.mutable.Buffer
import scala.math._
import java.awt.Graphics2D
import java.awt.Color

object Space {
  val G = 6.67384 * pow(10, -11) //gravitational constant
  val DAY = 60*60  //one hour
  val SCALE = 200/ (1.4960*pow(10,11))

  //RK4 shit
  /**
  var time = 0.0
  val dt = 0.1
  */

  val spaceObjects = Buffer[Spaceobject]()

  val aurinko = Spaceobject("aurinko", new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0), 1.989*pow(10,30), 30.0, new Color(255,180,0))
  val maa = Spaceobject("maa", new Vector3D(1.4960*pow(10,8)*1000,0,0), new Vector3D(0.0,29.78*1000,0.0), 5.9737*pow(10,24), 9.0, new Color(0,0,255))
  val merkurius = Spaceobject("merkurius", new Vector3D(0.5791*pow(10,8)*1000,0,0), new Vector3D(0.0,47.36*1000,0.0), 330.1*pow(10,21), 5.0, new Color(192,192,192))
  val kuu = Spaceobject("kuu", new Vector3D(1.4960*pow(10,8)*1000+3.844*pow(10,5)*1000,0,0), new Vector3D(0.0,29.78*1000+1.022*1000,0.0), 7.342*pow(10,22), 4.0, new Color(192,192,102))
  spaceObjects += aurinko
  spaceObjects += maa
  spaceObjects += merkurius
  spaceObjects += kuu


  def distance(a:Spaceobject, b:Spaceobject) = (a.position.-(b.position)).length

  def updatePosition(a:Spaceobject): Unit = {
    var totalForceX = 0.0
    var totalForceY = 0.0
    var totalForceZ = 0.0

    for (i <- spaceObjects) {
      if (a.name != i.name) {
        val (forceX, forceY, forceZ) = a.gravitationForces(a,i)
        totalForceX += forceX
        totalForceY += forceY
        totalForceZ += forceZ
      }
    }
    val velocityX = totalForceX / a.mass * DAY
    val velocityY = totalForceY / a.mass * DAY
    val velocityZ = totalForceZ / a.mass * DAY

    //Calculates new position and velocity vectors to a
    a.velocity = a.velocity.+(new Vector3D(velocityX, velocityY, velocityZ))
    a.position = a.position.+(new Vector3D(a.velocity.x*DAY, a.velocity.y*DAY, a.velocity.z*DAY))
    a.scaledPos = a.scaledPos.+(new Vector3D(a.velocity.x*DAY*SCALE, a.velocity.y*DAY*SCALE, a.velocity.z*DAY*SCALE))
  }

  def step() = spaceObjects.foreach(updatePosition(_))

  def draw(g: Graphics2D) = spaceObjects.foreach(_.draw(g))

}
