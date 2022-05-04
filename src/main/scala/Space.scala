import scala.collection.mutable.Buffer
import scala.math._
import java.awt.Graphics2D
import java.awt.Color

object Space {
  val G = 6.67384 * pow(10, -11) //gravitational constant
  val TIMESTEP = 60*60  //one hour
  val AU = 1.4960*pow(10,8)*1000 //meters
  var SCALE =  200 / AU //scale
  var viewAngle = 0
  var viewScale = 0



  val spaceObjects = Buffer[Spaceobject]()

  val aurinko = Spaceobject("Sun", new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0), 1.989*pow(10,30), 30.0, new Color(255,180,0), false)
  val maa = Spaceobject("Earth", new Vector3D(AU,0,0), new Vector3D(0.0,29.78*1000,0.0), 5.9737*pow(10,24), 9.0, new Color(0,100,255), false)
  val kuu = Spaceobject("Moon", new Vector3D(1.00257*AU,0,0), new Vector3D(0.0,30.8022*1000,0.0), 7.342*pow(10,22), 4.0, new Color(192,192,102), false)
  val merkurius = Spaceobject("Mercury", new Vector3D(0.3871*AU,0,0), new Vector3D(0.0,47.36*1000,0.0), 330.1*pow(10,21), 5.0, new Color(192,192,192), false)
  val venus = Spaceobject("Venus", new Vector3D(0.7233*AU,0,0), new Vector3D(0.0,35.02*1000,0.0), 4.8685*pow(10,24), 9.0, new Color(242,208,117), false)
  val mars = Spaceobject("Mars", new Vector3D(1.5237*AU,0,0), new Vector3D(0.0,24.08*1000,0.0), 6.4185*pow(10,23), 6.5, new Color(255,50,0), false)
  val jupiter = Spaceobject("Jupiter", new Vector3D(5.2034*AU,0,0), new Vector3D(0.0,13.07*1000,0.0), 1.899*pow(10,27), 20.0, new Color(240,130,0), false)
  val saturnus = Spaceobject("Saturn", new Vector3D(9.582*AU,0,0), new Vector3D(0.0,9.672*1000,0.0), 5.688*pow(10,26), 18.0, new Color(240,150,117), false)
  val uranus = Spaceobject("Uranus", new Vector3D(19.19*AU,0,0), new Vector3D(0.0,6.835*1000,0.0), 8.686*pow(10,25), 15.0, new Color(100,150,255), false)
  val neptunus = Spaceobject("Neptune", new Vector3D(30.07*AU,0,0), new Vector3D(0.0,5.478*1000,0.0), 1.024*pow(10,26), 15.0, new Color(70,0,255), false)



  spaceObjects += aurinko
  spaceObjects += maa
  spaceObjects += merkurius
  spaceObjects += kuu
  spaceObjects += venus
  spaceObjects += mars
  spaceObjects += jupiter
  spaceObjects += saturnus
  spaceObjects += uranus
  spaceObjects += neptunus


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
    val velocityX = totalForceX / a.mass * TIMESTEP
    val velocityY = totalForceY / a.mass * TIMESTEP
    val velocityZ = totalForceZ / a.mass * TIMESTEP

    //Calculates new position and velocity vectors to a
    a.velocity = a.velocity.+(new Vector3D(velocityX, velocityY, velocityZ))
    a.position = a.position.+(new Vector3D(a.velocity.x*TIMESTEP, a.velocity.y*TIMESTEP, a.velocity.z*TIMESTEP))
    a.scaledPos = a.scaledPos.+(new Vector3D(a.velocity.x*TIMESTEP*SCALE, a.velocity.y*TIMESTEP*SCALE, a.velocity.z*TIMESTEP*SCALE))
  }

  def step() = spaceObjects.foreach(updatePosition(_))

  def draw(g: Graphics2D) = spaceObjects.foreach(_.draw(g, viewAngle, viewScale))

}
