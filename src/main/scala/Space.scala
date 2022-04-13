import scala.collection.mutable.Buffer
import scala.math._

class Space {
  val G = 6.67384 * pow(10, -11) //gravitational constant
  val DAY = 24*60*60  //one day
  var time = 0.0
  val dt = 0.1

  val spaceObjects = Buffer[Spaceobject]()

  //calculates accleration
  def acceleration(a: Spaceobject, b: Spaceobject) = {

    val totalDistance = (a.position.-(b.position)).length
    val gravitationalAcc = G*b.mass / pow(totalDistance, 3)

    val accX = gravitationalAcc * (a.position.x - b.position.x)
    val accY = gravitationalAcc * (a.position.y - b.position.y)
    val accZ = gravitationalAcc * (a.position.z - b.position.z)
    // acceleration x, y, z ???
  }

  def evaluate(a: Spaceobject, t:Double, dt: Double, dx: Vector3D, dv: Vector3D) = {

    //positions
    a.position = a.position.+(dt*dx)
    a.velocity = a.velocity.+(dt*dv)

    val outputdx = a.velocity
    //val outputdv = acceleration() ???
    (new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0)) // for now ???
  }

  def RK4(a:Spaceobject) = {

    val (k1dx, k1dv) = evaluate(a, time, 0.0, new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0))
    val (k2dx, k2dv) =  evaluate(a, time, 0.5*dt, k1dx, k1dv)
    val (k3dx, k3dv) =  evaluate(a, time, 0.5*dt, k2dx, k2dv)
    val (k4dx, k4dv) =  evaluate(a, time, dt, k3dx, k3dv)

    val dxdt = (k1dx.+(2.0*k2dx).+(2.0*k3dx).+(k4dx)).multiply(dt/6.0)
    val dvdt = (k1dv.+(2.0*k2dv).+(2.0*k3dv).+(k4dv)).multiply(dt/6.0)

    a.position = a.position.+(dt*dxdt)
    a.velocity = a.velocity.+(dt*dvdt)
  }



  //NOT RK4 !!!

  /**
  def updatePositions(a:Spaceobject, planets: Buffer[Spaceobject]): Unit = {
    var totalForceX = 0.0
    var totalForceY = 0.0
    var totalForceZ = 0.0

    for (i <- planets) {
      if (a.name != i.name) {
        val (forceX, forceY, forceZ) = a.gravitationForces(a,i)
        totalForceX += forceX
        totalForceY += forceY
        totalForceZ += forceZ
      }
    }
    val velocityX = totalForceX / a.mass * STEP
    val velocityY = totalForceY / a.mass * STEP
    val velocityZ = totalForceZ / a.mass * STEP

    //Calculates new position and velocity vectors to a
    a.velocity = a.velocity.+(new Vector3D(velocityX, velocityY, velocityZ))
    a.position = a.position.+(new Vector3D(velocityX*STEP, velocityY*STEP, velocityZ*STEP))
  }
  */
}
