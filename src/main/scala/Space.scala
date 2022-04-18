import scala.collection.mutable.Buffer
import scala.math._

class Space {
  val G = 6.67384 * pow(10, -11) //gravitational constant
  val DAY = 24*60*60  //one day

  //RK4 shit
  /**
  var time = 0.0
  val dt = 0.1
  */

  val spaceObjects = Buffer[Spaceobject]()

  def distance(a:Spaceobject, b:Spaceobject) = (a.position.-(b.position)).length

  def gravitationForces(a: Spaceobject, b: Spaceobject) = {
    var forceX = 0.0
    var forceY = 0.0
    var forceZ = 0.0

    val r = distance(a,b)
    val totalForce = (G*a.mass*b.mass)/pow(r,2)

    if (a.position.x < b.position.x) forceX = abs(totalForce*(a.position.x-b.position.x)/r)
    else forceX = -1*abs(totalForce*(a.position.x-b.position.x)/r)

    if (a.position.y < b.position.y) forceY = abs(totalForce*(a.position.y-b.position.y)/r)
    else forceY = -1*abs(totalForce*(a.position.y-b.position.y)/r)

    if (a.position.z < b.position.z) forceZ = abs(totalForce*(a.position.z-b.position.z)/r)
    else forceZ = -1*abs(totalForce*(a.position.z-b.position.z)/r)

    (forceX, forceY, forceZ)
  }

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
  }



  // Tried RK4, too hard for me atm :))
  /**
  //calculates accleration
  def acceleration(a: Spaceobject) = {
    var accX = 0.0
    var accY = 0.0
    var accZ = 0.0
    for (b <- spaceObjects) {
      if (a.name != b.name) {
        val totalDistance = (a.position.-(b.position)).length
        val gravitationalAcc = G*b.mass / pow(totalDistance, 3)

        if (a.position.x < b.position.x) accX += abs(gravitationalAcc * (a.position.x - b.position.x))
        else accX += -1*abs(gravitationalAcc * (a.position.x - b.position.x))

        if (a.position.y < b.position.y) accY += abs(gravitationalAcc * (a.position.y - b.position.y))
        else accY += -1*abs(gravitationalAcc * (a.position.y - b.position.y))

        if (a.position.z < b.position.z) accZ += abs(gravitationalAcc * (a.position.z - b.position.z))
        else accZ += -1*abs(gravitationalAcc * (a.position.z - b.position.z))
      }
    }
    new Vector3D(accX, accY, accZ)
  }

  def evaluate(a: Spaceobject, t:Double, dt: Double, dx: Vector3D, dv: Vector3D) = {

    //positions
    a.position = a.position.+(dx.multiply(dt))
    a.velocity = a.velocity.+(dv.multiply(dt))

    val outputdx = a.velocity
    val outputdv = acceleration(a)
    (outputdx, outputdv) // for now ???
  }

  def RK4(a:Spaceobject) = {

    val (k1dx, k1dv) = evaluate(a, time, 0.0, new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0))
    val (k2dx, k2dv) =  evaluate(a, time, 0.5*dt, k1dx, k1dv)
    val (k3dx, k3dv) =  evaluate(a, time, 0.5*dt, k2dx, k2dv)
    val (k4dx, k4dv) =  evaluate(a, time, dt, k3dx, k3dv)

    val dxdt = (k1dx.+(k2dx.multiply(2.0)).+(k3dx.multiply(2.0)).+(k4dx)).multiply(dt/6.0)
    val dvdt = (k1dv.+(k2dv.multiply(2.0)).+(k3dv.multiply(2.0)).+(k4dv)).multiply(dt/6.0)

    a.position = a.position.+(dxdt.multiply(dt))
    a.velocity = a.velocity.+(dvdt.multiply(dt))
  }
  */

}
