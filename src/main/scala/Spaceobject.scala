import Space.{G, distance}

import java.awt.Graphics2D
import java.awt.geom._
import scala.math.{abs, pow}

case class Spaceobject(val name:String, var position: Vector3D, var velocity: Vector3D, val mass: Double) {

  val SCALE = 200/ (1.4960*pow(10,11))

  var scaledPos = new Vector3D(position.x*SCALE, position.y*SCALE, position.z*SCALE)

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

  def draw(g: Graphics2D) = {
    val oldTransform = g.getTransform()

    g.translate(scaledPos.x.toInt+500/2, scaledPos.y.toInt+500/2)
    g.fill(new Ellipse2D.Double(30.0, 30.0, 40.0, 40.0))

    g.setTransform(oldTransform)
  }

}
