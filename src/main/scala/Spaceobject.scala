import Space.{G, distance}

import java.awt.Graphics2D
import java.awt.geom._
import scala.math._
import java.awt.Color

case class Spaceobject(val name:String, var position: Vector3D, var velocity: Vector3D, val mass: Double, val diameter: Double, val color: Color, val satellite: Boolean) {

  var scaledPos = new Vector3D(position.x*Space.SCALE, position.y*Space.SCALE, position.z*Space.SCALE)


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

  def draw(g: Graphics2D, view: Int, scale: Int) = {

    g.setColor(color)
    val oldTransform = g.getTransform()

    if (view%2 ==0)  {
      if (scale%5 ==0) {
        g.translate(scaledPos.x.toInt+700/2, scaledPos.y.toInt+700/2)
        g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
      } else if (scale%5 ==1) {
        g.translate(scaledPos.x.toInt/2+700/2, scaledPos.y.toInt/2+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/4), ceil(-diameter/4), ceil(diameter/2), ceil(diameter/2)))
      } else if (scale%5 ==2) {
        g.translate(scaledPos.x.toInt/4+700/2, scaledPos.y.toInt/4+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/6), ceil(-diameter/6), ceil(diameter/3), ceil(diameter/3)))
      } else if (scale%5 ==3) {
        g.translate(scaledPos.x.toInt/8+700/2, scaledPos.y.toInt/8+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/8), ceil(-diameter/8), ceil(diameter/4), ceil(diameter/4)))
      } else {
        g.translate(scaledPos.x.toInt/16+700/2, scaledPos.y.toInt/16+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/10), ceil(-diameter/10), ceil(diameter/5), ceil(diameter/5)))
      }
    }
    else {
      if (scale%5 ==0) {
        g.translate(scaledPos.x.toInt+700/2, scaledPos.z.toInt+700/2)
        g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
      } else if (scale%5 ==1) {
        g.translate(scaledPos.x.toInt/2+700/2, scaledPos.z.toInt/2+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/4), ceil(-diameter/4), ceil(diameter/2), ceil(diameter/2)))
      } else if (scale%5 ==2) {
        g.translate(scaledPos.x.toInt/4+700/2, scaledPos.z.toInt/4+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/6), ceil(-diameter/6), ceil(diameter/3), ceil(diameter/3)))
      } else if (scale%5 ==3) {
        g.translate(scaledPos.x.toInt/8+700/2, scaledPos.z.toInt/8+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/8), ceil(-diameter/8), ceil(diameter/4), ceil(diameter/4)))
      } else {
        g.translate(scaledPos.x.toInt/16+700/2, scaledPos.z.toInt/16+700/2)
        if (satellite) g.fill(new Ellipse2D.Double(-diameter/2, -diameter/2, diameter, diameter))
        else g.fill(new Ellipse2D.Double(ceil(-diameter/10), ceil(-diameter/10), ceil(diameter/5), ceil(diameter/5)))
      }
    }

    g.setTransform(oldTransform)
  }

}
