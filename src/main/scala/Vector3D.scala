import scala.math._


class Vector3D (val x:Double, val y:Double, val z:Double){

  var length = sqrt(pow(x,2) + pow(y,2) + pow(z,2))

  def + (other: Vector3D) = {
    new Vector3D(x + other.x, y + other.y, z + other.z)
  }

  def - (other: Vector3D) = {
    new Vector3D(x - other.x, y - other.y, z - other.z)
  }

  def multiply(constant: Double) = {
    new Vector3D(constant * x, constant * y, constant * z)
  }

}
