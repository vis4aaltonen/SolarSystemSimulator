import scala.math._

object SpaceApp extends App {
  val avaruus = new Space
  val aurinko = new Spaceobject("aurinko", new Vector3D(0.0,0.0,0.0), new Vector3D(0.0,0.0,0.0), 1.989*pow(10,30))
  val maa = new Spaceobject("maa", new Vector3D(1.4960*pow(10,8)*1000,0,0), new Vector3D(0.0,29.8*1000,0.0), 5.9737*pow(10,24))
  avaruus.spaceObjects += aurinko
  avaruus.spaceObjects += maa
  println(avaruus.distance(aurinko, maa))

  for (i <- 0 until 1000) {
    avaruus.updatePosition(maa)
    //if ((i+30)%365==0) println("maan kordinaatit: " + maa.position.x + " ja "+ maa.position.y)
    println(avaruus.distance(maa,aurinko))
  }
}
