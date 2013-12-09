import example.profiling._
import example.profiling.power._

object HelloProfileRunner extends ProfileApplicationRunner with HelloProfile
trait HelloProfile extends ProfileApplication {
  
  def main() = {
    /*
    var acc = 0.
    val time =
      profile (100) times {
        
        for (i <- 0 until 100000) {
          acc += Math.exp(i)*Math.pow(i,10.0)*42.0
        }
        
        
      } report average

    println("\n\naverage loop time: " + time)
    */
    
    /*
    new PowerA with ArithStr {
      println {
        power("(x0+x1)",4)
      }
    }
    */
    
    new PowerA2 with ArithExpOpt with ScalaGenArith
    
  }
  
}

