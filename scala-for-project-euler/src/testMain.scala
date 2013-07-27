import eular.solves
import rational.Rational


class testMain {

}



object Main{
	def testEular(args:Array[String]):Unit = {
//		println (solves.p1(1000))
//		println (solves.p2(4000000) )
//		println (solves.p3(600851475143L) )
//		println (solves.p4(3) )
//		//println (solves.p5(1, 20) )//take long time
//		println (solves.p6(100) )
//		println (solves.p7(10000) )
//		println (solves.p9(1000) )
		println (solves.p10(50) )
	}
	
	def testRational(args:Array[String]):Unit = {
		import rational.Rational
		val v1 = new Rational(20, 100);
		println (v1 + new Rational(20, 120) toString)
		println ((v1 * new Rational(20, 120)) toString)
	}
	
	def testFile(){
		import fileuse.FileTest._
		interact()
	}
	
	def main(args:Array[String]):Unit = {
		testFile
	}
}