package rational

class Rational(n:Int, d:Int) {
	require(d != 0)
	private def gcd(a:Int, b:Int):Int ={
		if(a>b){
			gcd(b, a);
		}
		if(a == 0) return b;
		else {
			var a1 = b%a
			gcd(a1, a)
		}
	}
	private val divtor = gcd(n, d);
	val mn = n/divtor
	val md = d/divtor
	
	def this(n:Int) = this(n, 1)
	def +(other:Rational) = {
		new Rational(this.mn*other.md+this.md*other.mn, this.md*other.md);
	}
	
	def *(other:Rational) = {
		new Rational(mn*other.mn, md*other.md)
	}
	
	override def toString():String = {
		return mn.toString + " / " + md.toString
	} 
}