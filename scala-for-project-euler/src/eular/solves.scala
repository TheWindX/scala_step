package eular

import scala.collection.mutable._


object solves {
	//Add all the natural numbers below one thousand that are multiples of 3 or 5.*
	def p1(n:Int) = {
		(1 until n).view.filter(x=>x % 3 != 0 && x%5 != 0).sum
	}
	
	//Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.*
	def p2(limit:Int):Int = {
		var f = ArrayBuffer[Int]()
		f += 1
		f.append(1)
		def n(idx:Int) = { var n = f(idx-1)+f(idx-2); f+=n; n}
		var idx = 2
		var oldv = 2
		var newv = 2
		
		while(newv<limit){
			oldv = newv
			newv += n(idx)
			idx += 1
		}
		oldv
	}
	
	def p3(n:Long):Long = {
		def plist(n:Long):List[Long] = (2 to math.sqrt(n).toInt ).find(n%_==0)
				.map(x=>x.toLong::plist(n/x) ).getOrElse(List(n));
		plist(n).last
	}
	
	//Find the largest palindrome made from the product of two 3-digit numbers.*
	def p4(n:Int) = {
		def rn(x:Int) = {
			if(x == 1)(1, 9)
			else if(x==2)(10,99)
			else if(x==3)(100, 999)
			else (0, 0)
		}
		
		var (l, r) = rn(n)
		var ret = List[Int]()
		for(v1<-(l to r))
			for(v2<-(l to r))
			{
				var s = (v1*v2).toString
				if(s.reverse == s)
					ret = (v1*v2)::ret 
			}
		ret.max
	}
	
	//What is the smallest number divisible by each of the numbers 1 to 20?*
	def p5(l:Int, r:Int) = {
		def ok(n:Int) = (l to r).forall(n%_==0)
		var i = 1
		while(!ok(i) ){
			//println("i="+i)
			i += 1
		}
		i
	}
	
	//What is the difference between the sum of the squares and the square of the sums?* 1~100
	def p6(n:Int) = {
		def square(n:Int) = n*n
		var v1 = square( (1 to n).sum )
		var v2 = (1 to n).map(square(_) ).sum
		v1-v2
	}
	
	//Find the 10001st prime.*
	def p7(n:Int) = {
		object plist {
			val prims = Stream.cons(2, checkfrom(3));
			def checkfrom(n: Int):Stream[Int] ={
					val testps = prims.takeWhile(_ <= math.floor(math.sqrt(n)).toInt);
					val res = testps.foldLeft(true){(acc, e)=>(n%e != 0)&&acc}
					if(res) Stream.cons(n, checkfrom(n+2) )
					else checkfrom(n+2);
				}	
		}
		plist.prims(n)	
	}
	
	//Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.*
	def p9(n:Int) = {
		var k = for(a<-(0 to n); b<-(0 to n-a); c<-(1000-a-b to 1000-a-b) if a*a+b*b == c*c ) yield{
			(a,b,c)
		}
		
		k.toList
	}
	
	//Calculate the sum of all the primes below two million.*
	def p10(n:Int) = {
		object plist {
			val prims = Stream.cons(2, checkfrom(3));
			def checkfrom(n: Int):Stream[Int] ={
					val testps = prims.takeWhile(_ <= math.floor(math.sqrt(n)).toInt);
					val res = testps.foldLeft(true){(acc, e)=>(n%e != 0)&&acc}
					if(res) Stream.cons(n, checkfrom(n+2) )
					else checkfrom(n+2);
				}	
		}
		var (l1, l2) = plist.prims.span( _<n )
		l1.sum
	}
}


object Main{
	def main(args:Array[String]):Unit = {
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
}