package fileuse
 
import java.io.File
import java.io.Console
import java.io

object FileTest {
	
	val cmdMap:Map[String, String=>String] = Map("ls"->listFile, "cd"->changeDir, "cat"->catfile)
	
	var mcwd = new java.io.File( "." ).getCanonicalPath();
	
	def interact(){
		print(mcwd+">>> ")
		val str = readLine();
		if(str == "exit"){
			return
		}
		
		var strs = str.split(" ").toList
		var lns = strs.foldLeft(List.empty:List[String] )( (acc, item)=>if(item == "")acc else (acc++List(item) ) )
		if(lns.length == 0){
			interact()
		}
		
		var opcmd = cmdMap.get(lns(0) )
		var arg = lns.tail.foldLeft("")((acc, item)=>acc+" "+item )
		var ret = opcmd match{
			case Some(cmd) => cmd(arg.trim)
			case _ => "not a valid command"
		}
		println(ret)
		interact
	}
	
	
	def changeDir(dir:String):String = {
		var dir1 = mcwd+"\\"+dir.trim
		val f = new File(dir1);
		if(!f.exists() ) "no a valid file"
		else if(!f.isDirectory() ) "no a directory"
		else {
			mcwd = f.getCanonicalPath();
			"now change to "+mcwd
		}
	}
	
	def listFile(is:String):String = {
		var f = new File(mcwd+"/"+is);
		if(!f.exists) return "no a exist file"
		var fl = f.list
		var ret = ""
		for(fitem<-fl){
			ret += new File(fitem).getPath;
			ret += "\n";
		}
		ret
	}
	
	def catfile(is:String):String = {
		var f = new File(mcwd+"/"+is);
		var file = new io.FileInputStream(f);
		var buf = new Array[Byte](file.available);  
		file.read(buf, 0, file.available()); //一次性读入
		var str = new String(buf);
		str
	}
}





