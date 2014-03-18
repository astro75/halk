package halk;
import hscript.Interp;

class HalkInterp extends Interp
{

	public function new() 
	{
		super();
	}
	
	#if flash
	override private function fcall(o:Dynamic, f:String, args:Array<Dynamic>):Dynamic 
	{
		if (o == Std && f == "int") f = "_int";
		return super.fcall(o, f, args);
	}
	#end
	
}