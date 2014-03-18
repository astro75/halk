package halk;

class Settings
{

	public static function getUrl():String
	{
		return "http://localhost:2222/";
	}
	
	public static function getOutPath():String
	{
		// run http server in this path (nekotools server -h 0.0.0.0 -p 2222)
		return "C:/halk/";
	}
	
	public static function afterHalkGenerate():Void
	{
		#if halk_stop
			// throws an error to prevent compilation
			throw "halk: Script file created";
		#end
	}
	
}