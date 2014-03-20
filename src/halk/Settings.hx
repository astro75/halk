package halk;

class Settings
{
	
	public static var checkInterval:Int = 500; // miliseconds

	public static function getUrl():String
	{
		// this path will be used to get script files from your program
		return getOutPath();
		
		// return "http://localhost:2222/";
		// for remote targets you will need to run http server
		// you already have a simple server (nekotools server -h 0.0.0.0 -p 2222)
	}
	
	public static function getOutPath():String
	{
		// this is where script files will be saved
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