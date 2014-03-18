package halk;

import haxe.Timer;
import hscript.Parser;
import hscript.Interp;

class Live
{
	
	var url:String;
	var path:String;
	var parser:Parser;
	var interp:Interp;
	var script:String;
	var methods:Dynamic;
	
	static public var instance(default, null):Live = new Live();
	
	function new()
	{
		url = Settings.getUrl() + "index.hs";
		path = Settings.getOutPath() + "index.hs";
		parser = new Parser();
		if (parser.identChars.indexOf("`") == -1) parser.identChars += "`";
		//parser.allowTypes = true;
		parser.allowJSON = true;
		interp = new HalkInterp();
		methods = { };
		listeners = [];
		
		interp.variables.set("this", null);
		interp.variables.set("`c", Live.getClass);

		#if sys delayed(load, 500); #else
		load(); #end
	}
	
	private static function getClass(n:String) 
	{
		var ref:Dynamic = null;
		try {
			ref = Type.resolveClass(n);
		} catch (e:Dynamic) { }
		
		if (ref == null) {
			try {
				ref = Type.resolveEnum(n);
			} catch (e:Dynamic) { }
		}
		
		return ref;
		
	}
	
	
	#if sys
	function delayed(f:Dynamic, time) {
		#if neko neko.vm.Thread #else cpp.vm.Thread #end
		.create(function() {
		Sys.sleep(time / 1000);
			f();
		});
	}
	#end

	function load()
	{
		#if (sys)
		var data = sys.io.File.getContent(path);
		parse(data);
		delayed(load, 500);
		#else
		
		var http = new haxe.Http(url + "?r="+ (Timer.stamp() * 10e6));
		http.onData = function(data) {
			parse(http.responseData);
			haxe.Timer.delay(load, 500);
		}
		http.onError = function(data) {
			trace('can\'t load "$url" file');
			//parse(http.responseData);
			haxe.Timer.delay(load, 500);
		}
		http.request();
		
		#end
	}

	function parse(data:String)
	{
		if (data == script) return;
		script = data;
		// trace("parse: " + data);
		
		var nmethods = null;
		#if !debug try { #end
			var program = parser.parseString(script);
			nmethods = interp.execute(program);
		#if !debug }
		catch (e:Dynamic) { 
			trace("hscript: Error in live code");
			trace(e);
		}#end
		
		if (nmethods != null) {
			methods = nmethods;
			for (l in listeners) l();
		}
	}
	
	var listeners:Array<Dynamic>;
	
	public function addListener(f:Dynamic) {
		listeners.remove(f);
		listeners.push(f);
	}
	
	public function removeListener(f:Dynamic) {
		listeners.remove(f);
	}
	
	public function getField(method)
	{
		return Reflect.field(methods, method);
	}
	
	public function callVoid(instance:Dynamic, method:String, args:Array<Dynamic>):Void
	{		
		var prev = interp.variables.get("this");
		interp.variables.set("this", instance);
		#if !debug try { #end
			 Reflect.callMethod(instance, getField(method), args);
		#if !debug } catch (e:Dynamic) {
			trace("hscript: execute error");
			trace(e);
		}
		#end
		interp.variables.set("this", prev);
	}

	public function call(instance:Dynamic, method:String, args:Array<Dynamic>):Dynamic
	{
		var prev = interp.variables.get("this");
		interp.variables.set("this", instance);
		var ret:Dynamic = null;
		#if !debug try { #end
			ret = Reflect.callMethod(instance, getField(method), args);
		#if !debug } catch (e:Dynamic) {
			trace("hscript: execute error");
			trace(e);
		}
		#end
		interp.variables.set("this", prev);
		return ret;
	}
}