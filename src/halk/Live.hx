package halk;

import haxe.Timer;
import hscript.Parser;
import hscript.Interp;

//@:build(halk.Macro.buildLive()) 
class Live
{
	
	var url:String;
	var path:String;
	var parser:Parser;
	var interp:Interp;
	var script:String;
	var methods:Dynamic;
	
	static public var instance(default, null):Live = new Live();
	
	public static function callField(d:Dynamic, n:String, args:Array<Dynamic>) {
		#if (flash) if (d == Std && n == "int") n = "_int"; #end
		return Reflect.callMethod(d, Reflect.getProperty(d, n), args);
	}
	
	static function setProperty(d:Dynamic, n:String, v:Dynamic) {
		Reflect.setProperty(d, n, v);
		return v;
	}
	
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

		interp.variables.set("callField", Live.callField);
		interp.variables.set("callMethod", Reflect.callMethod);
		interp.variables.set("getProperty", Reflect.getProperty);
		interp.variables.set("setProperty", setProperty);
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
			var types:Array<String> = Reflect.field(nmethods, "___types___");
			//trace(types);
			var ok = true;
			if (types != null) {
				var i = 0;
				while (i < types.length) {
					var n = types[i++];
					trace(n);
					var ref:Dynamic = null;
					try {
						ref = Type.resolveClass(n);
					} catch (e:Dynamic) { }
					
					if (ref == null) {
						//ok = false;
						trace("can't find type in app: '" + n + "'");
						try {
							ref = Type.resolveEnum(n);
						} catch (e:Dynamic) { }
					}
					
					if (ref == null) {
					}
					else {
						var arr = n.split(".");
						if (arr.length == 1) interp.variables.set(n, ref);
						else {
							var res:Dynamic;
							var root = arr.shift();
							var last = arr.pop();
							if (interp.variables.exists(root)) {
								res = interp.variables.get(root);
							} else {
								interp.variables.set(root, res = { } );
							}
							for (s in arr) {
								if (Reflect.hasField(res, s)) {
									res = Reflect.field(res, s);
								} else {
									var child = { };
									Reflect.setField(res, s, child );
									res = child;
								}
							}
							Reflect.setField(res, last, ref);
						}
					}
				}
			}
			
			if (ok) {
				methods = nmethods;
				for (l in listeners) l();
			}
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