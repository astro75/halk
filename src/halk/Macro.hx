package halk;

import haxe.io.Path;
import haxe.macro.Compiler;
import haxe.macro.ComplexTypeTools;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.MacroStringTools;
import haxe.macro.Printer;
import haxe.macro.TypedExprTools;

import haxe.macro.TypeTools;
import sys.FileSystem;
import sys.io.File;

using haxe.macro.Tools;
using Lambda;
using StringTools;
using Type;

import haxe.macro.Type;

private typedef TypeDesc = {
	var methods:Array<String>;
	var smethods:Array<String>;
	var vars:Array<String>;
	var svars:Array<String>;
}

private typedef MethodData = {name:String, name2:String, args:String, method:Expr}

/**
 * Haxe 3 requered
 *
 * These things don't work:
 *  - Enums
 *  - switch
 *  - function declaration inside another function
 *  - array comprehension
 *  - function.bind
 *
 * Important:
 *  - "cast(a, Type)" is replaced with "if(Std.is(a, Type)) a; else throw "can't cast 'a' to 'Type'";
 *  - "a += 10" is replaced with "a = a + 10;" (same thing for "*=", "/=" etc.)
 *  - $type(a) is replaced with "a", but compiller still shows variable type
 *  - var a = 1, b = 2; is replaced with var b = 2; var a = 1;
 */
class Macro
{
#if macro
	static public inline var ENUM_OFFSET:Int = 1000000;
	static var methods:Map<String, Array<MethodData>> = new Map();

	//static var typeDesc:TypeDesc;
	//static var typeCall:Expr;

	static var varTypes:Map < String, Bool > = new Map();

	static var inited:Null<Bool> = null;
	static var firstBuild = true;

	// classes descriptions cache. Improves describeType speed
	static var descCache:Map<String, TypeDesc> = new Map();

	// get class description (all fields and methods)
	static function describeType(t:ClassType):TypeDesc {

		var tn = typeName(t);
		if (descCache.exists(tn)) return descCache[tn];

		var smethods = [];
		var methods = [];
		var svars = [];
		var vars = [];

		for (f in t.statics.get()) {
			switch (f.kind) {
				case FVar(_, _): svars.push(f.name);
				case FMethod(_): smethods.push(f.name);
			}
		}

		for (f in t.fields.get()) {
			switch (f.kind) {
				case FVar(_, _): vars.push(f.name);
				case FMethod(_): methods.push(f.name);
			}
		}

		var res = { smethods:smethods, svars:svars, methods:methods, vars:vars };

		if (t.superClass != null) {

			var s = describeType(t.superClass.t.get());

			res.methods = res.methods.concat(s.methods);
			res.smethods = res.smethods.concat(s.smethods);
			res.vars = res.vars.concat(s.vars);
			res.svars = res.svars.concat(s.svars);
		}

		descCache[tn] = res;

		return res;
	}

	// create a file with all scripts once per compilation
	static function onGenerate(types:Array<Type>) {

		trace("inited: " + types.length + " inited: " + inited);
		if (inited) return;
		inited = true;
		
		/*var exprs = new Array<String>();
		for (type in types) {
			exprs.push((new Printer("  ")).printComplexType(TypeTools.toComplexType(type)));
		}

		if (exprs.length > 0) {
			File.saveContent(getOutPath() + "asd2", exprs.join('\n'));
		}*/
		
		trace("test 1111");

		if (!methods.keys().hasNext()) return;
		

		var classTypes:Array<ClassType> = [];
		for (t in types)
			switch (t) {
				case TInst(t, _):
					var t = t.get();
					//registerType(t.name, t.pack);
					if (!(t.isInterface || t.isExtern)) {
						//trace(typeName(t));
						classTypes.push(t);
					}
				case _:
			}

		var res = [];
		
		trace("test 1111");

		for (tn in methods.keys()) {

			var type = null;
			for (t in classTypes) {
				if (typeName(t) == tn) type = t;
			}
			if (type == null) throw "unknown type: " + tn;

			//typeCall = type.pack.concat([type.name]).toFieldExpr();
			//typeDesc = describeType(type);

			for (m in methods[tn]) {
				
				var expr1 = null;
				for (f in type.fields.get().concat(type.statics.get())) {
					if (f.name == m.name2) {
						switch(f.kind) {
							case FVar(_, _):
							case FMethod(_): 
								expr1 = Context.getTypedExpr(fixTypes(f.expr()));
								//trace(f.expr().toString(true));
						}
					}
				}
				
				if (expr1 == null) continue;
				
				trace(expr1.toString());
				
				switch (expr1.expr) {
					case EFunction(_, f):
						expr1 = f.expr;
					case _:
				}
				
				
				
				//trace(m.name);
				//var body = process(m.method, []).toString();
				//var arr = process(expr1, []).toString().split('\n');
				//if (StringTools.trim(arr[1]) == "var ` = this;") arr[
				var expr2 = process(expr1);
				//var body = Context.typeExpr(expr2).toString(true);
				var body = expr2.toString();
				//var body = expr1.toString();
				res.push('${m.name}:function(${m.args})$body');
			}
		}

		var script = "{" + res.join(",\n");
		if (varTypes.keys().hasNext())
			script += ",\n___types___:[\"" + [for (n in varTypes.keys()) n].join("\", \"") + "\"]";
		script += "\n}";

		File.saveContent(Settings.getOutPath() + "index.hs", script);
		
		//trace("halk: Scripts generated");
		
		Settings.afterHalkGenerate();
	}
	
	static private function fullTypePath(pack, name) {
		return (pack.length > 0 ? pack.join(".") + "." : "") + name; 
	}
	
	static private function fixTypes(expr:TypedExpr) 
	{
		if (expr == null)
			return null;
		return switch(expr.expr) {
			case TTypeExpr(m):
				var path = switch(m) {
					case TClassDecl(e):
						var e = e.get();
						fullTypePath(e.pack, e.name);
					case TEnumDecl(e):
						var e = e.get();
						fullTypePath(e.pack, e.name);
					case TTypeDecl(e):
						var e = e.get();
						fullTypePath(e.pack, e.name);
					case TAbstract(e):
						var e = e.get();
						fullTypePath(e.pack, e.name);
				}
				//var m = macro { calltest($i { path } ); };
				//Context.typeExpr( m );
				var tl = TLocal( { t:expr.t, name:"`c", id:99999, capture:false, extra:null } );
				{ expr:TCall( { expr:tl, t:expr.t, pos:expr.pos }, [ { expr:TConst(TString(path)), t:expr.t, pos:expr.pos } ]), t:expr.t, pos:expr.pos };
				//expr;
			case TSwitch(e1, cases, e2): 
				//trace(e1.toString(true));
				//trace(e1.toString());
				TypedExprTools.map( { expr:TSwitch(fixTypes(e1), cases, e2), pos:expr.pos, t:expr.t }, fixTypes);
			case TParenthesis(e1):
				{ expr:TParenthesis(fixTypes(e1)), pos:expr.pos, t:expr.t };
			case TMeta(m, e1):
				trace("META " + m.name + "(" + m.params.toString() + ")");
				fixTypes(e1);
			case TEnumParameter(e1, _, index):
				index += ENUM_OFFSET; // possible failure
				{ expr:TArray(e1, { expr:TConst(TInt(index)), pos:expr.pos, t:expr.t }), pos:expr.pos, t:expr.t };
				//trace(expr.toString(true));
				//Context.error("ERR", Context.currentPos());
				//TypedExprTools.map(expr, fixTypes);
			case _:
				TypedExprTools.map(expr, fixTypes);
		}
	}

	inline static function typeName(type:ClassType) {
		return (type.module.length > 0 ? type.module + "." : "") + type.name;
	}
	
	/*// remove previous url and create new one with our path.
	public static function buildLive() {
		var res = Context.getBuildFields();
		for (f in res) if (f.name == "url") { res.remove(f); break; }

		res.push( { kind:FVar(null, macro $v{getOutPath()}), name:"url", pos:Context.currentPos() }  );
		return res;
	}*/

	public static function build()
	{
		if (!Context.defined("halk")) return Context.getBuildFields();
		//trace("build");
		
		Context.onGenerate(onGenerate);
		if ( firstBuild ) {
			firstBuild = false;
			Context.onMacroContextReused(function() {
				inited = false;
				descCache = new Map();
				varTypes = new Map();
				methods = new Map();
				return true;
			});
		}
		var fields = Context.getBuildFields();
		
		
		/*var exprs = new Array<String>();
		for (field in fields)
			exprs.push((new Printer("  ")).printField(field));

		if (exprs.length > 0) {
			var fo = File.write(getOutPath() + "asd", false);
			fo.writeString(exprs.join('\n'));
			fo.close();
		}*/
		
		if (fields.length == 0) return fields;

		var type = Context.getLocalClass().get();
		//registerType(type.name, type.pack);

		var tn = typeName(type);

		var ctor = null;
		var liveListeners = [];
		var firstField = null;

		var res = [];
		for (field in fields)
		{
			if (firstField == null) firstField = field;
			if (field.name == "new") ctor = field;

			//if (field.meta.exists(function(m) return m.name=="live" || m.name=="liveUpdate") || true)
			if (!field.meta.exists(function(m) return m.name=="noLive"))
			{
				
				switch (field.kind)
				{
					case FFun(f):
						var isStatic = field.access.has(AStatic);
						//if (field.access.has(AStatic)) continue; // Context.error("static method can't be live", f.expr.pos);
						if (f.expr == null) continue;
						var inside = null;
						switch (f.expr.expr) {
							case EBlock(exprs):
								if (exprs.length == 0) continue;
								inside = exprs;
							case _:
						}

						var name = tn.replace(".", "_") + "_" + field.name;
						res.push({
							name : name,
							name2: field.name,
							args : [for (a in f.args) a.name].join(","),
							method : f.expr,
						});
						var args = [for (a in f.args) macro $i { a.name } ];
						
						//trace(name);
						
						//trace(ComplexTypeTools.toString(TypeTools.follow(field)));
						
						var thisVal = isStatic ? "null" : "this";
						
						var isVoid = !hasReturn(f.expr);
						if (isVoid) {
							var m = macro { 
									if (halk.Live.instance.getField($v { name } ) != null) {
										halk.Live.instance.callVoid($i{thisVal}, $v { name }, $a { args } ); 
										return;
									};
								};
							//m = { expr:EMeta({pos:m.pos, params:[], name:"noLive"}, m), pos:m.pos };
							inside.unshift(m);
							f.expr = macro $b { inside };
						} else {
							var m = macro { 
								if (halk.Live.instance.getField($v { name } ) != null) 
									return halk.Live.instance.call($i{thisVal}, $v { name }, $a { args } ); 
							};
							//m = { expr:EMeta({pos:m.pos, params:[], name:"noLive"}, m), pos:m.pos };
							inside.unshift(m);
							f.expr = macro $b { inside };
							}
						//f.expr = macro live.Live.instance.call(this, $v { name }, $v { args } );

					case _:

						//Context.error("only methods can be live", field.pos);
				}
			}

			if (field.meta.exists(function(m) return m.name == "liveUpdate")) {
				switch (field.kind) {
					case FFun(f):
						if (f.args.length > 0) Context.error("liveUpdate method doesn't support arguments", field.pos);
						liveListeners.push(field.name);
					case _: Context.error("only methods can be liveUpdate", field.pos);
				}
			}
		}
		if (res.length > 0) methods.set(tn, res);

		if (ctor == null && liveListeners.length > 0) {
			//if class doesn't have a constructor, notify developer
			Context.error("Please define constructor for live listeners", firstField.pos);
		}

		if (liveListeners.length > 0) {
			switch (ctor.kind) {
				case FFun( f ):

					// listeners for live methods' changes to be added to constructor code
					var listeners = [for (l in liveListeners) macro halk.Live.instance.addListener($i { l } )];
					var add = macro $b { listeners };
					
					if (f.expr == null) f.expr = add;
					else {
						var m = f.expr;
						f.expr = macro { $m; $add; };
					}

					ctor.kind = FFun( f );

					var removeListeners = [for (l in liveListeners) macro halk.Live.instance.removeListener($i { l } )];

					// removeLiveListeners method
					fields.push( { name:"removeLiveListeners", access:[APublic], pos:ctor.pos,
						kind:FFun( { args:[], ret:null, expr:macro $b { removeListeners }, params:[] } )
					});
				case _:
			}
		}

		return fields;
	}
	
	static private function hasReturn(expr:Expr):Bool
	{
		var found = false;
		function check(e:Expr) {
			switch(e.expr) {
				case EReturn(ret):
					if (ret != null)
						found = true;
				case EFunction(_, _):
				case _:
					e.iter(check);
			}
		}
		expr.iter(check);
		return found;
	}

	static function process(expr:Expr, ?aVars:Array<Expr>):Expr
	{

		function processExpr(expr:Expr) {
			//trace(expr);
			return switch (expr.expr)
			{
				case EIf({expr:EParenthesis( {expr:EBinop(OpNotEq, {expr:ECall({expr:EField(_, "getField")}, _)} , _)} )}, _, _):
					//trace(e.expr.enumConstructor());
					//trace(f);
					//trace(e.toString());
					null; // Remove inserted code
					
				/*case EMeta(a, _):
					Context.error("META   " + a.name, expr.pos);
					trace("META   " + a.name);
					null;*/
					
				// TEnumParameter
				case EArray(e1, tmp = {expr:EConst(CInt(i))}) if (Std.parseInt(i) >= ENUM_OFFSET):
					var ident = "`c";
					macro $i{ident}("Type").enumParameters(${processExpr(e1)})[$tmp];
					
				case EBlock(exprs):
					var res = [];
					for (e in exprs) {
						var addVars = [];
						var r = process(e, addVars);
						if (r == null) continue;
						res.push(r);
						if (addVars.length > 0) res = res.concat(addVars);
					}
					return {expr:EBlock(res), pos:expr.pos};

				case ESwitch(e, cases, edef):
					e = processExpr(e);
					if (edef != null)
						edef = processExpr(edef);
					var last = edef;
					cases.reverse();
					var ident = "`sw";
					for ( c in cases ) {
						var p = processExpr(c.values.shift());
						var last2 = macro $i{ident} == $p;
						for ( v in c.values ) {
							var p = processExpr(v);
							last2 = macro ($last2) || $i{ident} == $p;
						}
						
						if (c.expr != null) { c.expr = processExpr(c.expr); }
						else c.expr = macro {};
						
						last = {expr:EIf(last2, c.expr, last), pos:e.pos};
					}
					
					macro { var $ident = $e; $last; }
					
					//var s = expr.toString();
					//var a = s.split("\n");
					//if (a.length > 1) s = a[0] + "...";
					//else if (s.length > 20) s = s.substr(0, 20) + "...";
//
					//Context.error("Live doesn't support: " + s, expr.pos);
					
				case EFunction(name, f):
					var func = { 
						ret: null,
						params: [],
						expr: processExpr(f.expr),
						args: f.args.map(function(arg) { arg.type = null; return arg; } ),
					};
					return {expr:EFunction(name, func), pos:expr.pos};

				case  EArrayDecl(t):
					if (t.length > 0) {
						switch (t[0].expr) {
							case EFor(_, _), EWhile(_, _, _):
								Context.error("Live doesn't support complex Array definition", expr.pos);

							case EBinop(OpArrow, _, _):
								Context.error("Live doesn't support inline Map definition", expr.pos);
							case _:
						}
					}
					t = [for (e in t) processExpr(e) ];
					macro $a { t };

				case ECast(e, t):
					if (t == null)
						e.map(processExpr);
					else
					switch (t) {
						case TPath(t2):
							/*var tn = (t.pack.length > 0 ? t.pack.join(".") + "." : "") + t.name
							+ (t.sub == null ? "" : "."+t.sub);
							var type = Context.getType(tn);
							//registerMacroType(type, expr.pos);

							tn = type.toString();*/
							//e = processExpr(e);
							//var msg = "can't cast '" + e.toString() + "' to '" + tn + "'";
							//registerType("Std", []);
							//macro if (Std.is($e, $t )) $e; else throw $v { msg };
							e.map(processExpr);

						case _:
							expr;
					}

				case EVars(vars):

					if (vars.length > 1 && aVars == null) Context.error("Live doesn't support multiple vars on one line", expr.pos);
					var i = 0;
					for (v in vars) {
						if (v.type != null) {
							switch (v.type) {

								case TPath(p):
									var name = (p.pack.length > 0 ? p.pack.join(".") + "." : "") + p.name;
									if (name != "Bool") {
										//var t = Context.getType(name);
										//registerMacroType(t, expr.pos);
									}
								case _:
							}
						}
						v.expr = v.expr != null ? processExpr(v.expr) : v.expr;
						v.type = null;
						
						i++;
						if (i > 1) {
							aVars.push({ expr:EVars([v]), pos:expr.pos } );
						}
					}
					if (i > 1)
						{expr:EVars([vars[0]]), pos:expr.pos };
					else expr;


				/*case EBinop(OpAssignOp(op), e1, e2):  // translate a+=1 to a=a+1
					e1 = processExpr(e1);
					e2 = processExpr(e2);

					var op = { expr:EBinop(op, e1, e2), pos:expr.pos };
					processExpr(macro $e1 = $op);*/

				case ECall( { expr:EConst(CIdent(name)) }, params): // Add "this" for classes

					if (name == "$type") processExpr(params[0]);
					else {
						//if (typeDesc.methods.has(name)) {
							//params = [for (p in params) processExpr(p)];
							//macro callField(this, $v{name}, $a { params } );
						//} else if (typeDesc.smethods.has(name)) {
							//params = [for (p in params) processExpr(p)];
							//macro callField($typeCall, $v { name }, $a { params }  );
						//}
						//else expr.map(processExpr);
						expr.map(processExpr);
					}

				case ECall( { expr:EField(e, field) }, params): // need to register haxe.Log to enable haxe.Log.clear() usage

					params = params.map(processExpr);

					var t = null;
					try {
						t = Context.typeof(e);
					} catch (e:Dynamic) { }

					if (t != null && field == "bind") {
						switch (t) {
							case TFun(_, _):
								Context.error("Live doesn't support function bind", expr.pos);
							case _:
						}
					}

					/*if (t == null) {
						try {
							var s = e.toString();
							if (s.length > 0) s += ".";
							t = Context.getType(s + field);
						} catch (e:Dynamic) {}
					}
					e = processExpr(e);

					if (t != null) {
						var path = registerMacroType(t, expr.pos);
						if (path != null) e = path.split(".").toFieldExpr();
					}*/

					//macro callField($e, $v { field }, $a { params } );
					//{expr:ECall( { expr:EField(processExpr(e), field), pos:expr.pos }, params), pos:expr.pos };
					expr.map(processExpr);

				/*case EConst(CIdent(n)):  // add "this" to identifier if it's a property of current class

					/*var t = null;
					try {
						t = Context.getType(n);
					} catch (e:Dynamic) { }
					if (t == null) {
						try {
							t = Context.typeof(expr);
						}
						catch (e:Dynamic) { }
					}

					var path = null;
					if (t != null) path = registerMacroType(t, expr.pos);*

					//if (localVars.has(n)) expr;
					//else if (typeDesc.vars.has(n))
						//macro getProperty(this, $v{n});
					//else if (typeDesc.svars.has(n))
						//macro getProperty($typeCall, $v { n } );
					//else if (path != null) {
						//macro $p{path.split(".")};
					//} else expr;
					//if (n == "`") {
						//trace(n);
						//{expr:EConst(CIdent("this")), pos:expr.pos };
					//} else {
						expr;
					//}*/
					
				/*case EConst(CString(s)):
					if (MacroStringTools.isFormatExpr(expr)) {
						MacroStringTools.formatString(s, expr.pos);
					} else {
						expr;
					}*/

				/*case EBinop(OpAssign, e1, e2):

					var expr = processExpr(e1);
					var value = processExpr(e2);

					//switch (expr.expr)
					//{
						//case ECall(e, params):
							//switch (e.expr) {
								//case EConst(CIdent("getProperty")):
									//var p1 = params[0];
									//var p2 = params[1];
									//macro setProperty($p1, $p2, $value);
								//case _ : macro $expr = $value;
							//}
						//case EField(e, field):
							//macro setProperty($e, $v{field}, $value);
						//case _:
							macro $expr = $value;
							//expr.map(processExpr);
					//}*/

				/*case EBinop(op, e1, e2):
					e1 = processExpr(e1);
					e2 = processExpr(e2);
					if (StringTools.startsWith(e2.toString(), "-")) {
						// Printer temp fix
						e2 = { expr:EParenthesis(e2), pos:e2.pos };
					}
					
					{expr:EBinop(op, e1, e2), pos:expr.pos };*/
				
				case ENew(t, params):

					//if (t.params.length > 0) Context.error("Live doesn't support type params", expr.pos);
					//trace(t);
					//trace(TypeTools.toString(t));
					
					/*var type = Context.getType((t.pack.length > 0 ? t.pack.join(".") + "." : "") + t.name);
					registerMacroType(type, expr.pos);
					var tn = type.toString();
					var pack = tn.split(".");
					pack.pop();*/

					{expr:ENew( { name:t.name, pack:t.pack, params:[], sub:t.sub }, params.map(processExpr)), pos:expr.pos };

				/*case EField(e, field):
					
					/*var t = null;
					try {
						t = Context.typeof(e);
					} catch (e:Dynamic) { }
					if (t == null) {
						try {
							var s = e.toString();
							if (s.length > 0) s += ".";
							t = Context.getType(s + field);
						} catch (e:Dynamic) {}
					}
					if (t != null) registerMacroType(t, expr.pos);*
					e = processExpr(e);
					//macro getProperty($e, $v{field} );
					{ expr : EField(e, field), pos : expr.pos};*/


				case _:
					expr.map(processExpr);
			}
		}

		return processExpr(expr);
	}

	static function registerMacroType(t:Type, pos:Position):String {
		return null;
		//trace(t);
		t = Context.follow(t, false);
		//trace(t);
		return switch (t) {
			case TType(t, _):

				var t = t.get();
				registerType(t.name, t.pack);
				//{expr:EField(macro $i { n }, field), pos:expr.pos };

			case TInst(t, _):

				var t = t.get();
				registerType(t.name, t.pack);
				//macro $i { n };

			case TMono(t):
				var t = t.get();
				if (t != null) registerMacroType(t, pos);
				null;

			case TAbstract(t, _):

				//var t = t.get();
				//if (t != null) registerMacroType(t.type, pos);
				null;

			case TDynamic(t):
				if (t != null) registerMacroType(t, pos);
				null;

			case TEnum(t, _):
				//var m = haxe.rtti.Meta.getType(t);
				//trace(m);
				var t = t.get();
				registerType(t.name, t.pack);
				//Context.error("Live doesn't support enums", pos);

			case TAnonymous(_), TFun(_, _): null;

			case _:
				throw "Unknown type: " + t + ". Please report to author";
		}
	}

	inline static function registerType(n:String, p:Array<String>):String {

		if (StringTools.startsWith(n, "#")) n = n.substr(1);
		//get full classpath based on package and class name
		if (p.length > 0) n = p.join(".") + "." + n;
		//trace("register: " + n);
		varTypes[n] = true;
		return n;
	}
#end
}