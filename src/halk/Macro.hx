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
 *  - function.bind
 *
 * Important:
 *  - "cast(a, Type)" is replaced with a;
 *  - $type(a) is replaced with "a", but compiller still shows variable type
 *  - var a = 1, b = 2; is replaced with var b = 2; var a = 1;
 */
class Macro
{
#if macro
	static inline var ENUM_OFFSET:Int = 1000000;
	static var methods:Map<String, Array<MethodData>> = new Map();

	static var inited:Null<Bool> = null;
	static var firstBuild = true;

	// create a file with all scripts once per compilation
	static function onGenerate(types:Array<Type>) {

		trace("inited: " + types.length + " inited: " + inited);
		if (inited) return;
		inited = true;

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

		for (tn in methods.keys()) {

			var type = null;
			for (t in classTypes) {
				if (typeName(t) == tn) type = t;
			}
			if (type == null) throw "unknown type: " + tn;

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
				
				//trace(expr1.toString());
				
				switch (expr1.expr) {
					case EFunction(_, f):
						expr1 = f.expr;
					case _:
				}
				
				var expr2 = process(expr1);
				var body = expr2.toString();
				
				res.push('${m.name}:function(${m.args})$body');
			}
		}

		var script = "{" + res.join(",\n") + "\n}";

		File.saveContent(Settings.getOutPath() + "index.hs", script);
		
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

	public static function build()
	{
		if (!Context.defined("halk")) return Context.getBuildFields();
		//trace("build");
		
		Context.onGenerate(onGenerate);
		if ( firstBuild ) {
			firstBuild = false;
			Context.onMacroContextReused(function() {
				inited = false;
				methods = new Map();
				return true;
			});
		}
		var fields = Context.getBuildFields();
		var newFields = new Array<Field>();
		
		if (fields.length == 0) return fields;

		var type = Context.getLocalClass().get();

		var tn = typeName(type);

		var ctor = null;
		var liveListeners = [];
		var firstField = null;

		var res = [];
		for (field in fields)
		{
			if (firstField == null) firstField = field;
			if (field.name == "new") ctor = field;

			if (!field.meta.exists(function(m) return m.name=="noLive"))
			{
				
				switch (field.kind)
				{
					case FFun(f):
						var isStatic = field.access.has(AStatic);
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
							//f.expr = macro $b { inside };
						} else {
							var m = macro { 
								if (halk.Live.instance.getField($v { name } ) != null) 
									return halk.Live.instance.call($i{thisVal}, $v { name }, $a { args } ); 
							};
							//m = { expr:EMeta({pos:m.pos, params:[], name:"noLive"}, m), pos:m.pos };
							inside.unshift(m);
							//f.expr = macro $b { inside };
							}
						//f.expr = macro live.Live.instance.call(this, $v { name }, $v { args } );
						
						if (field.meta.exists(function(m) return m.name == "liveUpdate")) {
							if (isStatic) Context.error("liveUpdate on static metods is not supported", field.pos);
							if (f.args.length > 0) Context.error("liveUpdate method doesn't support arguments", field.pos);
							liveListeners.push(field.name);
						}
						
						if (field.access.has(AOverride)) {
							//trace(field);
							// sreate method for super.fun() calls
							var params = [ for (a in f.args) { expr:EConst(CIdent(a.name)), pos:f.expr.pos } ];
							var name = field.name;
							var m = macro super.$name($a { params } );
							if (!isVoid) m = macro return $m;
							newFields.push( { name:"`s`" + field.name, access:[APublic], pos:f.expr.pos,
								kind:FFun( { args:f.args, ret:f.ret, expr:m, params:f.params } )
							});
						}
						
					case _:
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

		return fields.concat(newFields);
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
					null; // Remove inserted halk helper code
					
				// TEnumParameter hacky fix
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

				case ESwitch(e, cases, edef): // Replace switch with if else 
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

				case ECast(e, t): // remove casts
					e.map(processExpr);

				case EVars(vars):

					if (vars.length > 1 && aVars == null) Context.error("Live doesn't support multiple vars on one line", expr.pos);
					var i = 0;
					for (v in vars) {
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

				case ECall( e = { expr:EConst(CIdent(name)) }, params):

					if (name == "$type") processExpr(params[0]);
					else {
						expr.map(processExpr);
					}

				case ECall( { expr:EField(e, field) }, params):

					params = params.map(processExpr);
					
					
					switch(e.expr) {
						case EConst(CIdent("super")):
							var name = "`s`" + field;
							return macro this.$name($a{params});
						case _:
					}

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
					expr.map(processExpr);
				
				case ENew(t, params):
					{expr:ENew( { name:t.name, pack:t.pack, params:[], sub:t.sub }, params.map(processExpr)), pos:expr.pos };

				case _:
					expr.map(processExpr);
			}
		}

		return processExpr(expr);
	}
	
#end
}