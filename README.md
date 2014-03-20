Halk
====

Halk is a live coding extension for Haxe programming language.

To use it you need to implement ILive interface into your class.
This makes all your methods (except the constructor) in that class "live".

You also need to include `hscript` library and add `-D halk`, `--no-inline` to your compile parameters.

For openfl use:
```
<haxelib name="hscript" />
<haxedef name="halk"/>
<haxeflag name="--no-inline" />
```
`<haxeflag name="-dce" value="no" />` is also recommended

Required files are in `/src/halk`

You shoud edit `Settings.hx` file to fit your needs.
Make sure that `Settings.getOutPath()` directory is available.


While compiling, halk will create script file for methods on your "live" classes. When executing, halk will try to fetch and parse new script files.
If script file is not found, your class will run original compiled code. When script file is loaded, all methods in that class will be run on hscript interpreter.

Now you can edit your code and compile it to create new script file. That script will be reloaded on your program that is already running.

`@noLive` Exclude method from "live"

`@liveUpdate` Whenever the script file gets updated, call this method.


### Things that are not working:

- function.bind
- changing function parameters (inline functions work fine)
- @liveUpdate on static methods
- live constructor
- different target platforms running at the same time
