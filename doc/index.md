
# debug

## ` fun ast(str source, str scriptName) > str`
Parse `source` and return the abstract syntax tree in JSON  @param source the buzz source  @param script name (used to fetch eventual extern functions)  @return AST as JSON
# gc

## ` fun allocated() > num`
Returns the number of allocated bytes
## ` fun collect()`
Triggers a GC sweep
# std

## ` fun assert(bool condition, str message)`
If condition is false throw error with given message
## ` fun print(str value)`
Prints value on stdout
## ` fun parseNumber(str string) > num?`
Parse number, returns false if string does not represent a number
## ` fun runFile(str filename)`
Run a buzz file
# math

## ` fun abs(num n) > num`
Returns absolute value of n
## ` fun acos(num n) > num`
Return acos of n
## ` fun asin(num n) > num`
Return asin of n
## ` fun atan(num n) > num`
Return atan of n
## ` fun bzceil(num n) > num`
Return ceiled n
## ` fun bzcos(num n) > num`
Return cos of n
## `pi num`
π constant
## ` fun deg(num n) > num`
Convert radian to degree
## ` fun bzexp(num n) > num`
Return exp of n
## ` fun bzfloor(num n) > num`
Returned floored n
## ` fun bzlog(num base, num n) > num`
Return log(base) of n
## ` fun max(num a, num b) > num`
Return max of a and b
## ` fun min(num a, num b) > num`
Return min of a and b
## ` fun rad(num n) > num`
Convert degree to radian
## ` fun random() > num`
Returns random number between 0 and 1
## ` fun bzsin(num n) > num`
Return sin of n
## ` fun bzsqrt(num n) > num`
Return square root of n
## ` fun bztan(num n) > num`
Return tan of n
# os

## ` fun time() > num`
Returns epoch time in ms
## ` fun env(str key) > str?`
Returns environment variable under [key]
## ` fun tmpDir() > str`
Returns path to system temp directory
## ` fun tmpFilename(str? prefix) > str`
Returns a temporary file name in system tmp dir
## ` fun buzzExit(num exitCode)`
Exit program with [exitCode]
## ` fun execute([str] command) > num`
Execute command and return its exit code
# fs

## ` fun currentDirectory() > str`
Returns current directory absolute path
## ` fun makeDirectory(str path)`
Creates directory path
## ` fun delete(str path)`
Deletes directory or file at path
## ` fun move(str source, str destination)`
Moves/renames file
## ` fun list(str path) > [str]`
List files under path
# io

## ` fun FileOpen(str filename, num mode) > num`
Undocumented
## ` fun FileClose(num fd)`
Undocumented
## ` fun FileReadAll(num fd) > str`
Undocumented
## ` fun FileReadLine(num fd) > str?`
Undocumented
## ` fun FileRead(num fd, num n) > str?`
Undocumented
## ` fun FileWrite(num fd, str bytes)`
Undocumented
## ` fun getStdIn() > num`
Undocumented
## ` fun getStdOut() > num`
Undocumented
## ` fun getStdErr() > num`
Undocumented
## ` enum FileMode`
File mode with which you can open a file
## ` object File`
Object to manipulate an opened file

## ` fun close()`
Close file

## ` fun read(num n) > str?`
Reads n bytes, returns null if nothing to read

## ` fun readAll() > str`
Reads all

## ` fun readLine() > str?`
Reads next line, returns null if nothing to read

## ` fun write(str bytes)`
Write bytes

## ` fun open(str filename, FileMode mode) > File`
Open file  @param filename Path of file to open  @param mode Mode with which to open it

## ` num fd`
File descriptor
## `stdin File`
File opened in read mode
## `stdout File`
File opened in write mode
## `stderr File`
File opened in write mode
# json

## ` object JsonParser`
Parse JSON string into a `Json` tree  @private

## ` str source`


## ` fun peek() > str?`


## ` num offset`


## ` fun skipWhitespaces()`


## ` fun next() > Json`


## ` fun array() > [Json]`


## ` fun map() > {str, Json}`


## ` fun advance() > str?`


## ` fun number(str parsed) > num`


## ` fun string() > str`


## ` fun consume(str expected)`


## ` fun match(str expected) > bool`

## ` object Json`
Utility object to manage data from a JSON string

## ` fun listValue() > [Json]`
Returns wrapped data list value or empty list if not a list

## ` num? number`
When wrapped data is a number

## ` str? string`
When wrapped data is a string

## ` {str, Json}? map`
When wrapped data is an object, object property values are themselves wrapped in a `Json`

## ` [Json]? list`
When wrapped data is a list, list elements are themselves warpped in a `Json`

## ` fun booleanValue() > bool`
Returns wrapped data boolean value or `false` if not a boolean

## ` fun stringValue() > str`
Returns wrapped data string value or empty string if not a string

## ` fun numberValue() > num`
Returns wrapped data number value or `0` if not a number

## ` fun mapValue() > {str, Json}`
Returns wrapped data map value or empty map if not a map

## ` fun encode() > str`
Encode to a JSON string  @return str the JSON string

## ` fun decode(str json) > Json`
Decode string to a Json instance  @param str json The JSON string  @return Json

## ` bool? boolean`
When wrapped data is a boolean