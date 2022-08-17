# Buzz std lib
## Table of contents

- [debug](#debug)
- [gc](#gc)
- [std](#std)
- [math](#math)
- [buffer](#buffer)
- [os](#os)
- [fs](#fs)
- [io](#io)
- [json](#json)
## debug

### ` fun ast(str source, str scriptName) > str`
Parse `source` and return the abstract syntax tree in JSON
- **`script`:** name (used to fetch eventual extern functions)


**Returns:**  AST as JSON
## gc

### ` fun allocated() > num`
Returns the number of allocated bytes

**Returns:**  allocated bytes
### ` fun collect()`
Triggers a GC sweep
## std

### ` fun assert(bool condition, str message)`
If condition is false throw error with given message
- **`message`:** message printed if `condition` is false

### ` fun print(str value)`
Prints value on stdout
- **`value`:** value to print

### ` fun parseNumber(str string) > num?`
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  number parsed or null
### ` fun runFile(str filename)`
Run a buzz file
- **`filename`:** path to buzz file

## math

### ` fun abs(num n) > num`


**Returns:**  absolute value of n
### ` fun acos(num n) > num`


**Returns:**  acos of n
### ` fun asin(num n) > num`


**Returns:**  asin of n
### ` fun atan(num n) > num`


**Returns:**  atan of n
### ` fun bzceil(num n) > num`


**Returns:**  ceiled n
### ` fun bzcos(num n) > num`


**Returns:**  cos of n
### `pi num`
π constant
### ` fun deg(num n) > num`
Convert radian to degree
### ` fun bzexp(num n) > num`


**Returns:**  exp of n
### ` fun bzfloor(num n) > num`

### ` fun bzlog(num base, num n) > num`


**Returns:**  log(base) of n
### ` fun max(num a, num b) > num`


**Returns:**  max of a and b
### ` fun min(num a, num b) > num`


**Returns:**  min of a and b
### ` fun rad(num n) > num`
Convert degree to radian
### ` fun random() > num`


**Returns:**  random number between 0 and 1
### ` fun bzsin(num n) > num`


**Returns:**  sin of n
### ` fun bzsqrt(num n) > num`


**Returns:**  square root of n
### ` fun bztan(num n) > num`


**Returns:**  tan of n
## buffer

### ` object Buffer`
Read and write data to a string buffer

#### ` fun readBoolean() > bool?`
Reads a boolean

**Returns:**  Read boolean or `null` if nothing to read

#### ` str buffer`


#### ` fun read(num n) > str?`
Reads bytes
- **`n`:** How many bytes to read


**Returns:**  Read bytes or `null` if nothing to read

#### ` num cursor`


#### ` fun write(str bytes)`
Writes bytes
- **`bytes`:** Bytes to write


#### ` fun writeBoolean(bool value)`
Writes boolean
- **`boolean`:** Value to write


#### ` fun writeNumber(num number)`
Writes number
- **`number`:** Value to write


#### ` fun readNumber() > num?`
Reads a number

**Returns:**  Read number or `null` if nothing to read
## os

### ` fun time() > num`


**Returns:**  epoch time in ms
### ` fun env(str key) > str?`
Returns environment variable under `key`
- **`key`:** environment variable name

### ` fun tmpDir() > str`


**Returns:**  path to system temp directory
### ` fun tmpFilename(str? prefix) > str`

- **`prefix`:** prefix to the temp file name


**Returns:**  a temporary file name in system tmp dir
### ` fun buzzExit(num exitCode)`
Exit program with `exitCode`
- **`exitCode`:** exit code

### ` fun execute([str] command) > num`
Execute command and return its exit code
- **`command`:** command to execute


**Returns:**  exit code of the command
### ` enum SocketProtocol`
Protocols supported over a socket
### ` object Socket`
A socket

#### ` fun connect(str address, num port, SocketProtocol protocol) > Socket`
Opens a socket
- **`protocol`:** Protocol to use


**Returns:**  A new `Socket` opened and ready to use

#### ` fun receive(num n) > str?`
Receive at most `n` bytes from the socket
- **`n`:** How many bytes we're prepare to receive


**Returns:**  The bytes received or null if nothing to read

#### ` fun close()`
Close the socket

#### ` fun receiveLine() > str?`
Receive from socket until it's closed or a linefeed is received

**Returns:**  The bytes received or null if nothing to read


#### ` fun send(str bytes)`
Send bytes on the socket
- **`bytes`:** Bytes to send

### ` object TcpServer`
A TCP Server


#### ` fun init(str address, num port, bool reuseAddr) > TcpServer`
Starts a TCP server
- **`reuseAddr`:** Wether we want to accept multiple connections


**Returns:**  New `TcpServer` bound to `<address>:<port>`

#### ` fun accept() > Socket`
Accept a new connection

**Returns:**  Socket opened with the client

#### ` fun close()`
Close server

## fs

### ` fun currentDirectory() > str`
Returns current directory absolute path

**Returns:**  current directory
### ` fun makeDirectory(str path)`
Creates directory path
- **`path`:** directory to create

### ` fun delete(str path)`
Deletes directory or file at path
- **`path`:** direcotry/file to delete

### ` fun move(str source, str destination)`
Moves/renames file
- **`destination`:** where to move it

### ` fun list(str path) > [str]`
List files under path
- **`path`:** directory to list

## io

### ` enum FileMode`
File mode with which you can open a file
### ` object File`
Object to manipulate an opened file

#### ` fun close()`
Close file

#### ` fun read(num n) > str?`
Reads `n` bytes, returns null if nothing to read
- **`n`:** how many bytes to read


#### ` fun readAll() > str`
Reads file until `EOF`

#### ` fun readLine() > str?`
Reads next line, returns null if nothing to read

#### ` fun write(str bytes)`
Write bytes
- **`bytes`:** string to write


#### ` fun open(str filename, FileMode mode) > File`
Open file
- **`mode`:** Mode with which to open it


**Returns:**  opened file

#### ` num fd`
File descriptor
### `stdin File`
stdin
### `stdout File`
stdout
### `stderr File`
stderr
## json

### ` object Json`
Utility object to manage data from a JSON string

#### ` num? number`
When wrapped data is a number

#### ` str? string`
When wrapped data is a string

#### ` {str, Json}? map`
When wrapped data is an object, object property values are themselves wrapped in a `Json`

#### ` [Json]? list`
When wrapped data is a list, list elements are themselves warpped in a `Json`

#### ` fun encode() > str`
Encode to a JSON string

**Returns:**  str the JSON string

#### ` fun numberValue() > num`


**Returns:**  wrapped data number value or `0` if not a number

#### ` fun decode(str json) > Json`
Decode string to a Json instance
- **`str`:** json The JSON string


**Returns:**  Json

#### ` fun listValue() > [Json]`


**Returns:**  wrapped data list value or empty list if not a list

#### ` fun booleanValue() > bool`


**Returns:**  wrapped data boolean value or `false` if not a boolean

#### ` fun stringValue() > str`


**Returns:**  wrapped data string value or empty string if not a string

#### ` fun mapValue() > {str, Json}`


**Returns:**  wrapped data map value or empty map if not a map

#### ` fun q([str] path) > Json`
Query the json element at `path`, if nothing matches return `Json{}`
- **`path`:** Path to query


**Returns:**  Found `Json` or `Json{}` (which is `null`)

#### ` bool? boolean`
When wrapped data is a boolean