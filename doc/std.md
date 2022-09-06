# Buzz std lib
## Table of contents

- [debug](#debug)
- [gc](#gc)
- [std](#std)
- [http](#http)
- [math](#math)
- [buffer](#buffer)
- [os](#os)
- [errors](#errors)
- [fs](#fs)
- [io](#io)
- [json](#json)
## debug

### ` fun ast(str source, str scriptName) > str !> lib.errors.CompileError`
Parse `source` and return the abstract syntax tree in JSON
- **`script`:** name (used to fetch eventual extern functions)


**Returns:**  AST as JSON
## gc

### ` object lib.gc.CollectError`

### ` fun allocated() > num`
Returns the number of allocated bytes

**Returns:**  allocated bytes
### ` fun collect() > void !> lib.gc.CollectError`
Triggers a GC sweep
## std

### ` object lib.std.AssertError`

### ` fun assert(bool condition, str message) > void !> lib.std.AssertError`
If condition is false throw error with given message
- **`message`:** message printed if `condition` is false

### ` fun print(str value) > void`
Prints value on stdout
- **`value`:** value to print

### ` fun parseNumber(str string) > num?`
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  number parsed or null
## http

### ` object lib.http.Request`

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
### ` fun pow(num x, num y) > num !> lib.errors.OverflowError, lib.errors.UnderflowError`


**Returns:**  `x`^`y`
## buffer

### ` object lib.buffer.WriteWhileReadingError`

### ` object lib.buffer.Buffer`
Read and write data to a string buffer

#### ` fun len() > num`


**Returns:**  Length of the buffer

#### ` fun readBoolean() > bool?`
Reads a boolean

**Returns:**  Boolean we read or `null` if nothing to read


#### ` fun read(num n) > str?`
Reads `n` bytes

**Returns:**  Read bytes or `null` if nothing to read

#### ` fun init() > lib.buffer.Buffer`


**Returns:**  A new `Buffer`

#### ` fun write(str bytes) > void !> lib.buffer.WriteWhileReadingError`
Writes a string
- **`bytes`:** Bytes to write


#### ` fun writeNumber(num number) > void !> lib.buffer.WriteWhileReadingError`
Writes a number
- **`number`:** Number to write


#### ` fun deinit() > void`
Frees the buffer TODO: with finalizers we could do this automatically when the object is collected

#### ` fun cursor() > num`


**Returns:**  Position of the reading cursor

#### ` fun readNumber() > num?`
Reads a number

**Returns:**  Read number or `null` if nothing to read

#### ` fun getBuffer() > str`


#### ` fun empty() > void`
Empties the buffer

#### ` fun writeBoolean(bool boolean) > void !> lib.buffer.WriteWhileReadingError`
Writes a boolean
- **`boolean`:** Boolean to write

## os

### ` fun time() > num`


**Returns:**  epoch time in ms
### ` fun env(str key) > str? !> lib.errors.InvalidArgumentError`
Returns environment variable under `key`
- **`key`:** environment variable name

### ` fun tmpDir() > str`


**Returns:**  path to system temp directory
### ` fun tmpFilename(str? prefix) > str`

- **`prefix`:** prefix to the temp file name


**Returns:**  a temporary file name in system tmp dir
### ` fun buzzExit(num exitCode) > void`
Exit program with `exitCode`
- **`exitCode`:** exit code

### ` fun execute([str] command) > num !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError`
Execute command and return its exit code
- **`command`:** command to execute


**Returns:**  exit code of the command
### ` enum lib.os.SocketProtocol`
Protocols supported over a socket
### ` object lib.os.Socket`
A socket

#### ` fun receive(num n) > str? !> lib.errors.InvalidArgumentError, lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Receive at most `n` bytes from the socket
- **`n`:** How many bytes we're prepare to receive


**Returns:**  The bytes received or null if nothing to read

#### ` fun close() > void`
Close the socket

#### ` fun receiveAll() > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
Receive from socket until it's closed

**Returns:**  The bytes received or null if nothing to read

#### ` fun send(str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Send bytes on the socket
- **`bytes`:** Bytes to send


#### ` fun connect(str address, num port, lib.os.SocketProtocol protocol) > lib.os.Socket !> lib.errors.InvalidArgumentError, lib.errors.SocketError, lib.errors.NotYetImplementedError`
Opens a socket
- **`protocol`:** Protocol to use


**Returns:**  A new `Socket` opened and ready to use

#### ` fun receiveLine() > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
Receive from socket until it's closed or a linefeed is received

**Returns:**  The bytes received or null if nothing to read

### ` object lib.os.TcpServer`
A TCP Server


#### ` fun init(str address, num port, bool reuseAddr) > lib.os.TcpServer !> lib.errors.SocketError, lib.errors.UnexpectedError, lib.errors.InvalidArgumentError, lib.errors.FileSystemError`
Starts a TCP server
- **`reuseAddr`:** Wether we want to accept multiple connections


**Returns:**  New `TcpServer` bound to `<address>:<port>`

#### ` fun accept() > lib.os.Socket !> lib.errors.SocketError, lib.errors.UnexpectedError`
Accept a new connection

**Returns:**  Socket opened with the client

#### ` fun close() > void`
Close server

## errors

### ` enum lib.errors.FileSystemError`

### ` enum lib.errors.ExecError`

### ` enum lib.errors.SocketError`

### ` object lib.errors.FileNotFoundError`

### ` enum lib.errors.ReadWriteError`

### ` object lib.errors.OutOfMemoryError`

### ` object lib.errors.CompileError`

### ` object lib.errors.InterpretError`

### ` object lib.errors.UnexpectedError`

### ` object lib.errors.InvalidArgumentError`

### ` object lib.errors.OverflowError`

### ` object lib.errors.UnderflowError`

### ` object lib.errors.NotYetImplementedError`

## fs

### ` fun currentDirectory() > str !> lib.errors.FileSystemError, lib.errors.InvalidArgumentError`
Returns current directory absolute path

**Returns:**  current directory
### ` fun makeDirectory(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Creates directory path
- **`path`:** directory to create

### ` fun delete(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Deletes directory or file at path
- **`path`:** direcotry/file to delete

### ` fun move(str source, str destination) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Moves/renames file
- **`destination`:** where to move it

### ` fun list(str path) > [str] !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError`
List files under path
- **`path`:** directory to list

## io

### ` enum lib.io.FileMode`
File mode with which you can open a file
### ` object lib.io.File`
Object to manipulate an opened file

#### ` fun close() > void`
Close file

#### ` fun read(num n) > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.InvalidArgumentError, lib.errors.UnexpectedError`
Reads `n` bytes, returns null if nothing to read
- **`n`:** how many bytes to read


#### ` fun readAll() > str !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Reads file until `EOF`

#### ` fun readLine() > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Reads next line, returns null if nothing to read

#### ` fun write(str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Write bytes
- **`bytes`:** string to write


#### ` fun open(str filename, lib.io.FileMode mode) > lib.io.File !> lib.errors.FileNotFoundError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Open file
- **`mode`:** Mode with which to open it


**Returns:**  opened file

#### ` num fd`
File descriptor
### `stdin lib.io.File`
stdin
### `stdout lib.io.File`
stdout
### `stderr lib.io.File`
stderr
### ` fun runFile(str filename) > void !> lib.errors.CompileError, lib.errors.InterpretError, lib.errors.FileSystemError, lib.errors.ReadWriteError`
Run a buzz file
- **`filename`:** path to buzz file

## json

### ` object lib.json.JsonParseError`

### ` object lib.json.Json`
Utility object to manage data from a JSON string

#### ` num? number`
When wrapped data is a number

#### ` str? string`
When wrapped data is a string

#### ` {str, lib.json.Json}? map`
When wrapped data is an object, object property values are themselves wrapped in a `Json`

#### ` [lib.json.Json]? list`
When wrapped data is a list, list elements are themselves warpped in a `Json`

#### ` fun encode() > str`
Encode to a JSON string

**Returns:**  str the JSON string

#### ` fun numberValue() > num`


**Returns:**  wrapped data number value or `0` if not a number

#### ` fun decode(str json) > lib.json.Json !> lib.json.JsonParseError, str`
Decode string to a Json instance
- **`str`:** json The JSON string


**Returns:**  Json

#### ` fun listValue() > [lib.json.Json]`


**Returns:**  wrapped data list value or empty list if not a list

#### ` fun booleanValue() > bool`


**Returns:**  wrapped data boolean value or `false` if not a boolean

#### ` fun stringValue() > str`


**Returns:**  wrapped data string value or empty string if not a string

#### ` fun mapValue() > {str, lib.json.Json}`


**Returns:**  wrapped data map value or empty map if not a map

#### ` fun q([str] path) > lib.json.Json !> lib.json.JsonParseError, str`
Query the json element at `path`, if nothing matches return `Json{}`
- **`path`:** Path to query


**Returns:**  Found `Json` or `Json{}` (which is `null`)

#### ` bool? boolean`
When wrapped data is a boolean