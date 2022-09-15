# Buzz std lib
## Table of contents

- [debug](#debug)
- [gc](#gc)
- [std](#std)
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


**Returns:**  AST as JSONvoid
## gc

### ` object lib.gc.CollectError`
void
### ` fun allocated() > num`
Returns the number of allocated bytes

**Returns:**  allocated bytesvoid
### ` fun collect() > void !> lib.gc.CollectError`
Triggers a GC sweepvoid
## std

### ` object lib.std.AssertError`
void
### ` fun assert(bool condition, str message) > void !> lib.std.AssertError`
If condition is false throw error with given message
- **`message`:** message printed if `condition` is false
void
### ` fun print(str value) > void`
Prints value on stdout
- **`value`:** value to print
void
### ` fun parseNumber(str string) > num?`
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  number parsed or nullvoid
## math

### ` fun abs(num n) > num`


**Returns:**  absolute value of nvoid
### ` fun acos(num n) > num`


**Returns:**  acos of nvoid
### ` fun asin(num n) > num`


**Returns:**  asin of nvoid
### ` fun atan(num n) > num`


**Returns:**  atan of nvoid
### ` fun bzceil(num n) > num`


**Returns:**  ceiled nvoid
### ` fun bzcos(num n) > num`


**Returns:**  cos of nvoid
### `pi num`
π constantvoid
### ` fun deg(num n) > num`
Convert radian to degreevoid
### ` fun bzexp(num n) > num`


**Returns:**  exp of nvoid
### ` fun bzfloor(num n) > num`
void
### ` fun bzlog(num base, num n) > num`


**Returns:**  log(base) of nvoid
### ` fun max(num a, num b) > num`


**Returns:**  max of a and bvoid
### ` fun min(num a, num b) > num`


**Returns:**  min of a and bvoid
### ` fun rad(num n) > num`
Convert degree to radianvoid
### ` fun random() > num`


**Returns:**  random number between 0 and 1void
### ` fun bzsin(num n) > num`


**Returns:**  sin of nvoid
### ` fun bzsqrt(num n) > num`


**Returns:**  square root of nvoid
### ` fun bztan(num n) > num`


**Returns:**  tan of nvoid
### ` fun pow(num x, num y) > num !> lib.errors.OverflowError, lib.errors.UnderflowError`


**Returns:**  `x`^`y`void
## buffer

### ` object lib.buffer.WriteWhileReadingError`
void
### ` fun BufferNew() > ud`
void
### ` fun BufferDeinit(ud userdata) > void`
void
### ` fun BufferRead(ud userdata, num n) > str?`
void
### ` fun BufferWrite(ud userdata, str bytes) > void !> lib.buffer.WriteWhileReadingError`
void
### ` fun BufferReadBoolean(ud userdata) > bool?`
void
### ` fun BufferWriteBoolean(ud userdata, bool b) > void !> lib.buffer.WriteWhileReadingError`
void
### ` fun BufferWriteNumber(ud userdata, num n) > str !> lib.buffer.WriteWhileReadingError`
void
### ` fun BufferReadNumber(ud userdata) > num?`
void
### ` fun BufferLen(ud userdata) > num`
void
### ` fun BufferCursor(ud userdata) > num`
void
### ` fun BufferBuffer(ud userdata) > str`
void
### ` fun BufferEmpty(ud userdata) > void`
void
### ` object lib.buffer.Buffer`
Read and write data to a string buffer
#### ` fun len() > num`


**Returns:**  Length of the buffer
void
#### ` fun readBoolean() > bool?`
Reads a boolean

**Returns:**  Boolean we read or `null` if nothing to read
void
#### ` fun toString() > str`

void
#### ` ud buffer`

void
#### ` fun read(num n) > str?`
Reads `n` bytes

**Returns:**  Read bytes or `null` if nothing to read
void
#### ` fun init() > lib.buffer.Buffer`


**Returns:**  A new `Buffer`
void
#### ` fun write(str bytes) > void !> lib.buffer.WriteWhileReadingError`
Writes a string
- **`bytes`:** Bytes to write

void
#### ` fun writeNumber(num number) > void !> lib.buffer.WriteWhileReadingError`
Writes a number
- **`number`:** Number to write

void
#### ` fun deinit() > void`
Frees the buffer TODO: with finalizers we could do this automatically when the object is collected
void
#### ` fun cursor() > num`


**Returns:**  Position of the reading cursor
void
#### ` fun readNumber() > num?`
Reads a number

**Returns:**  Read number or `null` if nothing to read
void
#### ` fun empty() > void`
Empties the buffer
void
#### ` fun writeBoolean(bool boolean) > void !> lib.buffer.WriteWhileReadingError`
Writes a boolean
- **`boolean`:** Boolean to write

voidvoid
## os

### ` fun time() > num`


**Returns:**  epoch time in msvoid
### ` fun env(str key) > str? !> lib.errors.InvalidArgumentError`
Returns environment variable under `key`
- **`key`:** environment variable name
void
### ` fun tmpDir() > str`


**Returns:**  path to system temp directoryvoid
### ` fun tmpFilename(str? prefix) > str`

- **`prefix`:** prefix to the temp file name


**Returns:**  a temporary file name in system tmp dirvoid
### ` fun buzzExit(num exitCode) > void`
Exit program with `exitCode`
- **`exitCode`:** exit code
void
### ` fun execute([str] command) > num !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError`
Execute command and return its exit code
- **`command`:** command to execute


**Returns:**  exit code of the commandvoid
### ` fun SocketConnect(str address, num port, num protocol) > num !> lib.errors.InvalidArgumentError, lib.errors.SocketError, lib.errors.NotYetImplementedError`
void
### ` fun SocketClose(num fd) > void`
void
### ` fun SocketRead(num fd, num n) > str? !> lib.errors.InvalidArgumentError, lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
void
### ` fun SocketWrite(num fd, str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
void
### ` fun SocketServerStart(str address, num port, bool reuseAddr) > num !> lib.errors.InvalidArgumentError, lib.errors.SocketError, lib.errors.UnexpectedError, lib.errors.FileSystemError`
void
### ` fun SocketServerAccept(num fd, bool reuseAddr) > num !> lib.errors.SocketError, lib.errors.UnexpectedError`
void
### ` fun SocketReadLine(num fd) > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
void
### ` fun SocketReadAll(num fd) > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
void
### ` enum lib.os.SocketProtocol`
Protocols supported over a socketvoid
### ` object lib.os.Socket`
A socket
#### ` fun receive(num n) > str? !> lib.errors.InvalidArgumentError, lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Receive at most `n` bytes from the socket
- **`n`:** How many bytes we're prepare to receive


**Returns:**  The bytes received or null if nothing to read
void
#### ` fun close() > void`
Close the socket
void
#### ` fun receiveAll() > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
Receive from socket until it's closed

**Returns:**  The bytes received or null if nothing to read
void
#### ` fun send(str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Send bytes on the socket
- **`bytes`:** Bytes to send

void
#### ` fun connect(str address, num port, lib.os.SocketProtocol protocol) > lib.os.Socket !> lib.errors.InvalidArgumentError, lib.errors.SocketError, lib.errors.NotYetImplementedError`
Opens a socket
- **`protocol`:** Protocol to use


**Returns:**  A new `Socket` opened and ready to use
void
#### ` fun receiveLine() > str? !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.ReadWriteError`
Receive from socket until it's closed or a linefeed is received

**Returns:**  The bytes received or null if nothing to read
void
#### ` num fd`

voidvoid
### ` object lib.os.TcpServer`
A TCP Server
#### ` lib.os.Socket serverSocket`

void
#### ` fun init(str address, num port, bool reuseAddr) > lib.os.TcpServer !> lib.errors.SocketError, lib.errors.UnexpectedError, lib.errors.InvalidArgumentError, lib.errors.FileSystemError`
Starts a TCP server
- **`reuseAddr`:** Wether we want to accept multiple connections


**Returns:**  New `TcpServer` bound to `<address>:<port>`
void
#### ` fun accept() > lib.os.Socket !> lib.errors.SocketError, lib.errors.UnexpectedError`
Accept a new connection

**Returns:**  Socket opened with the client
void
#### ` fun close() > void`
Close server
void
#### ` bool reuseAddr`

voidvoid
## errors

### ` enum lib.errors.FileSystemError`
void
### ` enum lib.errors.ExecError`
void
### ` enum lib.errors.SocketError`
void
### ` object lib.errors.FileNotFoundError`
void
### ` enum lib.errors.ReadWriteError`
void
### ` object lib.errors.OutOfMemoryError`
void
### ` object lib.errors.CompileError`
void
### ` object lib.errors.InterpretError`
void
### ` object lib.errors.UnexpectedError`
void
### ` object lib.errors.InvalidArgumentError`
void
### ` object lib.errors.OverflowError`
void
### ` object lib.errors.UnderflowError`
void
### ` object lib.errors.NotYetImplementedError`
void
## fs

### ` fun currentDirectory() > str !> lib.errors.FileSystemError, lib.errors.InvalidArgumentError`
Returns current directory absolute path

**Returns:**  current directoryvoid
### ` fun makeDirectory(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Creates directory path
- **`path`:** directory to create
void
### ` fun delete(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Deletes directory or file at path
- **`path`:** direcotry/file to delete
void
### ` fun move(str source, str destination) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError`
Moves/renames file
- **`destination`:** where to move it
void
### ` fun list(str path) > [str] !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError`
List files under path
- **`path`:** directory to list
void
## io

### ` fun FileOpen(str filename, num mode) > num !> lib.errors.FileNotFoundError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
void
### ` fun FileClose(num fd) > void`
void
### ` fun FileReadAll(num fd) > str !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
void
### ` fun FileReadLine(num fd) > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
void
### ` fun FileRead(num fd, num n) > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.InvalidArgumentError, lib.errors.UnexpectedError`
void
### ` fun FileWrite(num fd, str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
void
### ` fun getStdIn() > num`
void
### ` fun getStdOut() > num`
void
### ` fun getStdErr() > num`
void
### ` enum lib.io.FileMode`
File mode with which you can open a filevoid
### ` object lib.io.File`
Object to manipulate an opened file
#### ` fun close() > void`
Close file
void
#### ` fun read(num n) > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.InvalidArgumentError, lib.errors.UnexpectedError`
Reads `n` bytes, returns null if nothing to read
- **`n`:** how many bytes to read

void
#### ` fun readAll() > str !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Reads file until `EOF`
void
#### ` fun readLine() > str? !> lib.errors.ReadWriteError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Reads next line, returns null if nothing to read
void
#### ` fun write(str bytes) > void !> lib.errors.FileSystemError, lib.errors.ReadWriteError, lib.errors.UnexpectedError`
Write bytes
- **`bytes`:** string to write

void
#### ` fun open(str filename, lib.io.FileMode mode) > lib.io.File !> lib.errors.FileNotFoundError, lib.errors.FileSystemError, lib.errors.UnexpectedError`
Open file
- **`mode`:** Mode with which to open it


**Returns:**  opened file
void
#### ` num fd`
File descriptor
voidvoid
### `stdin lib.io.File`
stdinvoid
### `stdout lib.io.File`
stdoutvoid
### `stderr lib.io.File`
stderrvoid
### ` fun runFile(str filename) > void !> lib.errors.CompileError, lib.errors.InterpretError, lib.errors.FileSystemError, lib.errors.ReadWriteError`
Run a buzz file
- **`filename`:** path to buzz file
void
## json

### ` object lib.json.JsonParseError`
void
### ` object lib.json.JsonParser`
void
### ` object lib.json.Json`
Utility object to manage data from a JSON string
#### ` num? number`
When wrapped data is a number
void
#### ` str? string`
When wrapped data is a string
void
#### ` {str, lib.json.Json}? map`
When wrapped data is an object, object property values are themselves wrapped in a `Json`
void
#### ` [lib.json.Json]? list`
When wrapped data is a list, list elements are themselves warpped in a `Json`
void
#### ` fun encode() > str`
Encode to a JSON string

**Returns:**  str the JSON string
void
#### ` fun numberValue() > num`


**Returns:**  wrapped data number value or `0` if not a number
void
#### ` fun decode(str json) > lib.json.Json !> lib.json.JsonParseError, lib.buffer.WriteWhileReadingError`
Decode string to a Json instance
- **`str`:** json The JSON string


**Returns:**  Json
void
#### ` fun listValue() > [lib.json.Json]`


**Returns:**  wrapped data list value or empty list if not a list
void
#### ` fun booleanValue() > bool`


**Returns:**  wrapped data boolean value or `false` if not a boolean
void
#### ` fun stringValue() > str`


**Returns:**  wrapped data string value or empty string if not a string
void
#### ` fun mapValue() > {str, lib.json.Json}`


**Returns:**  wrapped data map value or empty map if not a map
void
#### ` fun q([str] path) > lib.json.Json !> lib.json.JsonParseError`
Query the json element at `path`, if nothing matches return `Json{}`
- **`path`:** Path to query


**Returns:**  Found `Json` or `Json{}` (which is `null`)
void
#### ` bool? boolean`
When wrapped data is a boolean
voidvoid