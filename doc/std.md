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

### `extern fun dump(<T>, generic type #66-0 value) > void `
Dump any value to stdout
### `extern fun ast(str source, str scriptName) > str !> lib.errors.CompileError `
Parse `source` and return the abstract syntax tree in JSON
- **`script`:** name (used to fetch eventual extern functions)


**Returns:**  AST as JSON
## gc

### `object lib.gc.CollectError `
Error occured while collecting
### `extern fun allocated() > int `
Returns the number of allocated bytes

**Returns:**  allocated bytes
### `extern fun collect() > void !> lib.gc.CollectError `
Triggers a GC sweep
## std

### `fun assert(bool condition, str message) > void `
If condition is false print message and exit program
- **`message`:** message printed if `condition` is false

### `extern fun print(str value) > void `
Prints value on stdout
- **`value`:** value to print

### `extern fun parseInt(str string) > int? `
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  integer parsed or null
### `extern fun parseFloat(str string) > float? `
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  float parsed or null
### `extern fun toInt(float n) > int `
Cast integer to a float value
- **`n`:** value to cast


**Returns:**  casted value
### `extern fun toFloat(int n) > float `
Cast float to a integer value
- **`n`:** value to cast


**Returns:**  casted value
### `extern fun char(int byte) > str `
Return ascii char for given byte
## http

## math

### `extern fun abs(float n) > float `


**Returns:**  absolute value of n
### `extern fun acos(float n) > float `


**Returns:**  acos of n
### `extern fun asin(float n) > float `


**Returns:**  asin of n
### `extern fun atan(float n) > float `


**Returns:**  atan of n
### `extern fun bzceil(float n) > int `


**Returns:**  ceiled n
### `extern fun bzcos(float n) > float `


**Returns:**  cos of n
### `float pi`
π constant
### `fun deg(float n) > float `
Convert radian to degree
### `extern fun bzexp(float n) > float `


**Returns:**  exp of n
### `extern fun bzfloor(float n) > int `

### `extern fun bzlog(float base, float n) > float `


**Returns:**  log(base) of n
### `extern fun max(float a, float b) > float `


**Returns:**  max of a and b
### `extern fun min(float a, float b) > float `


**Returns:**  min of a and b
### `fun rad(float n) > float `
Convert degree to radian
### `extern fun random() > float `


**Returns:**  random number between 0 and 1
### `extern fun bzsin(float n) > float `


**Returns:**  sin of n
### `extern fun bzsqrt(float n) > float `


**Returns:**  square root of n
### `extern fun bztan(float n) > float `


**Returns:**  tan of n
### `extern fun pow(float x, float y) > float !> lib.errors.OverflowError, lib.errors.UnderflowError `


**Returns:**  `x`^`y`
## buffer

### `object lib.buffer.Buffer `
Read and write data to a string buffer
## os

### `extern fun time() > int `


**Returns:**  epoch time in ms
### `extern fun env(str key) > str? !> lib.errors.InvalidArgumentError `
Returns environment variable under `key`
- **`key`:** environment variable name

### `extern fun tmpDir() > str `


**Returns:**  path to system temp directory
### `extern fun tmpFilename(str? prefix) > str `

- **`prefix`:** prefix to the temp file name


**Returns:**  a temporary file name in system tmp dir
### `extern fun buzzExit(int exitCode) > void `
Exit program with `exitCode`
- **`exitCode`:** exit code

### `extern fun execute([str] command) > int !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError `
Execute command and return its exit code
- **`command`:** command to execute


**Returns:**  exit code of the command
### `enum lib.os.SocketProtocol `
Protocols supported over a socket
### `object lib.os.Socket `
A socket
### `object lib.os.TcpServer `
A TCP Server
## errors

## fs

### `fun currentDirectory() > str !> lib.errors.FileSystemError, lib.errors.InvalidArgumentError `
Returns current directory absolute path

**Returns:**  current directory
### `extern fun makeDirectory(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError `
Creates directory path
- **`path`:** directory to create

### `extern fun delete(str path) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError `
Deletes directory or file at path
- **`path`:** direcotry/file to delete

### `extern fun move(str source, str destination) > void !> lib.errors.FileSystemError, lib.errors.FileNotFoundError, lib.errors.UnexpectedError `
Moves/renames file
- **`destination`:** where to move it

### `extern fun list(str path) > [str] !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError `
List files under path
- **`path`:** directory to list

## io

### `enum lib.io.FileMode `
File mode with which you can open a file
### `object lib.io.File `
Object to manipulate an opened file
### `lib.io.File stdin`
stdin
### `lib.io.File stdout`
stdout
### `lib.io.File stderr`
stderr
### `extern fun runFile(str filename) > void !> lib.errors.CompileError, lib.errors.InterpretError, lib.errors.FileSystemError, lib.errors.ReadWriteError `
Run a buzz file
- **`filename`:** path to buzz file

## json

### `object lib.json.Json `
Utility object to manage data from a JSON string