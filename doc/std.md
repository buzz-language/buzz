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

### `extern fun dump(<T>, generic type #60-0 value) > void `
Dump any value to stdout
### `extern fun ast(str source, str scriptName) > str !> lib.errors.CompileError `
Parse `source` and return the abstract syntax tree in JSON
- **`script`:** name (used to fetch eventual extern functions)


**Returns:**  AST as JSON
## gc

### `object lib.gc.CollectError `
Error occured while collecting
### `extern fun allocated() > num `
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

### `extern fun parseNumber(str string) > num? `
Parse number, returns false if string does not represent a number
- **`string`:** string to parse


**Returns:**  number parsed or null
### `extern fun char(num byte) > str `
Return ascii char for given byte
## http

## math

### `extern fun abs(num n) > num `


**Returns:**  absolute value of n
### `extern fun acos(num n) > num `


**Returns:**  acos of n
### `extern fun asin(num n) > num `


**Returns:**  asin of n
### `extern fun atan(num n) > num `


**Returns:**  atan of n
### `extern fun bzceil(num n) > num `


**Returns:**  ceiled n
### `extern fun bzcos(num n) > num `


**Returns:**  cos of n
### `num pi`
π constant
### `fun deg(num n) > num `
Convert radian to degree
### `extern fun bzexp(num n) > num `


**Returns:**  exp of n
### `extern fun bzfloor(num n) > num `

### `extern fun bzlog(num base, num n) > num `


**Returns:**  log(base) of n
### `extern fun max(num a, num b) > num `


**Returns:**  max of a and b
### `extern fun min(num a, num b) > num `


**Returns:**  min of a and b
### `fun rad(num n) > num `
Convert degree to radian
### `extern fun random() > num `


**Returns:**  random number between 0 and 1
### `extern fun bzsin(num n) > num `


**Returns:**  sin of n
### `extern fun bzsqrt(num n) > num `


**Returns:**  square root of n
### `extern fun bztan(num n) > num `


**Returns:**  tan of n
### `extern fun pow(num x, num y) > num !> lib.errors.OverflowError, lib.errors.UnderflowError `


**Returns:**  `x`^`y`
## buffer

### `object lib.buffer.Buffer `
Read and write data to a string buffer
## os

### `extern fun time() > num `


**Returns:**  epoch time in ms
### `extern fun env(str key) > str? !> lib.errors.InvalidArgumentError `
Returns environment variable under `key`
- **`key`:** environment variable name

### `extern fun tmpDir() > str `


**Returns:**  path to system temp directory
### `extern fun tmpFilename(str? prefix) > str `

- **`prefix`:** prefix to the temp file name


**Returns:**  a temporary file name in system tmp dir
### `extern fun buzzExit(num exitCode) > void `
Exit program with `exitCode`
- **`exitCode`:** exit code

### `extern fun execute([str] command) > num !> lib.errors.FileSystemError, lib.errors.UnexpectedError, lib.errors.FileNotFoundError `
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