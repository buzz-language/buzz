<p align="center">
    <img src="https://github.com/buzz-language/buzz/raw/main/logo.png" alt="buzz" width="204" height="204">
</p>

# 👨‍🚀 buzz

A small/lightweight typed scripting language written in Zig

<p align="center">
    <img src="https://github.com/buzz-language/buzz/raw/main/example.png" alt="buzz code example">
</p>

## Goals

- Small in size and complexity (just a bit more than Lua though)
- Strict typing
- Unambiguous
- No nonsense coercion
- [Fibers](#fibers)
- Tooling
    - [Generate doc from docblocks (in progress)](https://github.com/buzz-language/buzz/blob/main/doc/index.md)
    - LSP (in progress)
    - Debugger and DAP
    - [TextMate syntax](https://github.com/buzz-language/code)

## Progress

We're not far from completing [milestone 0.1.0](https://github.com/buzz-language/buzz/milestone/1). As writing buzz code is the best way of finding bugs, we implement a lot of features of the [next](https://github.com/buzz-language/buzz/milestone/2) [milestones](https://github.com/buzz-language/buzz/milestone/3) too.

## How to build

### Requirements
- Since this is built with Zig, you should be able to build buzz on a wide variety of architectures even though this has only be tested on x86.
- Linux or macOS (not much work is needed to make it work on [Windows](https://github.com/buzz-language/buzz/issues/74))
- libpcre (not libpcre2)
- libc on Linux
- zig master

### Steps
1. Clone project: `git clone https://github.com/buzz-language/buzz <buzz_dir>`
2. Checkout submodules: `git submodule update --init`
3. Add to your shell rc:
```bash
export BUZZ_PATH="/path/to/buzz/dist"
export PATH="$BUZZ_PATH:$PATH"
```
4. Build it: `zig build -Drelease-safe`
5. Have fun: `buzz <myscript.buzz>`

Additionnally, install the [VS Code extension](https://github.com/buzz-language/code) to get syntax highlighting. If you don't use VS Code but your editor supports [TextMate grammar files](https://github.com/buzz-language/code/blob/main/syntaxes/buzz.tmLanguage.json), you can use that.

## Quick tour

**Note:** You can also take a look at `tests/` for more examples.

- [Types and variables](#types-and-variables)
- [Operators](#operators)
- [Functions](#functions)
- [Enums](#enums)
- [Control flow](#control-flow)
- [Objects](#objects)
- [Protocols](#protocols)
- [Errors](#errors)
- [Import/Export](#import/export)
- [Fibers](#fibers)
- [Call C/Zig code](#call-c/zig-code)

### Types and variables

```buzz
| Basic types
bool aBoolean = true;
| Immutable sequence of bytes
str aString = "hello world";
str multiline = `
    i'm on several
    lines
    yes
`;
int aNumber = 23; | i64
aNumber = 0b110;
aNumber = 0xA12F;
float aFloat = 23.123; | f64
| A PCRE regex
pat aPattern = _hello [a-z]+_;
| Userdata are pointers to foreign data wrapped inside a buzz obj
ud userdata = GetSomeForeignData();

| A constant
const float pi = 3.14;

| Data structures
[int] aListOfNumbers = [1, 2, 3];
| Keys and values can be of any type
{str, int} aMap = {
    "one": 1,
    "two": 2,
    "three": 3,
};
```

### Operators

```buzz
| Comparison
12 == 12;
12 != 13;
12 >= 12;
12 <= 12;
12 > 11;
12 < 13;

| Arithmetic
12 + 12 == 24;
12 - 12 == 0;
12 * 12 == 144;
12 / 12 == 1;
12 % 12 == 0;

| Logical
12 > 3 and 5 < 12;
12 > 3 or 12 < 5;

| String
"hello " + "world" == "hello world";
"hello" == "hello";

| Bitwise
15 << 3 == 120; | shift left
15 >> 3 == 1;   | shift right
12 & 23 == 4    | and
15 ^ 3 == 12;   | xor
15 \ 3 == 15;   | or
~15 == -16;     | not
```

#### Optionals

```buzz
str? aStringOrNull = "hello";

| Null coalescing operator is `??`
str unwrapped = aStringOrNull ?? "default value"

| Force unwrapping with `!`
str unwrapped = aStringOrNull!;

| Graceful unwrapping
[int]? optList = null;

print(optList?.len()); | -> null

| If unwrap
if (aStringOrNull -> aString) {
    print("{aString} is not null");
} else {
    print("aString was null");
}
```

### Functions

```buzz
fun sayHiTo(str name, str? lastName, int age) > str {
    | Interpolation with `{}`
    return "Hi {name} {lastName ?? ""}!"
}

| Same could be an arrow function
fun sayHiTo(str name, str? lastName, int age) > str -> "Hi {name} {lastName ?? ""}!"
```

When called, only the first argument name of a function can be omitted, order is not required:

```buzz
sayHiTo("Joe", age: 35, lastName: "Doe"); | -> "Hi Joe Doe!"
```

Functions are first-class citizens:

```buzz
Function() fn = fun () > void -> print("hello world"); | Arrow function

fn(); | -> "hello world"
```

#### Generics

```buzz
fun countMap(<K, V>, {K, V} map) > int {
    return map.size();
}

{str, int} map = {
    "one": 1,
    "two": 2,
    "three": 3,
};

countMap(<str, int>, map) == 3;
```

### Enums

```buzz
| Enums can have number or string values
enum Natural {
    zero,
    one,
    two,
}

Natural.zero.value == 0;

| Values must either be implicit or be all defined
enum(int) NumEnum {
    three = 3,
    four = 4,
    five = 5,
}

enum(str) Country {
    usa = "United States of America",
    uk = "United Kingdoms",
    fr = "France",
}

| To get the value associated with a enum case
print(Country.usa.value); | -> "United States of America"

enum(str) Locale {
    fr,
    en,
    it,
}

Locale.fr.value == "fr";

| Create enum instance from its value
Locale? locale = Locale("fr");
locale == Locale.fr
```

### Control flow

```buzz
| The usual
if (someCondition) {
    | ...
} else if (anotherCondition) {
    | ...
} else {
    | ...
}

int i = 0;
while (i < 10) {
    i = i + 1;
}

int j = 10;
do {
    j = j - 1;
} until (j == 0)

for (int i = 0; i < 10; i = i + 1) {
    | ...
    break;
}

| inline if
int value = if (something > 0) 12 else 13;
```

#### `foreach`

`foreach` can iterate over most data structures:

```buzz
foreach (SomeEnum case in SomeEnum) {
    | ...
}

foreach (int i, str value in listOfStrings) {
    | ...
}

foreach (str key, int value in aMap) {
    | ...
}

foreach (int i, str char in aString) {
    | ...
}

fib<void, int?> fibonnaciFib = &fibonnaci(10);
foreach (int value in fibonnaciFib) {
    | ...
}
```

### Objects

An `object` is like a class except it can't be inherited from and can't inherit from anything:

```buzz
object Person {
    static population = 0;

    str name = "Joe", | Fields can have default values
    int age = 35,

    | Method
    fun sayHello() > void {
        print("Hello {this.name}");
    }

    | Object don't have constructor but you can implement one with a static method
    static init(str name, int age) > Person {
        Person.population = Person.population + 1;

        return Person {
            name = name,
            age = age,
        };
    }
}
```

#### Anonymous objects

```buzz
| Anonymous objects don't have methods, static fields or default values
fun getInfo() > obj{ str name, int age } {
    return .{
        name = "Joe",
        age = 36,
    };
}

| ...

obj{ str name, int age } info = getInfo();
```

### Protocols

A `protocol` defines a set of methods. Objects can conform to any number of them:
```buzz
protocol Translatable {
    fun translate(dx: int, dy: int) > void;
}

protocol Printable {
    fun print() > void;
}

object(Translatable, Printable) Point {
    int x,
    int y,

    fun translate(dx: int, dy: int) > void {
        this.x = this.x + dx;
        this.y = this.y + dy;
    }

    fun print() > void {
        print("Point ({this.x}, {this.y})");
    }
}

object(Printable) Line {
    Point start,
    Point end,

    fun print() > void {
        print("Line ({this.start.x}, {this.start.y}) ({this.end.x}, {this.end.y})");
    }
}

| ...

[Printable] elements = [
    Point{ x = 0, y = 0 },
    Line{
        start = Point{ x = 10, y = 10 },
        end = Point{ x = 15, y = 12 },
    },
];

foreach (int i, Printable element in elements) {
    element.print();
}
```

### Errors

Functions must specify which error they can raise with `!> type1, type2, ...`. Error can't be raised in the global scope. `test` functions ignore errors.

```buzz
enum(str) MyErrors {
    failed = "Something failed",
    bad = "Something bad",
    ohno = "Oh no!",
}

enum(str) OtherErrors {
    failed = "Something failed",
    bad = "Something bad",
    ohno = "Oh no!",
}

fun willFail() > int !> MyErrors, OtherErrors, str {
    int random = rand();
    if (random == 1) {
        throw MyErrors.failed;
    } else if (random == 0) {
        throw OtherErrors.failed;
    }

    throw "something else";

    return 0;
}

| Use default value in case of any error
int result = willFail() catch 0;

| Try catch works as you would expect
try {
    willFail();
} catch (str error) {
    print("Caught error {error}");
} catch {
    print("Catches everything");
}

| Throwing an object instance with a `message` field will print the message
throw .{ message = "Something's wrong" } | -> Error: Something's wrong
throw SomeObject{ number = 12 }          | -> Error: object instance 0x1feb12 `SomeObject`
```

### Import/Export

```buzz
| hello.buzz

| Import std lib
import "lib/std";

fun sayHello() > void {
    print("Hello world!");
}

| Make it visible when imported
export sayHello;
```

```buzz
| main.buzz
import "hello";

fun main([str] args) > void {
    sayHello();
}
```

### Fibers

Similar to Lua's coroutines. Buzz's fibers have their own state and stack and can be switched in and out from.
Fibers can yield from within any call depth. Any function can be wrapped in a fiber. Unlike Lua, `yield` are evaluated and dismissed
if a function is not called within a fiber and do not raise an error.
`resolve` allows to run a fiber until completion without stopping for any `yield`. It can be called after the fiber is over in order to
get the wrapped function return value.

```buzz
| Returns a string, yields numbers
| Always yields an optional type because null is returned if you resume a terminated fiber
fun count(int n) > str > int? {
    for (int i = 0; i < n; i = i + 1) {
        | If the function is not called in a fiber, yields are evaluated and dismissed
        | Otherwise the value is returned as `resume` result
        yield i;
    }

    return "Counting is done!";
}

fun main([str] args) > void {
    | Wraps the call to `count` in a fiber, however the fiber is not started until a `resolve` or `resume` instruction
    fib<str, int?> counter = &count(10);

    int sum = 0;
    while (!counter.over()) {
        | resume returns null if nothing was yielded and/or fiber is over
        sum = sum + resume counter ?? 0;
    }

    assert(resolve counter == "Counting is done!", message: "resolve returns fiber return value");
}
```

### Call C/Zig code

First define the buzz interface. The `extern` keyword means that buzz we'll look for a dynamic library named `libmylib.dylib` (only dylib right now):

```buzz
| mylib.buzz
extern fun assert(bool condition, str message) > void
```

Then implement it in Zig or C using the [buzz_api](https://github.com/buzz-language/buzz/blob/main/lib/buzz_api.zig):

```zig
// buzz_mylib.zig
const std = @import("std");
const api = @import("buzz_api.zig");

// We have to respect C ABI
export fn assert(vm: *api.VM) c_int {
    var condition: bool = vm.bz_peek(1).bz_valueToBool();

    if (!condition) {
        vm.bz_throw(vm.bz_peek(0));
    }

    return 0;
}
```

Build a dynamic library for it (TODO: instructions for this) and you can use it in your buzz code:

```buzz
| main.buzz
import "mylib"

fun main([str] args) > void {
    assert(1 + 1 == 2, message: "Congrats on doing math!");
}
```

_Native_ functions have all the same signature `fn myfunction(vm: *VM) bool`. If values must be returned, push them on the stack and return `true`.

<p align="center">
    <img src="https://raw.githubusercontent.com/ziglang/logo/master/zig-logo-dark.svg" alt="zig" height="100">
</p>
