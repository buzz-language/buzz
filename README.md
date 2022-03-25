<p align="center">
    <img src="https://github.com/giann/buzz/raw/main/logo.png" alt="buzz" width="204" height="204">
</p>

# 👨‍🚀 buzz

A small/lightweight typed scripting language written in Zig

<p align="center">
    <img src="https://github.com/giann/buzz/raw/main/example.png" alt="buzz code example">
</p>

**Note: This is very much in development. Seriously, don't even try to build it.**

# Goals

- Small in size and complexity (just a bit more than Lua though)
- Strict typing
- Unambiguous
- No nonsense coercion
- TBD: coroutines?

# Quick tour

You can also take a look at `tests/` for more examples.

## Types and variables

```buzz
| Basic types
bool aBoolean = true;
str aString = "hello world";
num aNumber = 23;

| A constant
const num pi = 3.14;

| Data structures
[num] aListOfNumbers = [1, 2, 3];
{str, num} aMap = {
    "one": 1,
    "two": 2,
    "three": 3,
};
```

### Optionals

```buzz
str? aStringOrNull = "hello";

| Null coalescing operator is `??`
str unwrapped = aStringOrNull ?? "default value"

| Force unwrapping with `!`
str unwrapped = aStringOrNull!;

| Graceful unwrapping
[num]? optList = null;

print(optList?.len()); | -> null
```

## Functions

```buzz
fun sayHiTo(str name, str? lastName, num age) > str {
    | Interpolation with `{}`
    return "Hi {name} {lastName ?? ""}!"
}
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

## Enums

```buzz
| Enums can have a type, if none is specified the type is `num` and values are ordinal.
| If a type is specified, all values must be initialized.
enum(str) Country {
    usa = "United States of America",
    uk = "United Kingdoms",
    fr = "France",
}

| To get the value associated with a enum case
print(Country.usa.value); | -> "United States of America"
```

## Control flow

```buzz
| The usual
if (someCondition) {
    | ...
} else if (anotherCondition) {
    | ...
} else {
    | ...
}

num i = 0;
while (i < 10) {
    i = i + 1;
}

num j = 10;
do {
    j = j - 1;
} until (j == 10)

for (num i = 0; i < 10; i = i + 1) {
    | ...
    break;
}
```

### `foreach`

`foreach` can iterate over most data structures:

```buzz
foreach (SomeEnum case in SomeEnum) {
    | ...
}

foreach (num i, str value in listOfStrings) {
    | ...
}

foreach (str key, num value in aMap) {
    | ...
}
```

## Objects and Classes

An `object` is like a class except it can't be inherited from and can't inherit from anything:

```buzz
object Person {
    static population = 0;

    str name = "Joe", | Fields can have default values
    num age = 35,

    | Method
    fun sayHello() > void {
        print("Hello {this.name}");
    }

    | Object and classes don't have constructor but you can implement one with a static method
    static init(str name, num age) > Person {
        Person.population = Person.population + 1;

        return Person {
            name = name,
            age = age,
        };
    }
}
```

`class` act like you would expect. They don't have the central place they have in other languages (tbh I may end up removing them):

```buzz
class Form {
    num x,
    num y,

    fun toString() > str {
        return "({this.x}, {this.y})";
    }
}

| `Circle` inherits from `Form`
class Circle < Form {
    num radius,

    fun toString() > str {
        return "center: {super.toString()}, radius: {this.radius}";
    }
}
```

## Errors

Right now errors can be anything.

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

fun willFail() > num {
    throw MyErrors.failed;

    return 0;
}

| Use default value in case of any error
num result = willFail() catch 0;

| Handle different type of errors
num result = willFail() catch {
    (MyErrors e) -> 0,
    (OtherErrors e) -> 1,
    default {
        | Something unexpected
        os.exit(1);
    }
}
```

## Import/Export

```buzz
| hello.buzz

| Import std lib
import "lib/std.buzz";

fun sayHello() > void {
    print("Hello world!");
}

| Make it visible when imported
export sayHello;
```

```buzz
| main.buzz
import "hello.buzz";

func main() > void {
    sayHello();
}
```

## Call C/Zig code

First define the buzz interface. The `extern` keyword means that buzz we'll look for a dynamic library named `libmylib.dylib` (only dylib right now):

```buzz
| mylib.buzz
extern fun assert(bool condition, str message) > void
```

Then implement it in Zig or C using the [buzz_api](https://github.com/giann/buzz/blob/main/lib/buzz_api.zig):

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
import "mylib.buzz"

fun main() > void {
    assert(1 + 1 == 2, message: "Congrats on doing math!");
}
```

_Native_ functions have all the same signature `fn myfunction(vm: *VM) bool`. If values must be returned, push them on the stack and return `true`.
