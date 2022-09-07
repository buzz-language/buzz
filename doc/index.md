# Buzz

For std lib documentation see [here](std.md).

## Table of contents

- [Strings](#strings)
- [Lists](#lists)
- [Maps](#maps)
- [Patterns](#patterns)
- [Fibers](#fibers)

## Strings

### `fun len() > num`
**Returns:** Length of the string

### `fun upper() > str`
**Returns:** Uppercased string

### `fun lower() > str`
**Returns:** Lowercased string

### `fun trim() > str`
**Returns:** Trimmed string

### `fun byte(num at) > num`
Get byte value of character
- **`at`**: index of character in the string

**Returns:** Byte value of character

### `fun indexOf(str needle) > num?`
Find needle in string
- **`needle`**: needle to find

**Returns:** Index of found match

### `fun startsWith(str needle) > bool`
Test if strings starts with needle
- **`needle`**: needle to find

**Returns:** `true` if strings starts with needle

### `fun endsWith(str needle) > bool`
Test if strings ends with needle
- **`needle`**: needle to find

**Returns:** `true` if strings ends with needle


### `fun replace(str needle, str with) > str`
Replace first occurence of needle
- **`needle`**: needle to find
- **`with`**: replacement

**Returns:** New string

### `fun split(str separator) > [str]`
Split string
- **`separator`**: separator by which the string will be split

**Returns:** Splitted string

### `fun sub(num start, num? len) > str`
Get sub string
- **`start`**: Sub string start index
- **`end`**: Length of sub string, if `null` will go until end of string

**Returns:** The substring

### `fun repeat(num n) > str`
Repeat string
- **`n`**: How many times the string will be repeated

**Returns:** New string

### `fun encodeBase64() > str`
Base64 encode the string

**Returns:** Encoded string


### `fun decodeBase64() > str`
Base64 decode the string

**Returns:** Decoded string

## Lists

### `fun append(T value) > void`
Append new element at end of the list
- **`value`**: New element

### `fun remove(num at) > T`
Remove element form the list shifting elements after index
- **`at`**: Index of element to remove

**Returns:** Removed element

### `fun len() > num`
**Returns:** Length of list

### `fun next(num key) > num?`
Given index, returns next index or null if end of list is reached (function used by `foreach`).
- **`key`**: current index

**Returns:** Next index

### `fun sub(num start, num? len) > [T]`
Get sub list
- **`start`**: Start index of sub list
- **`len`**: Length of sub list, if `null` will go to end of list

**Returns:** Sub list

### `fun indexOf(T needle) > num?`
Search first occurence of the needle
- **`needle`**: Element to find

**Returns:** Index of element or `null` if not found

### `fun join(str separator) > str`
Join list element in a string with a separator. Elements are converted to a string just like an interpolation would.
- **`separator`**: Separator to put between each elements

**Returns:** Elements joined as a string


## Maps

### `fun size() > num`
**Returns:** Number of elements in the map

### `fun remove(K key) > V?`
Remove element from the map
- **`key``**: Key of element to remove

**Returns:** Removed element or `null` if nothing was under `key`

### `fun keys() > [K]`
**Returns:** Return list of the map keys

### `fun values() > [V]`
**Returns:** Return list of the map values

## Patterns

## `fun match(str subject) > [str]?`
Get first match of the pattern against a string
- **`subject`**: Subject to match the pattern against

**Returns:** List of match and captures or `null` if nothing matches

## `fun matchAll(str subject) > [[str]]?`
Get all matches of the pattern against a string
- **`subject`**: Subject to match the pattern against

**Returns:** List of matches or `null` if nothing matches

## Fibers

## `fun over() > bool`
**Returns:** `true` if fiber is over

## `fun cancel() > void`
Cancel the fiber by changing its internal status to `over` preventing further `resume` or `resolve`