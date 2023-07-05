# Buzz

For std lib documentation see [here](std.md).

## Table of contents

- [Strings](#strings)
- [Lists](#lists)
- [Maps](#maps)
- [Patterns](#patterns)
- [Fibers](#fibers)

## Strings

### `fun len() > int`
**Returns:** Length of the string

### `fun upper() > str`
**Returns:** Uppercased string

### `fun lower() > str`
**Returns:** Lowercased string

### `fun trim() > str`
**Returns:** Trimmed string

### `fun byte(int at) > int`
Get byte value of character
- **`at`**: index of character in the string

**Returns:** Byte value of character

### `fun indexOf(str needle) > int?`
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

### `fun sub(int start, int? len) > str`
Get sub string
- **`start`**: Sub string start index
- **`end`**: Length of sub string, if `null` will go until end of string

**Returns:** The substring

### `fun repeat(int n) > str`
Repeat string
- **`n`**: How many times the string will be repeated

**Returns:** New string

### `fun encodeBase64() > str`
Base64 encode the string

**Returns:** Encoded string


### `fun decodeBase64() > str`
Base64 decode the string

**Returns:** Decoded string

### `fun hex() > str`
Converts binary string to hexadecimal representation

**Returns:** Hex string

### `fun bin() > str`
Converts hexadecimal string to binary string

**Returns:** Binary string

## Lists

### `fun append(T value) > T`
Append new element at end of the list
- **`value`**: New element

**Returns:** The appended value

### `fun insert(int index, T value) > T`
Insert new element at `index` of the list. If `index` is less than `0`, element will be inserted at `0`. If `index` is greather than list length, it'll be appended at the end of the list.
- **`index`**: Index at which element will be inserted
- **`value`**: New element

**Returns:** The inserted value

### `fun remove(int at) > T`
Remove element form the list shifting elements after index
- **`at`**: Index of element to remove

**Returns:** Removed element

### `fun pop() > T?`
Remove and return last element of list or `null` if list is empty

**Returns:** Last element

### `fun len() > int`
**Returns:** Length of list

### `fun sub(int start, int? len) > [T]`
Get sub list
- **`start`**: Start index of sub list
- **`len`**: Length of sub list, if `null` will go to end of list

**Returns:** Sub list

### `fun indexOf(T needle) > int?`
Search first occurence of the needle
- **`needle`**: Element to find

**Returns:** Index of element or `null` if not found

### `fun join(str separator) > str`
Join list element in a string with a separator. Elements are converted to a string just like an interpolation would.
- **`separator`**: Separator to put between each elements

**Returns:** Elements joined as a string

### `fun forEach(Function callback(int index, T element) > void) > void`
Runs `callback` for each element of the list.
- **`callback`:** Ran for each element

### `fun sort(Function callback(T left, T right) > bool) > [T]`
Stable in-place sort. O(n) best case, O(n*log(n)) worst case and average case.
- **`callback`:** Used to compare elements

**Returns:** The list now sorted

### `fun map(<S>, Function callback(int index, T element) > S) > [S])`
Map list to new list of target type by running `callback` for each element of the list.
- **`callback`:** Ran for each element

**Returns:** New list of item type `S`

### `fun filter(Function callback(int index, T element) > bool) > [T])`
Filter list keeping element for which `callback` returns `true`.
- **`callback`:** Ran for each element

**Returns:** Filtered list

### `fun reduce(Function callback(<S>, int index, T element, S accumulator) > S, S initial) > S)`
Reduce list to value of type `S` by running `callback` with `accumulator` being the value being built.
- **`callback`:** Ran for each element
- **`initial`:** Initial value

**Returns:** Reduced value

## Maps

### `fun size() > int`
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