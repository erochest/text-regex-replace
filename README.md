
# text-regex-replace

![Travis CI status](https://travis-ci.org/erochest/text-regex-replace.svg?branch=master)

Easy replacement when using text-icu regexes.

## Installation

To install this, just use [`stack`](https://docs.haskellstack.org/en/stable/README/).

## Usage

The syntax used with `OverloadedStrings` is meant to be similar to that used in other regular expression libraries in other programming languages.

Generally, input text is considered to be static.

```haskell
>>> replaceAll "a" "b" "aaa"
"bbb"
>>> replaceAll "ab" "ba" "cdababcd"
"cdbabacd"
```

However, groups from the regular expression's matches can be insert using `$1` (to insert the first group) or `${7}` (to insert the seventh group).

```haskell
>>> replaceAll "(.*), (.*)" "$2 $1" "Beeblebrox, Zaphod"
"Zaphod Beeblebrox"
>>> replaceAll "4(\\d)" "${1}4" "7458"
"7548"
```

Dollar signs can be included in the output by doubling them (`$$`).

```haskell
>>> replaceAll "(\\d+\\.\\d+)" "$$$1" "9.99"
"$9.99"
```

## Contributors!

Thank you!

* [Kostiantyn Rybnikov](https://github.com/k-bx)
* [Maciej Bielecki](https://github.com/zyla)
* [Peter Becich](https://github.com/peterbecich)

