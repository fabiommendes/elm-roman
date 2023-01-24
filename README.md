# Roman numerals

Read and write roman numerals.

Exposes two functions: toRoman and fromRoman, which are responsible for the conversion
between integers and strings representing roman numerals.


# Examples

The two functions are straightforward

```elm
import Roman as exposing (..)


toRoman 4 ==> "IV"
fromRoman "IV" ==> Just 4
```