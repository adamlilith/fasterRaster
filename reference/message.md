# Display warning or message

Display a warning or message if the given warning has not been displayed
since **fasterRaster** was attached or if a given number or hours has
passed since then.

## Usage

``` r
.message(msg, message)
```

## Arguments

- msg:

  Character: Name for the message (used internally). Should be able to
  be assigned to a list (i.e., no spaces, punctuation, etc.).

- message:

  Text for the message.

## Value

`TRUE` (invisibly).
