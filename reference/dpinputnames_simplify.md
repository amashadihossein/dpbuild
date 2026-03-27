# Make dpinput names simplified

This function tries to drop the full descriptive name of dpinput
elements for code aesthetics

## Usage

``` r
dpinputnames_simplify(x, make_unique = FALSE)
```

## Arguments

- x:

  a character string of the form `{path}/{file_name.extension}/{sha1}`
  which will be converted to a character string of the form
  `{file_name}`

- make_unique:

  if TRUE it ensures each element of a vector names end up being unique.
  If not it errors if not simplified names not unique.

## Value

the code friendly converted character string
