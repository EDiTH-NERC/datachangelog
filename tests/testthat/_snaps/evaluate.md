# changelog works

    Code
      evaluate(x = x, y = y, by = "car", report = TRUE, file = "_snaps/changelog.md",
        metadata = list(author = "Mario"))
    Condition
      Warning in `evaluate()`:
      Records present in `x` but not `y`: Mazda RX4
    Message
      Comparing common records between `x` and `y`.
    Condition
      Warning in `evaluate()`:
      Fields present in `x` but not `y`: mpg
    Message
      Comparing common fields between `x` and `y`.
    Condition
      Warning in `evaluate()`:
      Record values have changed between `x` and `y`.
    Output
             car column original replacement
      1 Merc 280   disp    167.6         150

