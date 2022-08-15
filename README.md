# Significant Figures

## Supported Operations

| Operation | Example | Output |
| --- | --- | --- |
| addition | `2.0 + 4.31` | `6.3` |
| subtraction | `5.1 - 2` | `3` |
| multiplication | `4.8 * 5.2` | `25` |
| division | `4.00 / 3.1` | `1.3` |
| constants | `2c * 572.138` | `1144.28` |
| logarithms (`log(10, m)`) where `m` is measured | `log(10)` | `1.0` |
| antilogarithms (`10 ^ m`) where `m` is measured | `exp(6.24)` | `1.7 x 10^6` |
| exponentiation (`m ^ n`) where `m` is measured | `4.0 ** 4` | `2.6 x 10^2` |

For the rules regarding significant figures, [→ see here](http://cxp.cengage.com/contentservice/assets/owms01h/references/significantfigures/index.html).

> I created the distinction between antilogarithm and exponentiation for the
> purposes of distinguishing between which value is measured and which is
> constant

Of course, you can use parentheses and more complex expressions. Significant
figures are **correctly calculated by postponing rounding until the end of a
sequence of similar operations**; i.e., rounding to least decimal place only
occurs after all addition/subtraction operations in a row are computed.
Internally, this is done by parsing such runs as a list of operands and
associated operations instead of treating operators as binary.
