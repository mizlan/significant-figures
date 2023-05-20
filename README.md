# Significant Figures

![website](https://user-images.githubusercontent.com/44309097/187833301-db2c2a89-2562-42b3-a746-8c9a41a2bcae.png)

This repository contains:
* code for the `Data.SigFig` module that contains a variety of functions to
  parse and evaluate expressions involving significant figures, which is [→
  available on Hackage](https://hackage.haskell.org/package/significant-figures)
  as a library.
* code for a [→ frontend](https://significant-figures.herokuapp.com), along
  with a simple HTTP API
* code for a minimal CLI using Haskeline ([→
  wiki](https://github.com/judah/haskeline/wiki)).

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

> I created the distinction between antilogarithm and exponentiation to
> distinguish between which value is measured and which is constant. You'd use
> an antilogarithm, for instance, to calculate the molarity of H3O+ given pH,
> whereas exponentiation is nothing more than a shorthand for repeated
> multiplication.

For the rules regarding significant figures, [→ see here](http://cxp.cengage.com/contentservice/assets/owms01h/references/significantfigures/index.html).

Of course, you can use parentheses and more complex expressions:

```
expr> log(10.45) * 100c + 3.6200 * (9.4523 + 876.45) / 2c
1705.4 (5 s.f.)
```

Support for general arbitrary-precision arithmetic on the rationals:
```
expr> 4.52c * 100c * (1c/60c)
113/15 (non-terminating const)
```

Super-smart display that shows scientific notation when necessary, and
annotation for significant figures:
```
expr> 0.650 * 4000.
2.60 x 10^3 (3 s.f.)
```

## API

The API is simple; there is an endpoint at
[→ `/calc`](https://significant-figures.herokuapp.com/calc). You can perform either:
* a `GET` request with a single URL-encoded parameter `"expr"` that corresponds
  with the expression string
* a `POST` request with a JSON body with a single field `"input"` that
  corresponds with the expression string.

## Reminders

- Significant figures are **correctly calculated by postponing rounding until the
  end of a sequence of similar operations**; i.e., rounding to least decimal
  place only occurs after all addition/subtraction operations in a row are
  computed. Internally, this is done by parsing such runs as a list of operands
  and associated operations instead of treating operators as binary.

- Functions that return irrational numbers do not work when given a constant
  value as input, since constants are represented internally as rationals, and
  therefore the essence of a "constant" in the context of significant figures
  (i.e., "infinite" significant figures") cannot be guaranteed. Since using a
  symbolic math engine is complete overkill for such a calculator, we limit the
  domain of these functions. **NOTE:** This doesn't mean you can't use
  constants within the argument expression, it just means the expression must
  not _evaluate_ to a constant. For example, with the `log()` function,
  `log(45c + 2)` (`log(47)`) is perfectly fine, but `log(45c + 2c)` (`log(47c)`)
  is not.

- Zero is a weird number for significant figures. First, [→ no measurement
  should _ever_ be recorded as
  0](https://math.stackexchange.com/questions/2149316/how-many-significant-figures-in-0-0),
  or 0.0, or 0.00 or 0.0e2 or anything like that, since significant figures
  work on relative precision and that breaks down once you just have 0. But
  that's not the end of the story. Consider a case where you take a difference
  of two measurements and end up with 0, or really any situation in which you
  end up with 0 as an intermediate/final result. In this case, you _do_ have a
  good idea of what precision you have, but it becomes incredibly difficult to
  work since all notion of _significant decimal places_ is lost. Furthermore,
  you may think of a workaround, in which `0.000` and `000.0` (alternatively
  written `0.000 x 10^2`) as _distinct_ values in the context of significant
  figures. The below doesn't feel right to me:

  `exp(000.0) = 10 ^ (100c * 0.000) = 1`

  `exp(0.000) = 10 ^ (  1c * 0.000) = 1.00`

  I may work on a patch for this if I think it necessary. This will involve
  storing another piece of metadata for each value: its rightmost (or leftmost,
  given the total number of significant figures) significant decimal place.

  Since I have concluded there is no trivial general solution to this, I
  have chosen to retain the number of significant figures for 0-values, such
  that they work as expected in larger expressions. Also, this is an incredibly
  niche case that should not ever appear in well-designed lab procedures.

  TL;DR: How zeroes work is pretty controversial but it should work the way
  you'd expect them to intuitively (I asked other people how they'd expect
  zeroes to work and it was the way it currently is).
