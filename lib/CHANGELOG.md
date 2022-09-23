# Revision history for significant-figures

## 0.2.0.0 -- 2022-09-22

* **BREAKING** Rename `Leaf` to `Literal`.
* **BREAKING** Parser parses expressions for both base and exponent in
  exponentiation. The evaluation step is now the step that errors when the
  exponent is not an integer. This allows support for more valid expressions.
* added pretty printing module, used in property-testing

## 0.1.0.1 -- 2022-09-06

* Add README to front page

## 0.1.0.0 -- 2022-09-05

* First version. Released on an unsuspecting world.
