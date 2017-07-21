type point = {
  x: int,
  y: int
};

let id (x) = x;

type myVariant =
  | TwoCombos(inner, inner)
  | Short
  | AlsoHasARecord(int, int, point)
and inner =
  | Unused
  | HeresTwoConstructorArguments(int, int);

let computeTuple (a, b, c, d, e, f, g, h) = (
  a + b,
  c + d,
  e + f,
  g + h
);

let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b)
    ) => (
      x,
      y,
      a,
      b
    )
  | TwoCombos(_, _) => (0, 0, 0, 0)
  | Short
  | AlsoHasARecord(300, _, _) => (
      100000,
      100000,
      100000,
      100000
    )
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    computeTuple(
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      two,
      two,
      two
    )
  };


/**
 * Match bodies may include sequence expressions, but without the `{}`
 * braces required.
 */
let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b)
    ) =>
    let ret = (x, y, a, b);
    ret
  | TwoCombos(_, _) =>
    /**
     * See, no braces required - saves indentation as well!
     */
    let ret = (0, 0, 0, 0);
    ret
  | Short
  | AlsoHasARecord(300, _, _) =>
    /**
     * And no final semicolon is required.
     */
    let ret = (100000, 100000, 100000, 100000);
    ret
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    computeTuple(
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      two,
      two,
      two
    )
  };


/**
 * Ensure that nested Pexp_functions are correctly wrapped in parens.
 *
 */
let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b)
    ) => (
      fun
      | Some(x) => x + 1
      | None => 0
    )
  | TwoCombos(_, _) =>
    let x = (
      fun
      | Some(x) => x + 1
      | None => 0
    );
    x
  | Short
  | AlsoHasARecord(300, _, _) =>
    id(
      fun
      | Some(x) => x + 1
      | None => 0
    )
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    id(
      fun
      | Some(x) => x + 1
      | None => 0
    )
  };
