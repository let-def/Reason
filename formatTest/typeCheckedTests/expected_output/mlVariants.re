// Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
type polyVariantsInMl = [
  | `IntTuple(int, int)
  | `StillAnIntTuple(int, int)
];

let intTuple = `IntTuple((1, 2));

let stillAnIntTuple = `StillAnIntTuple((4, 5));

let sumThem =
  fun
  | `IntTuple(x, y) => x + y
  | `StillAnIntTuple(a, b) => a + b;

type nonrec t =
  | A(int)
  | B(bool);
