Some (1, 2, 3);

type bcd =
  | TupleConstructor (int, int)
  | MultiArgumentsConstructor int int;

let a = TupleConstructor (1, 2);

let b =
  MultiArgumentsConstructor 1 2
  [@implicit_arity];

module Test = {
  type a =
    | And (int, int)
    | Or (int, int);
};

let _ = Test.And (1, 2);

let _ = Test.Or (1, 2);

let _ = Some 1;

module M = {
  type t =
    | TupleConstructorInModule (int, int);
  type t2 =
    | TupleConstructor2 (int, int);
  type t3 =
    | TupleConstructor3 (int, int);
};

type t2 =
  | TupleConstructor2 (int, int);

type t3 =
  | TupleConstructor3 (int, int);

let _ = M.TupleConstructorInModule (1, 2);

let _ = M.TupleConstructor2 (1, 2);

let _ = TupleConstructor2 (1, 2);

let _ =
  M.TupleConstructor3 1 2 [@implicit_arity];

let _ = TupleConstructor3 (1, 2);
