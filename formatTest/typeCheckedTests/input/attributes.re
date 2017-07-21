/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
/**
 * Generally, dangling attributes [@..] apply to everything to the left of it,
 * up until a comma, equals asignment, arrow, bar, or infix symbol (+/-) or
 * prefix.
 *
 * This has a nice side effect when printing the terms:
 * If a node has attributes attached to it,
 */

[@ocaml.text "Floating comment text should be removed"];

/**
 * Core language features:
 * ----------------------
 */

[@ocaml.doc "Floating doc text should be removed"];

[@itemAttributeOnTypeDef] [@ocaml.text "removed text on type def"]
type itemText = int;
type nodeText = [@ocaml.text "removed text on item"] int;
[@itemAttributeOnTypeDef]
[@ocaml.text "removed text on type def"]
type nodeAndItemText =
  [@ocaml.text "removed text on item"] int;

[@itemAttributeOnTypeDef] [@ocaml.doc "removed doc on type def"]
type itemDoc = int;
[@itemAttributeOnTypeDef]
type nodeDoc = [@ocaml.text "removed text on item"] int;
[@itemAttributeOnTypeDef] [@ocaml.doc "removed doc on type def"]
type nodeAndItemDoc =
  [@ocaml.text "removed text on item"] int;

[@itemAttributeOnTypeDef]
type x = int;
type attributedInt = [@onTopLevelTypeDef] int;

[@itemAttributeOnTypeDef]
type attributedIntsInTuple = ([@onInt] int, [@onFloat] float);

type myDataType('x,'y) = | MyDataType('x,'y);

type myType =
  [@onEntireType]
  myDataType ([@onOptionInt] option(int),
              [@onOption] option(float));


let thisInst : myType =
  [@attOnEntireDatatype] MyDataType(Some(10),Some(10.0));

let thisInst : myType =
  [@attOnEntireDatatype] MyDataType([@onFirstParam] Some(10), Some(10.0));

let x = ([@onHello] "hello");
let x = [@onHello] "hello";

let x = "hello" ++ ([@onGoodbye] "goodbye");
let x = ([@onHello] "hello") ++ "goodbye";
let x = [@onHello] "hello" ++ "goodbye";
let x = "hello" ++ [@onGoodbye] "goodbye";
let x = [@onEverything] ("hello" ++ "goodbye");

let x = 10 + ([@on20] 20);
let x = 10 + [@on20] 20;
let x = [@on10] 10  + 20;
let x = ([@on10] 10) + 20;
let x = [@attrEverything] (10 + 20);

let x = 10 - ([@on20] 20);
let x = 10 - [@on20] 20;
let x = [@on10] 10 - 20;
let x = ([@on10] 10) - 20;
let x = [@attrEntireEverything] (10 - 20);

let x = true && ([@onFalse] false);
let x = true && [@onFalse] false;
let x = [@onTrue] true && false;
let x = ([@onTrue] true) && false;
let x = [@attrEverything] (true && false);


/* now make sure to try with variants (tagged and `) */

/**
 * How attribute parsings respond to other syntactic constructs.
 */
let add(a) { [@onRet] a };
let add = fun(a) => [@onRet] a;
let add = [@onEntireFunction] (fun(a) => a);

let res = if (true) false else [@onFalse] false;
let res = [@onEntireIf] (if (true) false else false);


let add(a,b) = [@onEverything] ([@onA] a + b);
let add(a,b) = [@onEverything] ([@onA]a + ([@onB]b));
let add = (a,b) => a + [@onB]b;

let both = [@onEntireFunction](fun(a) => a);
let both(a,b) = [@onEverything]([@onA]a && b);
let both(a,b) = [@onA] a && [@onB] ([@onB] b);
let both = fun(a,b) => [@onEverything](a && b);

let thisVal = 10;
let x = 20 + - [@onFunctionCall] add(thisVal,thisVal);
let x = [@onEverything] (20 + - add(thisVal,thisVal));
let x = - [@onFunctionCall] add(thisVal,thisVal);
let x = [@onEverything] (- add(thisVal,thisVal));


let bothTrue(x,y) = {contents: x && y};
let something = [@onEverythingToRightOfEquals](bothTrue(true,true)^);
let something = ([@onlyOnArgumentToBang]bothTrue(true,true))^;

let res = [@appliesToEntireFunctionApplication] add(2,4);
 [@appliesToEntireFunctionApplication]add(2,4);


let myObj = {
  pub p () = {
    pub z () = 10
  };
};

let result = [@onSecondSend]([@attOnFirstSend]myObj#p ())#z ();

[@onRecordFunctions]
type recordFunctions = {
  p: (unit) => ([@onUnit] recordFunctions),
  q: [@onArrow] ((unit) => unit)
}
[@onUnusedType]
and unusedType = unit;
[@onMyRecord]
let rec myRecord = {
  p: fun () => myRecord,
  q: fun () => ()
}
[@onUnused]
and unused = ();
let result = [@onSecondSend]([@attOnFirstSend]myRecord.p()).q();

[@onVariantType]
type variantType =
   [@onInt] | Foo(int)
   | Bar ([@onInt] int)
   | Baz;

[@onVariantType]
type gadtType('x) =
   | Foo(int) : [@onFirstRow] gadtType(int)
   | Bar ([@onInt]int) : [@onSecondRow]gadtType(unit)
   | Baz: [@onThirdRow] gadtType ([@onUnit] unit);

[@floatingTopLevelStructureItem hello];
[@itemAttributeOnEval]
print_string("hello");

[@itemAttrOnFirst]
let firstBinding = "first"
[@itemAttrOnSecond]
and secondBinding = "second";

/**
 * Let bindings.
 * ----------------------
 */
let showLets () = [@onOuterLet] {
  let tmp = 20;
  [@onFinalLet] {
    let tmpTmp = tmp + tmp;
    tmpTmp + tmpTmp;
  }
};


/**
 * Classes:
 * ------------
 */
/**
 * In curried sugar, the class_expr attribute will apply to the return.
 */
[@moduleItemAttribute]
class boxA('a) (init: 'a) = [@onReturnClassExpr] {
  [@ocaml.text "Floating comment text should be removed"];
  [@ocaml.doc "Floating comment text should be removed"];
  pub pr = init + init + init;
};

/**
 * In non-curried sugar, the class_expr still sticks to "the simple thing".
 */
class boxB('a) =
  fun (init: 'a) => [@stillOnTheReturnBecauseItsSimple] {
    pub pr = init + init + init;
  };

/* To be able to put an attribute on just the return in that case, use
 * parens. */
[@onBoxC x ; y]
class boxC('a) = [@onEntireFunction] (
  fun (init: 'a) => (
    [@onReturnClassExpr] {
      pub pr = init + init + init;
    }
  )
);

[@moduleItemAttribute onTheTupleClassItem;]
class tupleClass('a,'b)(init: ('a, 'b)) {
  let one = [@exprAttr ten;] 10;
  let two = [@exprAttr twenty;] 20
  and three = [@exprAttr thirty;] 30;
  [@pr prMember;]
  pub pr = one + two + three;
};

[@structureItem]
class type addablePointClassType = {
  [@ocaml.text "Floating comment text should be removed"];
  [@ocaml.doc "Floating comment text should be removed"];
  pub x: int;
  pub y: int;
  pub add: (addablePointClassType, addablePointClassType) => int;
}
[@structureItem]
and anotherClassType = {
  pub foo: int;
  pub bar: int;
};

class type _x = [@bs]{ pub height : int };

class type _y { [@bs.set] pub height : int };

[@bs] class type _z { pub height : int };

module NestedModule {
  [@floatingNestedStructureItem hello];
};
[@structureItem]
module type HasAttrs = {
  [@onTypeDef]
  type t = int;
  [@floatingNestedSigItem hello];
  [@sigItem]
  class type foo = {pub foo: int; pub bar: int;};
  [@sigItem]
  class fooBar: (int) => foo;
  [@ocaml.text "Floating comment text should be removed"];
  [@ocaml.doc "Floating comment text should be removed"];
};

type s = S(string);

let S ([@onStr] str) = S ([@onHello]"hello");
let [@onConstruction](S(str))  = [@onConstruction](S("hello"));

type xy = | X(string)
          | Y(string);

let myFun = fun ([@onConstruction]X(hello) | [@onConstruction]Y(hello)) => hello;
let myFun = fun (X ([@onHello] hello ) | Y ([@onHello]hello )) => hello;

/* Another bug: Cannot have an attribute on or pattern
let myFun = fun ((X(hello) | Y(hello)) [@onOrPattern]) => hello;
*/

/* Bucklescript FFI item attributes */

[@bs.val]
external imul : (int, int) => int = "Math.imul";

let module Js {
  type t('a);
};

type classAttributesOnKeys = {
  .
  [@bs.set] key1 : string,

  /* The follow two are the same */
  [@bs.get {null}] key2 : [@onType2] Js.t(int),
  [@bs.get {null}] key3 : ([@onType2] (Js.t(int))),

  key4 : Js.t ([@justOnInt] int)
};

type attr = ..;

[@block]
type attr +=
  [@tag1] [@tag2] | Str
  [@tag3] | Float ;

type reconciler('props) = ..;

[@onVariantType]
type reconciler('props) +=
 | Foo(int) : [@onFirstRow] reconciler(int)
 | Bar ([@onInt] int) : [@onSecondRow] reconciler(unit)
 | Baz: [@onThirdRow] reconciler ([@onUnit] unit);

type element;

type reactElement;

type reactClass;

/* "react-dom" shouldn't spread the attribute over multiple lines */
[@bs.val] [@bs.module "react-dom"]
external render : (reactElement, element) => unit = "render";

[@bs.module "f"]
external f : (int) => int = "f";

[@bs.val] [@bs.module "react"] [@bs.splice]
external createCompositeElementInternalHack
  : (reactClass, Js.t({.. reasonProps : 'props}), array(reactElement)) => reactElement
  = "createElement";

external add_nat: (int, int) => int = "add_nat_bytecode" "add_nat_native";

[@@bs.module "Bar"] [@@ocaml.deprecated "Use bar instead. It's a much cooler function. This string needs to be a little long"]
external foo : (bool) => bool = "";

/* Attributes on an entire polymorphic variant leaf */
external readFileSync :
  name::string =>
  ([ `utf8
   | `my_name [@bs.as "ascii"]
   ] [@bs.string]) =>
  string = ""
  [@@bs.module "fs"];

external readFileSync2 :
  name::string =>
  ([ `utf8 [@bs.as "ascii"] | `my_name [@bs.as "ascii"] ] [@bs.string]) =>
  string = ""
  [@@bs.module "fs"];

/* Ensure that attributes on extensions are printed */
[@@@test
  [%%extension] [@@attr]; ];
