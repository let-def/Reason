open Ast_404

open Asttypes
open Ast_mapper
open Parsetree
open Longident


(** [is_prefixed prefix i str] checks if prefix is the prefix of str
  * starting from position i
  *)
let is_prefixed prefix str i =
  let len = String.length prefix in
  let j = ref 0 in
  while !j < len && String.unsafe_get prefix !j =
                    String.unsafe_get str (i + !j) do
    incr j
  done;
  (!j = len)

(**
 * pick_while returns a tuple where first element is longest prefix (possibly empty) of the list of elements that satisfy p
 * and second element is the remainder of the list
 *)
let rec pick_while p = function
  | [] -> [], []
  | hd::tl when p hd ->
                  let (satisfied, not_satisfied) = pick_while p tl in
                  hd :: satisfied, not_satisfied
  | l -> ([], l)


(** [find_substring sub str i]
    returns the smallest [j >= i] such that [sub = str.[j..length sub - 1]]
    raises [Not_found] if there is no such j
    behavior is not defined if [sub] is the empty string
*)
let find_substring sub str i =
  let len = String.length str - String.length sub in
  let found = ref false and i = ref i in
  while not !found && !i <= len do
    if is_prefixed sub str !i then
      found := true
    else
      incr i;
  done;
  if not !found then
    raise Not_found;
  !i

(** [replace_string old_str new_str str] replaces old_str to new_str in str *)
let replace_string old_str new_str str =
  match find_substring old_str str 0 with
  | exception Not_found -> str
  | occurrence ->
    let buffer = Buffer.create (String.length str + 15) in
    let rec loop i j =
      Buffer.add_substring buffer str i (j - i);
      Buffer.add_string buffer new_str;
      let i = j + String.length old_str in
      match find_substring old_str str i with
      | j -> loop i j
      | exception Not_found ->
        Buffer.add_substring buffer str i (String.length str - i)
    in
    loop 0 occurrence;
    Buffer.contents buffer

let split_lines str =
  let rec loop acc i =
    match String.rindex_from str i '\n' with
    | exception Not_found -> String.sub str 0 (i + 1) :: acc
    | j -> loop (String.sub str (j + 1) (i - j) :: acc) (j - 1)
  in
  loop [] (String.length str - 1)

let rec trim_right_idx str idx =
  if idx = -1 then 0
  else
    match String.get str idx with
    | '\t' | ' ' | '\n' | '\r' -> trim_right_idx str (idx - 1)
    | _ -> idx + 1

let trim_right str =
  let length = String.length str in
  if length = 0 then ""
  else
    let index = trim_right_idx str (length - 1) in
    if index = 0 then ""
    else if index = length then str
    else String.sub str 0 index

let strip_trailing_whitespace str =
  str
  |> split_lines
  |> List.map trim_right
  |> String.concat "\n"
  |> String.trim

module StringMap = Map.Make (String)


(** Generate a suitable extension node for Merlin's consumption,
    for the purposes of reporting a syntax error - only used
    in recovery mode.
 *)
let syntax_error_extension_node loc message =
  let str = Location.mkloc "merlin.syntax-error" loc in
  let payload = PStr [{
    pstr_loc = Location.none;
    pstr_desc =
      Pstr_eval (
        {
          pexp_loc = Location.none;
          pexp_desc = Pexp_constant (Parsetree.Pconst_string (message, None));
          pexp_attributes = [];
        },
        []
      );
  }]
 in
 (str, payload)

(** Check to see if the string `s` is made up of `keyword` and zero or more
    trailing `_` characters. *)
let potentially_conflicts_with ~keyword s =
  let s_length = String.length s in
  let keyword_length = String.length keyword in
  (* It can't be a match if s is shorter than keyword *)
  s_length >= keyword_length && (
    try
      (* Ensure s starts with keyword... *)
      for i = 0 to keyword_length - 1 do
        if keyword.[i] <> s.[i] then raise Exit;
      done;
      (* ...and contains nothing else except trailing _ characters *)
      for i = keyword_length to s_length - 1 do
        if s.[i] <> '_' then raise Exit;
      done;
      (* If we've made it this far there's a potential conflict *)
      true
    with
    | Exit -> false
  )

(** Add/remove an appropriate suffix when mangling potential keywords *)
let string_add_suffix x = x ^ "_"
let string_drop_suffix x = String.sub x 0 (String.length x - 1)

(** What do these *_swap functions do? Here's an example: Reason code uses `!`
    for logical not, while ocaml uses `not`. So, for converting between reason
    and ocaml syntax, ocaml `not` converts to `!`, reason `!` converts to
    `not`.

    In more complicated cases where a reserved keyword exists in one syntax but
    not the other, these functions translate any potentially conflicting
    identifier into the same identifier with a suffix attached, or remove the
    suffix when converting back. Two examples:

    reason to ocaml:

    pub: invalid in reason to begin with
    pub_: pub
    pub__: pub_

    ocaml to reason:

    pub: pub_
    pub_: pub__
    pub__: pub___

    =====

    reason to ocaml:

    match: match_
    match_: match__
    match__: match___

    ocaml to reason:

    match: invalid in ocaml to begin with
    match_: match
    match__: match_
*)

let reason_to_ml_swap = function
  | "!" -> "not"
  | "^" -> "!"
  | "++" -> "^"
  | "===" -> "=="
  | "==" -> "="
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  | "\\!==" -> "!=="
  |  "\\===" -> "==="
  | "!=" -> "<>"
  | "!==" -> "!="
  | x when (
    potentially_conflicts_with ~keyword:"match" x
    || potentially_conflicts_with ~keyword:"method" x
    || potentially_conflicts_with ~keyword:"private" x) -> string_add_suffix x
  | x when (
    potentially_conflicts_with ~keyword:"switch_" x
    || potentially_conflicts_with ~keyword:"pub_" x
    || potentially_conflicts_with ~keyword:"pri_" x) -> string_drop_suffix x
  | everything_else -> everything_else

let ml_to_reason_swap = function
  | "not" -> "!"
  | "!" -> "^"
  | "^" -> "++"
  | "==" -> "==="
  | "=" -> "=="
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  | "!==" -> "\\!=="
  |  "===" -> "\\==="
  | "<>" -> "!="
  | "!=" -> "!=="
  | x when (
    potentially_conflicts_with ~keyword:"match_" x
    || potentially_conflicts_with ~keyword:"method_" x
    || potentially_conflicts_with ~keyword:"private_" x) -> string_drop_suffix x
  | x when (
    potentially_conflicts_with ~keyword:"switch" x
    || potentially_conflicts_with ~keyword:"pub" x
    || potentially_conflicts_with ~keyword:"pri" x) -> string_add_suffix x
  | everything_else -> everything_else

let swap_txt map txt =
  if StringMap.mem txt map then
    StringMap.find txt map
  else
    txt

(** identifier_mapper maps all identifiers in an AST with a mapping function f
  this is used by swap_operator_mapper right below, to traverse the whole AST
  and swapping the symbols listed above.
  *)
let identifier_mapper f super =
{ super with
  expr = begin fun mapper expr ->
    let expr =
      match expr with
        | {pexp_desc=Pexp_ident ({txt} as id);
           pexp_loc;
           pexp_attributes} ->
             let swapped = match txt with
               | Lident s -> Lident (f s)
               | Ldot(longPrefix, s) -> Ldot(longPrefix, f s)
               | Lapply (y,s) -> Lapply (y, s)
             in
             {expr with pexp_desc=Pexp_ident ({id with txt=swapped})}
        | _ -> expr
    in
    super.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let pat =
      match pat with
        | {ppat_desc=Ppat_var ({txt} as id);
           ppat_loc;
           ppat_attributes} ->
             {pat with ppat_desc=Ppat_var ({id with txt=(f txt)})}
        | _ -> pat
    in
    super.pat mapper pat
  end;
  signature_item = begin fun mapper signatureItem ->
    let signatureItem =
      match signatureItem with
        | {psig_desc=Psig_value ({pval_name} as name);
           psig_loc} ->
            {signatureItem with psig_desc=Psig_value ({name with pval_name=({pval_name with txt=(f name.pval_name.txt)})})}
        | _ -> signatureItem
    in
    super.signature_item mapper signatureItem
  end;
}

(** escape_stars_slashes_mapper escapes all stars and slases in an AST *)
let escape_stars_slashes_mapper =
  let escape_stars_slashes str =
    if String.contains str '/' then
      replace_string "/*" "/\\*" @@
      replace_string "*/" "*\\/" @@
      replace_string "//" "/\\/" @@
      str
    else
      str
  in
  identifier_mapper escape_stars_slashes

(* To be used in parser, transform a token into an ast node with different identifier
 *)
let reason_to_ml_swap_operator_mapper = identifier_mapper reason_to_ml_swap

(* To be used in printer, transform an ast node into a token with different identifier
 *)
let ml_to_reason_swap_operator_mapper = identifier_mapper ml_to_reason_swap

(* attribute_equals tests an attribute is txt
 *)
let attribute_equals to_compare = function
  | ({txt; _}, _) -> txt = to_compare

(* attribute_exists tests if an attribute exists in a list
 *)
let attribute_exists txt attributes = List.exists (attribute_equals txt) attributes

(* conflicted_attributes tests if both attribute1 and attribute2
 * exist
 *)
let attributes_conflicted attribute1 attribute2 attributes =
  attribute_exists attribute1 attributes &&
  attribute_exists attribute2 attributes

(* normalized_attributes removes attribute from a list of attributes
 *)
let normalized_attributes attribute attributes =
  List.filter (fun x -> not (attribute_equals attribute x)) attributes

(* apply_mapper family applies an ast_mapper to an ast *)
let apply_mapper_to_structure s mapper = mapper.structure mapper s
let apply_mapper_to_signature s mapper = mapper.signature mapper s
let apply_mapper_to_type      s mapper = mapper.typ       mapper s
let apply_mapper_to_expr      s mapper = mapper.expr      mapper s
let apply_mapper_to_pattern   s mapper = mapper.pat       mapper s

let apply_mapper_to_toplevel_phrase toplevel_phrase mapper =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_to_structure x mapper)
  | x -> x

let apply_mapper_to_use_file use_file mapper =
  List.map (fun x -> apply_mapper_to_toplevel_phrase x mapper) use_file

(* The following logic defines our own Error object
 * and register it with ocaml so it knows how to print it
 *)

type error = Syntax_error of string

exception Error of Location.t * error

let report_error ppf (Syntax_error err) =
  Format.(fprintf ppf "%s" err)

let () =
  Location.register_error_of_exn
    (function
     | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
     | _ ->
        None
     )

let map_first f = function
  | [] -> invalid_arg "Syntax_util.map_first: empty list"
  | x :: xs -> f x :: xs

let map_last f l =
  match List.rev l with
  | [] -> invalid_arg "Syntax_util.map_last: empty list"
  | x :: xs -> List.rev (f x :: xs)

type menhirMessagesError = {
  msg: string;
  loc: Location.t;
}

type menhirError =
  | NoMenhirMessagesError
  | MenhirMessagesError of menhirMessagesError

let menhirMessagesError = ref [NoMenhirMessagesError]

let findMenhirErrorMessage loc =
    let rec find messages =
      match messages with
      | MenhirMessagesError err :: tail when err.loc = loc -> MenhirMessagesError err
      | _ :: tail -> find tail
      | [] -> NoMenhirMessagesError
    in find !menhirMessagesError

let add_error_message err =
  let msg = try
    ignore (find_substring "UNKNOWN SYNTAX ERROR" err.msg 0);
    [MenhirMessagesError {err with msg = "A syntax error occurred. Help to improve this message: https://github.com/facebook/reason/blob/master/src/README.md#add-a-menhir-error-message"}]
  with
  | Not_found -> [MenhirMessagesError err]
  in
  menhirMessagesError := !menhirMessagesError @ msg

(** Returns true if loc1 is before loc2 *)
let location_before loc1 loc2 =
  let open Location in
  loc1.loc_end.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum

(** Returns true if loc1 contains loc2 *)
let location_contains loc1 loc2 =
  let open Location in
  loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum &&
  loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum

