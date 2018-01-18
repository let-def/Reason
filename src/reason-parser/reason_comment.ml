open Location

type category =
  | EndOfLine
  | SingleLine
  | Regular

let string_of_category = function
  | Regular -> "Regular"
  | EndOfLine -> "End of Line"
  | SingleLine -> "SingleLine"

type t = {
  location: Location.t;
  category: category;
  text: string;
}

let category t = t.category

let is_single_line t = match t.category with
  | SingleLine -> true
  | _ -> false

let dump ppf t =
  let open Lexing in
  Format.fprintf ppf "%d (%d:%d)-%d (%d:%d) -- %s:||%s||"
    t.location.loc_start.pos_cnum
    t.location.loc_start.pos_lnum
    (t.location.loc_start.pos_cnum - t.location.loc_start.pos_bol)
    t.location.loc_end.pos_cnum
    t.location.loc_end.pos_lnum
    (t.location.loc_end.pos_cnum - t.location.loc_end.pos_bol)
    (string_of_category t.category)
    t.text

let dump_list ppf list =
  List.iter (Format.fprintf ppf "%a\n" dump) list

let wrap ?(is_doc=false)t =
  match t.text with
  | "" when is_doc -> "/**/"
  | txt when is_doc -> "/**" ^ txt ^ "*/"
  | "" | "*" -> "/***/"
  | txt when txt.[0] = '*' && txt.[1] <> '*' -> "/**" ^ txt ^ "*/"
  | txt -> "/*" ^ txt ^ "*/"

let is_doc t =
  String.length t.text > 0 && t.text.[0] == '*'

let make ~location category text =
  { text; category; location }

let align_lines str =
  match Syntax_util.split_by ~keep_empty:true ((=) '\n') str with
  | [] -> ""
  | [x] -> x
  | first :: rest ->
    let leading_spaces =
      let count_leading acc str =
        if str = "" then acc
        else
          let len = min acc (String.length str) and i = ref 0 in
          while !i < len && str.[!i] = ' ' do incr i done;
          !i
      in
      List.fold_left count_leading max_int rest
    in
    let leading_spaces = max 0 (leading_spaces - 1) in
    let chop str =
      String.sub str leading_spaces (String.length str - leading_spaces)
    in
    String.concat "\n" (first :: List.map chop rest)
