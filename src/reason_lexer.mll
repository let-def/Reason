(*
 *  Copyright (c) 2015-present, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
 *
 *  Forked from OCaml, which is provided under the license below:
 *
 *  Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *
 *  Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 Inria
 *
 *  Permission is hereby granted, free of charge, to the Licensee obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense
 *  under any license of the Licensee's choice, and/or sell copies of the
 *  Software, subject to the following conditions:
 *
 *  1.	Redistributions of source code must retain the above copyright notice
 *  and the following disclaimer.
 *  2.	Redistributions in binary form must reproduce the above copyright
 *  notice, the following disclaimer in the documentation and/or other
 *  materials provided with the distribution.
 *  3.	All advertising materials mentioning features or use of the Software
 *  must display the following acknowledgement: This product includes all or
 *  parts of the Caml system developed by Inria and its contributors.
 *  4.	Other than specified in clause 3, neither the name of Inria nor the
 *  names of its contributors may be used to endorse or promote products
 *  derived from the Software without specific prior written permission.
 *
 *  Disclaimer
 *
 *  This software is provided by Inria and contributors “as is” and any express
 *  or implied warranties, including, but not limited to, the implied
 *  warranties of merchantability and fitness for a particular purpose are
 *  disclaimed. in no event shall Inria or its contributors be liable for any
 *  direct, indirect, incidental, special, exemplary, or consequential damages
 *  (including, but not limited to, procurement of substitute goods or
 *  services; loss of use, data, or profits; or business interruption) however
 *  caused and on any theory of liability, whether in contract, strict
 *  liability, or tort (including negligence or otherwise) arising in any way
 *  out of the use of this software, even if advised of the possibility of such
 *  damage.
 *
 *)

(* The lexer definition *)

{
open Lexing
open Reason_parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
  | Invalid_literal of string
;;

exception Error of error * Location.t;;

(* The table of keywords *)

let keyword_table =
  let create_hashtable n l =
    let t = Hashtbl.create n in
    List.iter (fun (k, v) -> Hashtbl.add t k v) l;
    t
  in
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "esfun", ES6_FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "switch", SWITCH;
    "module", MODULE;
    "pub", PUB;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "pri", PRI;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* To buffer string literals *)

let string_buffer = Buffer.create 256

let reset_string_buffer () =
  Buffer.reset string_buffer

let store_string_char c =
  Buffer.add_char string_buffer c

let store_string s =
  Buffer.add_string string_buffer s

let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

let get_stored_string () =
  let s = Buffer.contents string_buffer in
  Buffer.reset string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true

(* To "unlex" a few characters *)
let set_lexeme_length buf n = (
  let open Lexing in
  if n < 0 then
    invalid_arg "set_lexeme_length: offset should be positive";
  if n > buf.lex_curr_pos - buf.lex_start_pos then
    invalid_arg "set_lexeme_length: offset larger than lexeme";
  buf.lex_curr_pos <- buf.lex_start_pos + n;
  buf.lex_curr_p <- {buf.lex_start_p
                     with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
)

(* This cut comment characters of the current buffer.
 * Operators (including "/*" and "//") are lexed with the same rule, and this
 * function cuts the lexeme at the beginning of an operator. *)
let lexeme_without_comment buf = (
  let lexeme = Lexing.lexeme buf in
  let i = ref 0 and len = String.length lexeme - 1 in
  let found = ref (-1) in
  while !i < len && !found = -1 do
    begin match lexeme.[!i], lexeme.[!i+1] with
      | ('/', '*') | ('/', '/') | ('*', '/') ->
        found := !i;
      | _ -> ()
    end;
    incr i
  done;
  match !found with
  | -1 -> lexeme
  | n ->
      set_lexeme_length buf n;
      String.sub lexeme 0 n
)

(* Operators that could conflict with comments (those containing /*, */ and //)
 * are escaped in the source. The lexer removes the escapes so that the
 * identifier looks like OCaml ones.
 * An escape in first position is kept to distinguish "verbatim" operators
 * (\=== for instance). *)
let unescape_operator str =
  if (str <> "" && String.contains_from str 1 '\\') then (
    let b = Buffer.create (String.length str) in
    Buffer.add_char b str.[0];
    for i = 1 to String.length str - 1 do
      let c = str.[i] in
      if c <> '\\' then Buffer.add_char b c
    done;
    Buffer.contents b
  ) else str

let lexeme_operator lexbuf =
  unescape_operator (lexeme_without_comment lexbuf)

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                      Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0
                                                       (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let b = Bytes.create l in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else Bytes.sub_string b 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set b dst c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let preprocessor = ref None

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.prerr_warning (Location.curr lexbuf)
    (Warnings.Deprecated "ISO-Latin1 characters in identifiers")
;;

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
              Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable \
                   integers of type %s" ty
  | Invalid_literal s ->
      fprintf ppf "Invalid literal %s" s

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
          None
    )

(* To "unlex" a few characters *)
let set_lexeme_length buf n = (
  let open Lexing in
  if n < 0 then
    invalid_arg "set_lexeme_length: offset should be positive";
  if n > buf.lex_curr_pos - buf.lex_start_pos then
    invalid_arg "set_lexeme_length: offset larger than lexeme";
  buf.lex_curr_pos <- buf.lex_start_pos + n;
  buf.lex_curr_p <- {buf.lex_start_p
                     with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
)

}


let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let uppercase_or_lowercase = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let operator_chars =
  ['!' '$' '%' '&' '+' '-' ':' '<' '=' '>' '?' '@' '^' '|' '~' '#' '.'] |
  ( '\\'? ['/' '*'] )

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*

let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let literal_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | "\\" newline {
      match !preprocessor with
      | None ->
        raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      | Some _ ->
        update_loc lexbuf None 1 false 0;
        token lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        match !preprocessor with
        | None -> token lexbuf
        | Some _ -> EOL
      }
  | blank +
      { token lexbuf }
  | "::" lowercase identchar *
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 2 (String.length delim - 2) in
        COLONCOLONLIDENT delim
      }
  | lowercase identchar * "::"
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 0 (String.length delim - 2) in
        LIDENTCOLONCOLON delim
      }
  | "_"
      { UNDERSCORE }
  | "~"
      { TILDE }
  | "?"
      { QUESTION }
  | "=?"
      { set_lexeme_length lexbuf 1; EQUAL }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s }
  | lowercase_latin1 identchar_latin1 *
      { warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | uppercase_latin1 identchar_latin1 *
      { warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) }
  | int_literal { INT (Lexing.lexeme lexbuf, None) }
  | (int_literal as lit) (literal_modifier as modif)
      { INT (lit, Some modif) }
  | float_literal | hex_float_literal
      { FLOAT (Lexing.lexeme lexbuf, None) }
  | ((float_literal | hex_float_literal) as lit) (literal_modifier as modif)
      { FLOAT (lit, Some modif) }
  | (float_literal | hex_float_literal | int_literal) identchar+
      { raise (Error(Invalid_literal (Lexing.lexeme lexbuf),
                     Location.curr lexbuf)) }
  | "\""
      { reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), None) }
  | "{" lowercase* "|"
      { reset_string_buffer();
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        quoted_string delim lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), Some delim) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }
  | "#" operator_chars+
      { SHARPOP(lexeme_operator lexbuf) }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "=>" { EQUALGREATER }
  | "#"  { SHARP }
  | "."  { DOT }
  | ".." { DOTDOT }
  | "..."{ DOTDOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "<" uppercase_or_lowercase (identchar | '.') * {
    let buf = Lexing.lexeme lexbuf in
    LESSIDENT (String.sub buf 1 (String.length buf - 1))
  }
  | "</" uppercase_or_lowercase (identchar | '.') * ">" {
    let buf = Lexing.lexeme lexbuf in
    LESSSLASHIDENTGREATER (String.sub buf 2 (String.length buf - 2 - 1))
  }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  (* Having a GREATERRBRACKET makes it difficult to parse patterns such
     as > ]. The space in between then becomes significant and must be
     maintained when printing etc. >] isn't even needed!
  | ">]" { GREATERRBRACKET }
  *)
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "[@" { LBRACKETAT }
  | "[%" { LBRACKETPERCENT }
  | "[%%" { LBRACKETPERCENTPERCENT }
  | "!"  { BANG }
  | "!=" { INFIXOP0 "!=" }
  | "!==" { INFIXOP0 "!==" }
  | "\\!=" { INFIXOP0 "!=" }
  | "\\!==" { INFIXOP0 "!==" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "+=" { PLUSEQ }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | "<>" { LESSGREATER }
  | "</>" { LESSSLASHGREATER }
  | "<..>" { LESSDOTDOTGREATER }
  | '\\'? ['~' '?' '!'] operator_chars+
            { PREFIXOP(lexeme_operator lexbuf) }
  | '\\'? ['=' '<' '>' '|' '&' '$'] operator_chars*
            { INFIXOP0(lexeme_operator lexbuf) }
  | '\\'? '@' operator_chars*
            { INFIXOP1(lexeme_operator lexbuf) }
  | '\\'? '^' ('\\' '.')? operator_chars*
            { match lexeme_without_comment lexbuf with
              | "^." -> set_lexeme_length lexbuf 1; POSTFIXOP("^")
              | "^" -> POSTFIXOP("^")
              | op -> INFIXOP1(unescape_operator op) }
  | '\\'? ['+' '-'] operator_chars*
            { INFIXOP2(lexeme_operator lexbuf) }
  (* SLASHGREATER is an INFIXOP3 that is handled specially *)
  | "/>" { SLASHGREATER }
  (* The second star must be escaped so that the precedence assumptions for
   * printing match those of parsing. (Imagine what could happen if the other
   * rule beginning with * picked up */*, and we internally escaped it to **.
   * Whe printing, we have an understanding of the precedence of "**", which
   * enables us to safely print/group it, but that understanding would not
   * match the *actual* precedence that it was parsed at thanks to the *other*
   * rule beginning with *, picking it up instead of the special double ** rule
   * below.
   *)
  | '\\'? '*' '\\'? '*' operator_chars*
            { INFIXOP4(lexeme_operator lexbuf) }
  | '%'     { PERCENT }
  | '\\'? ['/' '*'] operator_chars*
            { match lexeme_operator lexbuf with
              | "" ->
                  (* If the operator is empty, it means the lexeme is beginning
                   * by a comment sequence: we let the comment lexer handle
                   * the case. *)
                  enter_comment lexbuf
              | op -> INFIXOP3 op }
  | '%' operator_chars*
            { INFIXOP3(lexeme_operator lexbuf) }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      }

and enter_comment = parse
  | "/*" ("*" "*"+)?
      { set_lexeme_length lexbuf 2;
        let start_loc = Location.curr lexbuf  in
        comment_start_loc := [start_loc];
        reset_string_buffer ();
        let {Location. loc_end; _} = comment lexbuf in
        let s = get_stored_string () in
        reset_string_buffer ();
        COMMENT (s, { start_loc with Location.loc_end })
      }
  | "/**"
      { let start_p = lexbuf.Lexing.lex_start_p in
        let start_loc = Location.curr lexbuf in
        comment_start_loc := [start_loc];
        reset_string_buffer ();
        let _ = comment lexbuf in
        let s = get_stored_string () in
        reset_string_buffer ();
        lexbuf.Lexing.lex_start_p <- start_p;
        DOCSTRING s
      }
  | "/**/"
      { DOCSTRING "" }
  | "/*/"
      { let loc = Location.curr lexbuf  in
        if !print_warnings then
          Location.prerr_warning loc Warnings.Comment_start;
        comment_start_loc := [loc];
        reset_string_buffer ();
        let {Location. loc_end; _} = comment lexbuf in
        let s = get_stored_string () in
        reset_string_buffer ();
        COMMENT (s, { loc with Location.loc_end })
      }
  | "*/"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        set_lexeme_length lexbuf 1;
        STAR
      }
  | _ { assert false }

and comment = parse
    "/*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf;
      }
  | "*/"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf;
       }
  | "\""
      {
        string_start_loc := Location.curr lexbuf;
        store_string_char '"';
        is_in_string := true;
        begin try string lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '"';
        comment lexbuf }
  | "{" lowercase* "|"
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        string_start_loc := Location.curr lexbuf;
        store_lexeme lexbuf;
        is_in_string := true;
        begin try quoted_string delim lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '|';
        store_string delim;
        store_string_char '}';
        comment lexbuf }

  | "''"
      { store_lexeme lexbuf; comment lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | _
      { store_lexeme lexbuf; comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        if in_comment () then store_lexeme lexbuf;
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { if in_comment () then store_lexeme lexbuf
        else store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { if in_comment () then store_lexeme lexbuf
        else store_string_char(char_for_decimal_code lexbuf 1);
        string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { if in_comment () then store_lexeme lexbuf
        else store_string_char(char_for_hexadecimal_code lexbuf 2);
        string lexbuf }
  | '\\' _
      { if in_comment ()
        then string lexbuf
        else begin
(*  Should be an error, but we are very lax.
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          store_string_char (Lexing.lexeme_char lexbuf 0);
          store_string_char (Lexing.lexeme_char lexbuf 1);
          string lexbuf
        end
      }
  | newline
      { if not (in_comment ()) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      { is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and quoted_string delim = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        quoted_string delim lexbuf
      }
  | eof
      { is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) }
  | "|" lowercase* "}"
      {
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim 1 (String.length edelim - 2) in
        if delim = edelim then ()
        else (store_lexeme lexbuf; quoted_string delim lexbuf)
      }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        quoted_string delim lexbuf }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }

{

  (* Filter commnets *)

  let token_with_comments lexbuf =
    match !preprocessor with
    | None -> token lexbuf
    | Some (_init, preprocess) -> preprocess token lexbuf

  let last_comments = ref []

  let rec token lexbuf =
    match token_with_comments lexbuf with
        COMMENT (s, comment_loc) ->
          last_comments := (s, comment_loc) :: !last_comments;
          token lexbuf
      | tok -> tok

  let add_invalid_docstring text loc =
    let open Location in
    let rec aux = function
      | ((_, loc') as x) :: xs
        when loc'.loc_start.pos_cnum > loc.loc_start.pos_cnum ->
        x :: aux xs
      | xs -> (text, loc) :: xs
    in
    last_comments := aux !last_comments

  let comments () = List.rev !last_comments

  (* Routines for manipulating lexer state *)

  let save_triple lexbuf tok =
    (tok, lexbuf.lex_start_p, lexbuf.lex_curr_p)

  let load_triple lexbuf (tok, p1, p2) = (
    lexbuf.lex_start_p <- p1;
    lexbuf.lex_curr_p <- p2;
    tok
  )

  let fake_triple t (_, pos, _) =
    (t, pos, pos)

  (* insert ES6_FUN *)

  exception Lex_balanced_failed of (token * position * position) list *
                                   (exn * position * position) option

  let closing_of = function
    | LPAREN -> RPAREN
    | LBRACE -> RBRACE
    | _ -> assert false

  let inject_es6_fun = function
    | tok :: acc ->
      tok :: fake_triple ES6_FUN tok :: acc
    | _ -> assert false

  let rec lex_balanced_step closing lexbuf acc tok =
    let acc = save_triple lexbuf tok :: acc in
    match tok, closing with
    | (RPAREN, RPAREN) | (RBRACE, RBRACE) | (RBRACKET, RBRACKET) ->
      acc
    | ((RPAREN | RBRACE | RBRACKET | EOF), _) ->
      raise (Lex_balanced_failed (acc, None))
    | (( LBRACKET | LBRACKETLESS | LBRACKETGREATER
       | LBRACKETAT
       | LBRACKETPERCENT | LBRACKETPERCENTPERCENT ), _) ->
      lex_balanced closing lexbuf (lex_balanced RBRACKET lexbuf acc)
    | ((LPAREN | LBRACE), _) ->
      let rparen =
        try lex_balanced (closing_of tok) lexbuf []
        with (Lex_balanced_failed (rparen, None)) ->
          raise (Lex_balanced_failed (rparen @ acc, None))
      in
      begin match token lexbuf with
      | exception exn ->
        raise (Lex_balanced_failed (rparen @ acc, Some (save_triple lexbuf exn)))
      | EQUALGREATER ->
        let acc = inject_es6_fun acc in
        lex_balanced_step closing lexbuf (rparen @ acc) EQUALGREATER
      | tok' ->
        lex_balanced_step closing lexbuf (rparen @ acc) tok'
      end
    | (LIDENT _, _) ->
      begin match token lexbuf with
      | exception exn ->
        raise (Lex_balanced_failed (acc, Some (save_triple lexbuf exn)))
      | EQUALGREATER ->
        lex_balanced_step closing lexbuf (inject_es6_fun acc) EQUALGREATER
      | tok' -> lex_balanced_step closing lexbuf acc tok'
      end
    | _ -> lex_balanced closing lexbuf acc

  and lex_balanced closing lexbuf acc =
    match token lexbuf with
    | exception exn ->
      raise (Lex_balanced_failed (acc, Some (save_triple lexbuf exn)))
    | tok -> lex_balanced_step closing lexbuf acc tok

  let queued_tokens = ref []
  let queued_exn = ref None

  let lookahead_esfun lexbuf (tok, _, _ as lparen) =
    let triple =
      match lex_balanced (closing_of tok) lexbuf [] with
      | exception (Lex_balanced_failed (tokens, exn)) ->
        queued_tokens := List.rev tokens;
        queued_exn := exn;
        lparen
      | tokens ->
        begin match token lexbuf with
          | exception exn ->
            queued_tokens := List.rev tokens;
            queued_exn := Some (save_triple lexbuf exn);
            lparen
          | token ->
            let tokens = save_triple lexbuf token :: tokens in
            if token == EQUALGREATER then (
              queued_tokens := lparen :: List.rev tokens;
              fake_triple ES6_FUN lparen
            ) else (
              queued_tokens := List.rev tokens;
              lparen
            )
        end
    in
    load_triple lexbuf triple

  let token lexbuf =
    match !queued_tokens, !queued_exn with
    | [], Some exn ->
      queued_exn := None;
      raise (load_triple lexbuf exn)
    | [(LPAREN, _, _) as lparen], None ->
      let _ = load_triple lexbuf lparen in
      lookahead_esfun lexbuf lparen
    | [(LBRACE, _, _) as lparen], None ->
      let _ = load_triple lexbuf lparen in
      lookahead_esfun lexbuf lparen
    | [], None ->
      begin match token lexbuf with
      | LPAREN | LBRACE as tok ->
          lookahead_esfun lexbuf (save_triple lexbuf tok)
      | LIDENT _ as tok ->
          let tok = save_triple lexbuf tok in
          begin match token lexbuf with
          | EQUALGREATER ->
              queued_tokens := [tok; save_triple lexbuf EQUALGREATER];
              load_triple lexbuf (fake_triple ES6_FUN tok)
          | exception exn ->
              queued_exn := Some (save_triple lexbuf exn);
              load_triple lexbuf tok
          | tok' ->
              queued_tokens := [save_triple lexbuf tok'];
              load_triple lexbuf tok
          end
      | token -> token
      end
    | x :: xs, _ -> queued_tokens := xs; load_triple lexbuf x

  let init () =
    is_in_string := false;
    last_comments := [];
    comment_start_loc := [];
    queued_tokens := [];
    queued_exn := None;
    match !preprocessor with
    | None -> ()
    | Some (init, _preprocess) -> init ()

  let set_preprocessor init preprocess =
    preprocessor := Some (init, preprocess)

}
