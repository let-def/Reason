(* There are three main categories of error:
   - _lexer errors_, thrown by Reason_lexer when the source **text is malformed**
     and no token can be produced
   - _concrete parsing errors_, thrown by the menhir parser / parsing loop
     when a **token is unexpected**
   - _abstract parsing errors_, thrown by hand-written semantic actions or
     further AST checks, when the source text was incorrect but this restriction
     was too fine to be captured by the grammar rules

   A fourth case is when unknown / unexpected error occurs.
*)

open Format

type lexing_error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
  | Invalid_literal of string

type ast_error =
  | Not_expecting of Location.t * string
  | Other_syntax_error of string
  | Variable_in_scope of Location.t * string
  | Applicative_path of Location.t

type parsing_error = string

type reason_error =
  | Lexing_error of lexing_error
  | Parsing_error of parsing_error
  | Ast_error of ast_error

exception Reason_error of reason_error * Location.t

let catch_errors
  : (reason_error * Location.t) list ref option ref
  = ref None

let raise_error error loc =
  match !catch_errors with
  | None -> raise (Reason_error (error, loc))
  | Some caught -> caught := (error, loc) :: !caught

let raise_fatal_error error loc =
  raise (Reason_error (error, loc))

let recover_non_fatal_errors f =
  let catch_errors0 = !catch_errors in
  let errors = ref [] in
  catch_errors := Some errors;
  let result =
    match f () with
    | x -> Ok x
    | exception exn -> Error exn
  in
  catch_errors := catch_errors0;
  (result, List.rev !errors)

(* Report lexing errors *)

let format_lexing_error ppf = function
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

let format_parsing_error ppf msg =
  fprintf ppf "%s" msg

let format_ast_error ppf = function
  | Not_expecting (loc, nonterm) ->
    fprintf ppf
      "Syntax error: %a%s not expected."
      Location.print_error loc nonterm
  | Applicative_path loc ->
    fprintf ppf
      "Syntax error: %aapplicative paths of the form F(X).t \
       are not supported when the option -no-app-func is set."
      Location.print_error loc
  | Variable_in_scope (loc, var) ->
    fprintf ppf "%aIn this scoped type, variable '%s \
                 is reserved for the local type %s."
      Location.print_error loc var var
  | Other_syntax_error msg ->
    fprintf ppf "%s" msg

let report_error ppf ~loc err =
  let mk_error f x = Location.error_of_printer loc f x in
  let error = match err with
    | Lexing_error err -> mk_error format_lexing_error err
    | Parsing_error err -> mk_error format_parsing_error err
    | Ast_error err -> mk_error format_ast_error err
  in
  Format.fprintf ppf "@[%a@]@." Location.report_error error
