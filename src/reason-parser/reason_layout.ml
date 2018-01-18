module Comment = Reason_comment

(**
 * These represent "intent to format" the AST, with some parts being annotated
 * with original source location. The benefit of tracking this in an
 * intermediate structure, is that we can then interleave comments throughout
 * the tree before generating the final representation. That prevents the
 * formatting code from having to thread comments everywhere.
 *
 * The final representation is rendered using Easy_format.
 *)
type t =
  | SourceMap      of Location.t * t (* a layout with location info *)
  | WithEOLComment of Comment.t * t (* a layout with comment attached *)
  | Sequence       of listConfig * t list
  | Label          of easyFormatLabelFormatter * t * t
  | Easy           of Easy_format.t

and listConfig = {
  (* Newlines above items that do not have any comments immediately above it.
     Only really useful when used with break:Always/Always_rec *)
  newlinesAboveItems: int;
  (* Newlines above regular comments *)
  newlinesAboveComments: int;
  (* Newlines above doc comments *)
  newlinesAboveDocComments: int;
  (*
   * Whether or not to render the final separator. TODO: Add ability to only
   * render final sep (or first sep in the case of `!sepLeft` when it is next to a
   * line break (either first or last item (depending on `!sepLeft`)).  Also
   * control ability to only render the sep if there is more than one item in
   * list.
   *)
  renderFinalSep: bool;
  break: whenToDoSomething;
  (* Break setting that becomes activated if a comment becomes interleaved into
   * this list. Typically, if not specified, the behavior from [break] will be
   * used.
   *)
  wrap: string * string;
  inline: bool * bool;
  sep: string;
  indent: int;
  sepLeft: bool;
  preSpace: bool;
  (* Really means space_after_separator *)
  postSpace: bool;
  pad: bool * bool;
  (* A function, because the system might rearrange your previous settings, and
   * a function allows you to not be locked into some configuration that is made
   * out of date by the formatting system (suppose it removes the separator
   * token etc.) Having a function allows you to instruct our formatter how to
   * extend the "freshest" notion of the list config when comments are
   * interleaved. *)
  listConfigIfCommentsInterleaved: (listConfig -> listConfig) option;

  (* Formatting to use if an item in a list had an end-of-line comment appended *)
  listConfigIfEolCommentsInterleaved: (listConfig -> listConfig) option;
}

and easyFormatLabelFormatter = Easy_format.t -> Easy_format.t -> Easy_format.t

(* Make a standard list *)
and whenToDoSomething =
  | Never
  | IfNeed
  | Always
  (* Always_rec not only will break, it will break recursively up to the root *)
  | Always_rec

let fprintf = Format.fprintf

let string_of_easy_format = function
  | Easy_format.Atom (s,_) -> s
  | Easy_format.List (_,_) -> "list"
  | Easy_format.Label (_,_) -> "label"
  | Easy_format.Custom _ -> "custom"

let indent_more indent = indent ^ "  "

let dump ppf layout =
  let rec traverse indent = function
    | SourceMap (loc, layout) ->
      fprintf ppf "%s [%d (%d:%d)-%d (%d:%d)]\n" indent
        loc.loc_start.Lexing.pos_cnum
        loc.loc_start.Lexing.pos_lnum
        (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
        loc.loc_end.Lexing.pos_cnum
        loc.loc_end.Lexing.pos_lnum
        (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol);
      traverse (indent^"  ") layout
    | Sequence (config, layout_list) ->
      let break = match config.break with
        | Never  -> "Never"
        | IfNeed  -> "if need"
        | Always  -> "Always"
        | Always_rec  -> "Always_rec" in
      fprintf ppf
        "%s Sequence of %d, sep: %s, finalSep: %s stick_to_left: %s break: %s\n"
        indent
        (List.length layout_list) config.sep (string_of_bool config.renderFinalSep)
        (string_of_bool config.sepLeft) break;
      let indent' = indent_more indent in
      List.iter (traverse indent') layout_list
    | WithEOLComment (comment, layout) ->
      fprintf ppf "%s WithEOLComment: \n" indent;
      fprintf ppf "  %s node \n" indent;
      traverse (indent_more indent) layout;
      fprintf ppf "  %s comments : \n" indent;
      fprintf ppf "  %s %a\n" indent Comment.dump comment;
      fprintf ppf "\n";
    | Label (_, left, right) ->
      let indent' = indent_more indent in
      fprintf ppf "%s Label: \n" indent;
      fprintf ppf "  %s left \n" indent;
      traverse indent' left;
      fprintf ppf "  %s right \n" indent;
      traverse indent' right;
    | Easy e ->
      fprintf ppf "%s Easy: %s \n" indent (string_of_easy_format e)
  in
  traverse "" layout

let dump_easy ppf easy =
  let rec traverse indent = function
    | Easy_format.Atom (s,_) ->
      fprintf ppf "%s Atom:'%s'\n" indent s
    | Easy_format.List ((opening, sep, closing, config), items) ->
      let break = (match config.wrap_body with
          | `No_breaks -> "No_breaks"
          | `Wrap_atoms -> "Wrap_atoms"
          | `Never_wrap -> "Never_wrap"
          | `Force_breaks -> "Force_breaks"
          | `Force_breaks_rec -> "Force_breaks_rec"
          | `Always_wrap -> "Always_wrap") in
      fprintf ppf "%s List: open %s close %s sep %s break %s \n"
        indent opening closing sep break;
      let indent' = indent_more indent in
      List.iter (traverse indent') items
    | Easy_format.Label ((left, config), right) ->
      let break = match config.label_break with
        | `Never -> "Never"
        | `Always_rec -> "Always_rec"
        | `Auto -> "Auto"
        | `Always -> "Always"
      in
      let indent' = indent_more indent in
      fprintf ppf "%s Label (break = %s): \n" indent break;
      fprintf ppf "  %s left \n" indent;
      traverse indent' left;
      fprintf ppf "  %s right \n" indent;
      traverse indent' right
    | Custom _ -> fprintf ppf "custom \n"
  in
  traverse "" easy

let easy_list_param_of_listConfig list_param
    { break; indent; sepLeft; preSpace; postSpace; sep;
      wrap = (opn, cls); inline = (inlineStart, inlineEnd);
      pad = (padOpn, padCls);
    }
  =
  (opn, sep, cls, {
      list_param with
      Easy_format. wrap_body = (
        match break with
        | Never -> `No_breaks
        (* Yes, `Never_wrap is a horrible name - really means "if needed". *)
        | IfNeed -> `Never_wrap
        | Always -> `Force_breaks
        | Always_rec -> `Force_breaks_rec
      );
      indent_body = indent;
      space_after_separator = postSpace;
      space_before_separator = preSpace;
      space_after_opening = padOpn;
      space_before_closing = padCls;
      stick_to_label = inlineStart;
      align_closing = not inlineEnd;
    })

let to_easy_format ?(indent_body=0) layout =
  let base_param = {
    Easy_format.space_after_opening = false;
    space_after_separator = false;
    space_before_separator = false;
    separators_stick_left = true;
    space_before_closing = false;
    stick_to_label = true;
    align_closing = true;
    wrap_body = `No_breaks;
    indent_body;
    list_style = Some "list";
    opening_style = None;
    body_style = None;
    separator_style = None;
    closing_style = None;
  } in
  let rec traverse = function
    | Sequence (listConfig, subLayouts) ->
      Easy_format.List (
        easy_list_param_of_listConfig base_param listConfig ,
        List.map traverse subLayouts
      )
    | Label (labelFormatter, left, right) ->
      labelFormatter (traverse left) (traverse right)
    | SourceMap (_, subLayout) ->
      traverse subLayout
    | WithEOLComment (_, sub) ->
      traverse sub
    | Easy e -> e
  in
  traverse layout
