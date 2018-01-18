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

let string_of_easy_format = function
  | Easy_format.Atom (s,_) -> s
  | Easy_format.List (_,_) -> "list"
  | Easy_format.Label (_,_) -> "label"
  | Easy_format.Custom _ -> "custom"

let indent_more indent = indent ^ "  "

let dump ppf layout =
  let rec traverse indent = function
    | SourceMap (loc, layout) ->
      Format.fprintf ppf "%s [%d (%d:%d)-%d (%d:%d)]\n" indent
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
      Format.fprintf ppf
        "%s Sequence of %d, sep: %s, finalSep: %s stick_to_left: %s break: %s\n"
        indent
        (List.length layout_list) config.sep (string_of_bool config.renderFinalSep)
        (string_of_bool config.sepLeft) break;
      let indent' = indent_more indent in
      List.iter (traverse indent') layout_list
    | WithEOLComment (comment, layout) ->
      Format.fprintf ppf "%s WithEOLComment: \n" indent;
      Format.fprintf ppf "  %s node \n" indent;
      traverse (indent_more indent) layout;
      Format.fprintf ppf "  %s comments : \n" indent;
      Format.fprintf ppf "  %s %a\n" indent Comment.dump comment;
      Format.fprintf ppf "\n";
    | Label (_, left, right) ->
      let indent' = indent_more indent in
      Format.fprintf ppf "%s Label: \n" indent;
      Format.fprintf ppf "  %s left \n" indent;
      traverse indent' left;
      Format.fprintf ppf "  %s right \n" indent;
      traverse indent' right;
    | Easy e ->
      Format.fprintf ppf "%s Easy: %s \n" indent (string_of_easy_format e)
  in
  traverse "" layout

let dump_easy ppf easy =
  let rec traverse indent = function
    | Easy_format.Atom (s,_) ->
      Format.fprintf ppf "%s Atom:'%s'\n" indent s
    | Easy_format.List ((opening, sep, closing, config), items) ->
      let break = (match config.wrap_body with
          | `No_breaks -> "No_breaks"
          | `Wrap_atoms -> "Wrap_atoms"
          | `Never_wrap -> "Never_wrap"
          | `Force_breaks -> "Force_breaks"
          | `Force_breaks_rec -> "Force_breaks_rec"
          | `Always_wrap -> "Always_wrap") in
      Format.fprintf ppf "%s List: open %s close %s sep %s break %s \n"
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
      Format.fprintf ppf "%s Label (break = %s): \n" indent break;
      Format.fprintf ppf "  %s left \n" indent;
      traverse indent' left;
      Format.fprintf ppf "  %s right \n" indent;
      traverse indent' right
    | Custom _ -> Format.fprintf ppf "custom \n"
  in
  traverse "" easy

let config_to_easy_format list_param
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
        config_to_easy_format base_param listConfig ,
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

(** [has_comment layout] checks if a layout has comment attached to it *)
let rec has_comment = function
  | WithEOLComment (_, _) -> true
  | SourceMap (_, sub) -> has_comment sub
  | _ -> false


(** [get_location] recursively takes the unioned location of its children,
    and returns the max one *)
let get_location layout =
  let union loc1 loc2 =
    match (loc1, loc2) with
    | None, _ -> loc2
    | _, None -> loc1
    | Some loc1, Some loc2 ->
      Some {loc1 with Location.loc_end = loc2.Location.loc_end}
  in
  let rec loop = function
    | Sequence (listConfig, subLayouts) ->
      let locs = List.map loop subLayouts in
      List.fold_left union None locs
    | Label (formatter, left, right) ->
      let leftLoc = loop left in
      let rightLoc = loop right in
      union leftLoc rightLoc
    | SourceMap (loc, _) ->
      Some loc
    | WithEOLComment (_, sub) ->
      loop sub
    | _ -> None
  in
  loop layout

let is_before ~location layout =
  match get_location layout with
  | None -> true
  | Some loc -> Syntax_util.location_before loc location


(** Returns true if the layout's location contains loc *)
let contains_location loc layout =
  match get_location layout with
  | None -> false
  | Some subLoc -> Syntax_util.location_contains subLoc loc

module Syntax =
struct

  let source_map ?(loc=Location.none) layout =
    if loc <> Location.none
    then SourceMap (loc, layout)
    else layout

  let atom ?loc str =
    let labelStringStyle = { Easy_format.atom_style = Some "atomClss" } in
    let layout = Easy (Easy_format.Atom(str, labelStringStyle)) in
    source_map ?loc layout

  let makeList
      ?(newlinesAboveItems=0)
      ?(newlinesAboveComments=0)
      ?(newlinesAboveDocComments=0)
      ?listConfigIfCommentsInterleaved
      ?listConfigIfEolCommentsInterleaved
      ?(renderFinalSep=false)
      ?(break=Never)
      ?(wrap=("", ""))
      ?(inline=(true, false))
      ?(sep="")
      ?(indent=2)
      ?(sepLeft=true)
      ?(preSpace=false)
      ?(postSpace=false)
      ?(pad=(false,false))
      lst
    =
    let config = {
      newlinesAboveItems; newlinesAboveComments; newlinesAboveDocComments;
      listConfigIfCommentsInterleaved; listConfigIfEolCommentsInterleaved;
      renderFinalSep; break; wrap; inline; sep; indent; sepLeft;
      preSpace; postSpace; pad;
    } in
    Sequence (config, lst)

end

open Syntax

(* WIP: Format comment *)

(* Don't use `trim` since it kills line return too? *)
let rec beginsWithStar_ line length idx =
  if idx = length then false
  else
    let ch = String.get line idx in
    if ch = '*' then true
    else if ch = '\t' || ch = ' ' then beginsWithStar_ line length (idx + 1)
    else false

let beginsWithStar line = beginsWithStar_ line (String.length line) 0

let rec numLeadingSpace_ line length idx accum =
  if idx = length then accum
  else
    let ch = String.get line idx in
    if ch = '\t' || ch = ' ' then numLeadingSpace_ line length (idx + 1) (accum + 1)
    else accum

let numLeadingSpace line = numLeadingSpace_ line (String.length line) 0 0

(* Computes the smallest leading spaces for non-empty lines *)
let smallestLeadingSpaces strs =
  let rec smallestLeadingSpaces curMin strs = match strs with
    | [] -> curMin
    | hd::tl ->
      if hd = "" then
        smallestLeadingSpaces curMin tl
      else
        let leadingSpace = numLeadingSpace hd in
        let nextMin = min curMin leadingSpace in
        smallestLeadingSpaces nextMin tl
  in
  smallestLeadingSpaces 99999 strs

(* This is a special-purpose functions only used by `formatComment_`. Notice we
   skip a char below during usage because we know the comment starts with `/*` *)
let rec lineZeroMeaningfulContent_ line length idx accum =
  if idx = length then None
  else
    let ch = String.get line idx in
    if ch = '\t' || ch = ' ' || ch = '*' then
      lineZeroMeaningfulContent_ line length (idx + 1) (accum + 1)
    else Some accum

let lineZeroMeaningfulContent line = lineZeroMeaningfulContent_ line (String.length line) 1 0

let string_after s n = String.sub s n (String.length s - n)

let formatComment_ txt =
  let commLines =
    Syntax_util.split_by ~keep_empty:true (fun x -> x = '\n') (Comment.wrap txt)
  in
  match commLines with
  | [] -> atom ""
  | [hd] ->
    makeList ~inline:(true, true) ~postSpace:true ~preSpace:true ~indent:0 ~break:IfNeed [atom hd]
  | zero::one::tl ->
    let attemptRemoveCount = (smallestLeadingSpaces (one::tl)) in
    let leftPad =
      if beginsWithStar one then 1
      else match lineZeroMeaningfulContent zero with
        | None -> 1
        | Some num -> num + 1
    in
    let padNonOpeningLine s =
      let numLeadingSpaceForThisLine = numLeadingSpace s in
      if String.length s == 0 then ""
      else (String.make leftPad ' ') ^
           (string_after s (min attemptRemoveCount numLeadingSpaceForThisLine)) in
    let lines = zero :: List.map padNonOpeningLine (one::tl) in
    makeList ~inline:(true, true) ~indent:0 ~break:Always_rec (List.map atom lines)

let formatComment comment =
  let layout = formatComment_ comment in
  let loc = comment.location in
  if loc = Location.none then
    layout
  else
    SourceMap (loc, layout)

