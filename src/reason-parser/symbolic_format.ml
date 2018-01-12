type symbolic =
  | String of string
  | Spaces of int
  | Newline
  | Tag of string * symbolic list

type buffer = {
  mutable items : symbolic list;
  mutable stack : (string * symbolic list) list;
}

let push_item buffer item =
  buffer.items <- item :: buffer.items

let enter_tag buffer tag =
  buffer.stack <- (tag, buffer.items) :: buffer.stack;
  buffer.items <- []

let leave_tag buffer =
  match buffer.stack with
  | [] -> assert false
  | (tag, items') :: stack' ->
    buffer.items <- Tag (tag, List.rev buffer.items) :: items';
    buffer.stack <- stack'

let push_string buffer str i n =
  if n > 0 then (
    let item =
      try
        for k = i to i + n - 1 do
          if str.[k] <> ' ' then raise Exit
        done;
        Spaces n
      with Exit ->
        if i = 0 && n = String.length str then
          String str
        else
          String (String.sub str i n)
    in
    push_item buffer item
  )

let make_tag_functions buffer =
  { Format.
    mark_open_tag = (fun tag -> enter_tag buffer tag; "");
    mark_close_tag = (fun _tag -> leave_tag buffer; "");
    print_open_tag = ignore;
    print_close_tag = ignore;
  }

let make ?inherit_geometry ~on_flush () =
  let buffer = { items = []; stack = [] } in
  let formatter = Format.make_formatter
      (push_string buffer)
      (fun () ->
         assert (buffer.stack = []);
         let items = buffer.items in
         buffer.items <- [];
         on_flush (List.rev items))
  in
  let out_spaces n = push_item buffer (Spaces n) in
  let out_newline () = push_item buffer Newline in
  let outf = Format.pp_get_formatter_out_functions formatter () in
  let outf = {outf with Format. out_newline; out_spaces} in
  Format.pp_set_formatter_out_functions formatter outf;
  Format.pp_set_formatter_tag_functions formatter (make_tag_functions buffer);
  Format.pp_set_tags formatter true;
  assert (Format.pp_get_mark_tags formatter ());
  begin match inherit_geometry with
    | None -> ()
    | Some ppf ->
      Format.pp_set_margin formatter (Format.pp_get_margin ppf ());
      Format.pp_set_max_boxes formatter (Format.pp_get_max_boxes ppf ());
      Format.pp_set_max_indent formatter (Format.pp_get_max_indent ppf ());
      Format.pp_set_ellipsis_text formatter (Format.pp_get_ellipsis_text ppf ());
  end;
  formatter

let blank_line = String.make 80 ' '

let replay_output ~flush ppf commands =
  let outf = Format.pp_get_formatter_out_functions ppf () in
  let rec replay = function
    | String str -> outf.Format.out_string str 0 (String.length str)
    | Spaces n -> outf.Format.out_spaces n
    | Newline -> outf.Format.out_newline ()
    | Tag (_, contents) -> List.iter replay contents
  in
  List.iter replay commands;
  if flush then Format.pp_print_flush ppf ()
