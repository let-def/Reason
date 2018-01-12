type symbolic =
  | String of string
  | Spaces of int
  | Newline
  | Tag of string * symbolic list

val make :
  ?inherit_geometry:Format.formatter ->
  on_flush:(symbolic list -> unit) ->
  unit -> Format.formatter

(* Replay the symbolic output on the output functions of a formatter, by
   passing formatting *)
val replay_output : flush:bool -> Format.formatter -> symbolic list -> unit
