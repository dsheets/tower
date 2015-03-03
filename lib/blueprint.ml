(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* This is an extremely simple templating system based on XML ropes
   with named holes. *)

(* TODO: What about lexical info? values are assumed to be static per source *)
type prov =
| Stream of string
| File of string

let xmlns = "https://opam.ocaml.org/packages/tower/xmlns/template"

module Hole = struct
  type 'value t =
  | Named of string
  | Valued of string * 'value

  let compare a b = match a, b with
    | Named n, Named n'
    | Named n, Valued (n',_)
    | Valued (n,_), Named n'
    | Valued (n,_), Valued (n',_) ->
      String.compare n n'

  let equal a b = compare a b = 0

  let hash = Hashtbl.hash
end

type t = (hole, prov) XmlRope.t
and hole = valued Hole.t
and valued =
| Default of t
| Typed of string * t

(* TODO: Maybe these should just be string/name based. *)
module HoleMap = Map.Make(struct
  type t = hole
  let compare = Hole.compare
end)

module HoleTable = Hashtbl.Make(struct
  type t = hole
  let equal = Hole.equal
  let hash = Hole.hash
end)

(* TODO: This is a bit weird... *)
type bindings =
| Generator of (hole -> t)
| Map of t HoleMap.t * bindings option
| Table of t HoleTable.t * bindings option

let consumep = function 0::_ -> true | [] | _::_ -> false
let incr_stack = function [] -> [] | h::t -> (h+1)::t
let decr_stack = function [] -> [] | h::t -> (h-1)::t
let push_stack stack v = v::stack
let pop_stack = function [] -> [] | _::t -> t

let of_stream ~prov ~source =
  let rec run stack seq rope = function
    | acc, None -> acc, XmlRope.(rope ++ (of_list ~prov (List.rev seq)))
    | acc, Some el -> match el with
      | `El_start ((ns,el),attrs) when ns=xmlns ->
        handle stack seq rope acc attrs el
      | `El_start el ->
        run (incr_stack stack) ((`El_start el)::seq) rope (source acc)
      | `El_end when consumep stack ->
        run (pop_stack stack) seq rope (source acc)
      | `El_end ->
        run (decr_stack stack) (`El_end::seq) rope (source acc)
      | (`Dtd _ | `Data _) as signal ->
        run stack (signal::seq) rope (source acc)
  and handle stack seq rope acc attrs = XmlRope.(function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      (* TODO: this can raise, do something sensible *)
      let hole = hole ~prov (Hole.Named (List.assoc ("","name") attrs)) in
      let rope = rope ++ literal ++ hole in
      (* TODO: t:insert only inserts a hole on open but the contents
         become later sibling. A Hole.Valued should be created in the
         non-empty t:insert case. *)
      run (push_stack stack 0) [] rope (source acc)
    | "seq" -> run (push_stack stack 0) seq rope (source acc)
    | el ->
      (* TODO: do something better *)
      raise (Invalid_argument ("found t:"^el^" but that's an unknown tag"))
  ) in
  fun acc -> run [] [] XmlRope.empty (source acc)

let xml_source xml_input =
  xml_input, if Xmlm.eoi xml_input then None else Some (Xmlm.input xml_input)

let of_file ?(ns=fun _ -> None) path =
  let prov = File path in
  let ic = open_in path in
  try
    let xml_input = Xmlm.make_input ~ns (`Channel ic) in
    let source = xml_source in
    let _, rope = of_stream ~prov ~source xml_input in
    close_in ic;
    rope
  with e -> (* TODO: Actually handle exceptions *)
    close_in ic;
    raise e

let default_hole = Hole.(function
  | Valued (_, Default rope)    -> rope
  | Valued (_, Typed (_, rope)) -> rope
  | Named name ->
    (* TODO: Make this not horrible *)
    raise (Failure ("No value for hole named "^name))
)

let rec bind_hole bindings hole = match bindings with
  | Generator g -> g hole
  | Map (map, more) -> begin
    try HoleMap.find hole map
    with Not_found -> match more with
    | Some bindings -> bind_hole bindings hole
    | None -> default_hole hole
  end
  | Table (tbl, more) -> begin
    try HoleTable.find tbl hole
    with Not_found -> match more with
    | Some bindings -> bind_hole bindings hole
    | None -> default_hole hole
  end

let bind ~sink acc bindings rope =
  let bind_hole = bind_hole bindings in
  let patch prov acc hole = acc, bind_hole hole in
  XmlRope.to_stream ~patch ~sink acc rope
