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

type prov = Stream of string | File of string

val xmlns : string

module Hole : sig
  type 'value t = Named of string | Valued of string * 'value
  val compare : 'a t -> 'b t -> int
  val equal : 'a t -> 'b t -> bool
  val hash : 'a -> int
end

type t = (hole, prov) XmlRope.t
and hole = valued Hole.t
and valued = Default of t | Typed of string * t

module HoleMap : Map.S

module HoleTable : Hashtbl.S

(* TODO: shouldn't expose? *)
type bindings =
| Generator of (hole -> t)
| Map of t HoleMap.t * bindings option
| Table of t HoleTable.t * bindings option

val of_stream :
  prov:prov -> source:('acc -> 'acc * Xmlm.signal option) -> 'acc -> 'acc * t

val xml_source : Xmlm.input -> Xmlm.input * Xmlm.signal option

val of_file :
  ?ns:(string -> string option) -> string -> ('a Hole.t, prov) XmlRope.t

val bind_hole : bindings -> hole -> t

val bind :
  sink:(prov -> 'acc -> Xmlm.signal list -> 'acc) ->
  'acc -> bindings -> t -> 'acc
