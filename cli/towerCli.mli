(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

type path = [ `Dir of string | `File of string | `Missing of string ]

val map : ('a -> 'b) -> 'a Cmdliner.Term.t -> 'b Cmdliner.Term.t

val ret_map :
  ('a -> 'b Cmdliner.Term.ret) -> 'a Cmdliner.Term.t -> 'b Cmdliner.Term.t

val map_ret :
  ('a -> 'b) ->
  [< `Error of 'c * 'd | `Ok of 'a ] -> [> `Error of 'c * 'd | `Ok of 'b ]

val to_path :
  string ->
  [> `Error of bool * string
   | `Ok of [> `Dir of string | `File of string | `Missing of string ] ]

val path :
  doc:string ->
  (string option Cmdliner.Arg.converter ->
   'a option -> Cmdliner.Arg.info -> string option Cmdliner.Arg.t) ->
  [> `Dir of string | `File of string | `Missing of string ] Cmdliner.Term.t

val arg_opt :
  'a Cmdliner.Arg.converter -> 'a -> Cmdliner.Arg.info -> 'a Cmdliner.Arg.t

val path_opt :
  doc:string ->
  ?arg:(string option Cmdliner.Arg.converter ->
        string option -> Cmdliner.Arg.info -> string option Cmdliner.Arg.t) ->
  string list ->
  [> `Dir of string | `File of string | `Missing of string ] option
  Cmdliner.Term.t

val output :
  [> `Dir of string | `File of string | `Missing of string ] option
  Cmdliner.Term.t

val version : string

val global_option_section : string

val help_sections : [> `Noblank | `P of string | `S of string ] list
