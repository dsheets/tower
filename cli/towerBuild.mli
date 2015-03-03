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

val default_output_directory : string

val prov : Blueprint.prov

val rope_of_string : string -> ('a, Blueprint.prov) XmlRope.t

val bindings : Blueprint.bindings

val ns_bind_default : string -> string option

val markdown_extensions : Omd_representation.extensions

val process_blueprint : Blueprint.t -> string -> unit

val blueprint_of_file : string -> Blueprint.t

(* TODO: Clarify these interfaces *)
val process_xml : string -> string -> unit

val process_html : string -> string -> unit

val process_md : string -> string -> unit

val processors : (string * (string -> string -> unit)) list

val process :
  (string * (string -> string -> unit)) list ->
  string -> string -> string -> unit Cmdliner.Term.ret

val build : string -> string -> unit Cmdliner.Term.ret

val run : TowerCli.path option -> TowerCli.path option -> unit Cmdliner.Term.ret
