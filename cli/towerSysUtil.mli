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

val ( / ) : string -> string -> string

val ( $?. ) : string -> string -> bool

val page_size : int

val read_file : (bytes -> int -> 'a) -> string -> unit

val copy : string -> string -> unit

val buffer : string -> Buffer.t

val foldp_paths :
  ('a -> string -> 'a) -> (string -> string -> bool) -> 'a -> string -> 'a

module Dir : sig
  module Error : sig
    val nondirectory_segment : string -> [> `Error of bool * string ]
  end

  val make_exist :
    perm:Unix.file_perm -> string -> [> `Error of bool * string ] option

  val make_dirs_exist :
    perm:Unix.file_perm ->
    string list -> [> `Error of bool * string ] option

  val name : string -> string
end
