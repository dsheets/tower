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

let insert_element name =
  let name = String.concat "." name in
  Omd_representation.X (object (self)
    method name = "OmdPlumbing.insert_element["^name^"]"
    method to_html ?indent _render _v = Some ("<t:insert name=\""^name^"\"/>")
    method to_sexpr _render _v = Some self#name
    method to_t _v = None
  end)

let rec read_dotted_name acc = Omd_representation.(function
  | (Word label)::Dot::rest -> read_dotted_name (label::acc) rest
  | (Word label)     ::rest -> List.rev (label::acc), rest
  |                    rest -> List.rev acc, rest
)

let extension = Omd_representation.(object
  method parser_extension prev_els buf_toks next_toks =
    match next_toks with
    | (Obraces 0)::rest_toks ->
      let name, rest_toks = read_dotted_name [] rest_toks in
      (match rest_toks with
      | (Cbraces 0)::rest_toks ->
        Some ((insert_element name)::prev_els, buf_toks, rest_toks)
      | _ -> None
      )
    | _ -> None
  method to_string = "OmdPlumbing.extension"
end)
