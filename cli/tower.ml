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

open Cmdliner

type path = [
| `File of string
| `Dir of string
| `Missing of string
]

let map f = Term.(app (pure f))
let ret_map f t = Term.ret (map f t)
let map_ret f = function
  | `Ok v -> `Ok (f v)
  | `Error (help,msg) as err -> err

let rec to_path path = Unix.(
  try match (stat path).st_kind with
  | S_DIR -> `Ok (`Dir path)
  | S_REG -> `Ok (`File path)
  | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK ->
    `Error (false, "unsupported file type")
  with
  | Unix_error (ENOENT,_,_) -> `Ok (`Missing path)
  | Unix_error (e,_,_) -> `Error (false, path^": "^(error_message e))
)

let path ~doc arg =
  Arg.(ret_map to_path (required (
    let docv = "PATH" in
    arg (some string) None & info ~docv ~doc []
  )))

let arg_opt c v i = Arg.opt c v i

let path_opt ~doc ?(arg=arg_opt) names =
  Arg.(ret_map (function
  | None -> `Ok None
  | Some x -> map_ret (fun x -> Some x) (to_path x)
  ) (value (
    let docv = "PATH" in
    arg (some string) None & info ~docv ~doc names
  )))

let output = path_opt ~doc:"the output path" ["o"]

let version = TowerVersion.(git_rev ^ (if git_dirty then " (dirty)" else ""))

let global_option_section = "COMMON OPTIONS"

let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "David Sheets <sheets@alum.mit.edu>";
  `S "BUGS";
  `P "Browse and report new issues at"; `Noblank;
  `P "<https://github.com/dsheets/tower/issues>.";
]

let build_cmd =
  let doc = "generate a static site" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,tower build) generates a site from the current working directory.";
  ] @ help_sections
  in
  let path' = path_opt
    ~doc:"the configuration file or directory to use as source"
    ~arg:(Arg.pos 0) []
  in
  Term.(ret (pure TowerBuild.run $ path' $ output),
        info "build" ~doc ~sdocs:global_option_section ~man)

let default_cmd =
  let exec_name = Filename.basename Sys.argv.(0) in
  let doc = "manipulate a static web site" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") generates static web sites and provides tools \
         for their maintenance.");
  ] @ help_sections
  in
  let no_cmd_err = `Error (true, "No command specified. Try 'build'.") in
  Term.(ret (pure no_cmd_err),
        info exec_name ~version ~sdocs:global_option_section
          ~doc ~man)

;;

match Term.eval_choice default_cmd [
  build_cmd;
] with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
