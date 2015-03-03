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
open TowerCli

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
