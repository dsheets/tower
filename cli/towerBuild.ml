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

(* TODO: would be nice to be parameterized over Jekyll compat *)
let default_output_directory = "_site"

let prov = Blueprint.Stream "TowerBuild"
let rope_of_string s = XmlRope.of_list ~prov [`Data s]

(* TODO: fixme *)
let bindings = Blueprint.(Generator Hole.(function
  | Named name -> rope_of_string ("[[ "^name^" ]]")
  | Valued (_,Default v) | Valued (_,Typed (_,v)) -> v
))

let ns_bind_default = function "t" -> Some Blueprint.xmlns | _ -> None

let markdown_extensions = [ OmdPlumbing.extension ]

let process_blueprint blueprint out_file =
  let oc = open_out out_file in
  let xml_out = Xmlm.make_output ~decl:false ~nl:true (`Channel oc) in
  let sink _prov out s = List.iter (Xmlm.output out) s; out in
  (* we get our xml_out back, ignore it *)
  ignore (Blueprint.bind ~sink xml_out bindings blueprint);
  close_out oc

let blueprint_of_file file =
  let ns = ns_bind_default in
  let open Printf in
  try Blueprint.of_file ~ns file
  with Xmlm.Error ((l,c), err) -> (* TODO: handle this better. Rollback? *)
    eprintf "XML error: %s:%d:%d %s\n%!" file l c (Xmlm.error_message err);
    exit 1

let process_xml in_file = process_blueprint (blueprint_of_file in_file)

let process_html = process_xml

(* TODO: this could be majorly improved by not copying 2 times and not
   serializing HTML and then parsing again... *)
let process_md in_file out_file =
  let md = Buffer.contents (TowerSysUtil.buffer in_file) in
  let extensions = markdown_extensions in
  let omd = Omd.of_string ~extensions md in
  let html = Omd.to_html omd in
  let prov = Blueprint.File in_file in
  let ns = ns_bind_default in
  let xml_in = Xmlm.make_input ~ns (`String (0,html)) in
  let source = Blueprint.xml_source in
  let _, rope = Blueprint.of_stream ~prov ~source xml_in in
  let out_file =
    if Filename.check_suffix out_file ".md"
    then (Filename.chop_suffix out_file ".md")^".html"
    else out_file
  in
  process_blueprint rope out_file

(* TODO: a better map type would be nice *)
let processors = [
  ".html", process_html;
  ".xml",  process_xml;
  ".md",   process_md;
]

let (/) = Filename.concat

let process processors in_dir out_dir file =
  let (_,process) = List.find
    (fun (ext,_) -> Filename.check_suffix file ext) processors
  in
  let out_file = out_dir / file in
  (* Here, we rely on umask to attenuate the permissions. *)
  TowerSysUtil.Dir.make_exist ~perm:0o777 (Filename.dirname out_file);
  process (in_dir / file) out_file

(* TODO: The target should be wiped (Jekyll) or checked before processing. *)
let build in_dir out_dir =
  TowerSysUtil.foldp_paths
    (fun () -> process processors in_dir out_dir)
    (fun file _dir ->
      List.exists (fun (ext,_) -> Filename.check_suffix file ext) processors
    )
    ()
    in_dir;
  `Ok ()

let run path output =
  try
    let in_dir = match path with
      | None                 -> Unix.getcwd ()
      | Some (`Dir in_dir)   -> in_dir
      | Some (`File in_file) ->
        raise (Invalid_argument (in_file^" file source not yet supported"))
      | Some (`Missing path) ->
        raise (Invalid_argument ("missing source "^path))
    in
    let out_dir = match output with
      | None                  -> default_output_directory
      | Some (`Dir out_dir)   -> out_dir
      | Some (`File out_file) ->
        raise (Invalid_argument (out_file^" file output not yet supported"))
      | Some (`Missing path)  -> path
    in
    build in_dir out_dir
  with Invalid_argument why ->
    `Error (false, why)
