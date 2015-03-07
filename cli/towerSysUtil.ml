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

open Unix

let (  /  ) = Filename.concat
let ( $?. ) = Filename.check_suffix

let page_size = 4076 (* leave some space for metadata *)

let read_file f file =
  let ic = open_in_bin file in
  let pg = Bytes.create page_size in
  let rec read_more () =
    match input ic pg 0 page_size with
    | 0 -> ()
    | len -> f pg len; read_more ()
  in
  read_more ();
  close_in ic

let copy in_file out_file =
  let oc = open_out_bin out_file in
  read_file (fun buf len -> output oc buf 0 len) in_file;
  close_out oc

let buffer in_file =
  let buf = Buffer.create page_size in
  read_file (fun bytes len -> Buffer.add_subbytes buf bytes 0 len) in_file;
  buf

let rec read_files acc dh =
  match
    try Some (Unix.readdir dh)
    with End_of_file -> None
  with Some file -> read_files (file::acc) dh | None -> acc

let rec all_files base acc dh =
  let files = read_files [] dh in
  List.fold_left (fun acc -> function
  | "." | ".." -> acc
  | dirent ->
    let file = Filename.concat base dirent in
    try
      let dh = Unix.opendir file in
      let acc = all_files file acc dh in
      Unix.closedir dh;
      acc
    with
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> file::acc
    | Unix.Unix_error (Unix.ENOENT,  _, _) -> (* dangling symlink or race *)
      acc
  ) acc files

let in_dir path f =
  let cwd = Unix.getcwd () in
  Unix.chdir path;
  try let r = f () in Unix.chdir cwd; r
  with e -> Unix.chdir cwd; raise e

let foldp_paths f p acc dir =
  let dh = Unix.opendir dir in
  let files = in_dir dir (fun () -> all_files "" [] dh) in
  let () = Unix.closedir dh in
  List.fold_left (fun acc file ->
    if p file dir then f acc file else acc
  ) acc files

module Dir = struct
  module Error = struct
    let nondirectory_segment path =
      `Error (false, "path "^path^" is not a directory")
  end

  let rec make_exist ~perm path =
    try Unix.access path []; None
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let dir = Filename.dirname path in
      begin match make_exist ~perm dir with
      | None ->
        Unix.(mkdir path perm);
        None
      | Some err -> Some err
      end
    | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Some (Error.nondirectory_segment path)

  let make_dirs_exist ~perm =
    List.fold_left (fun err_opt path ->
      match err_opt with None -> make_exist ~perm path | Some err -> Some err
    ) None

  (* Placeholder for now *)
  let log msg = ()

  let gather_nodes root =
    (* By default enforce all recursive processing is done relative to
    the cwd dir, unless overridden *)
    let cwd = getcwd() in
    let rec gather_nodes node =
      let node_path = cwd / node in
      match (lstat node_path).st_kind with
      | S_DIR -> let subnodes = (Sys.readdir node) in
                 let gathered = Array.map (fun subnode -> gather_nodes (node / subnode)) subnodes in
                 (* Careful here so parents always come after descendants *)
                 Array.append (Array.fold_left Array.append [||] gathered) [|node|]
      | _ -> [|node|]
    in
    gather_nodes root

  let rmdir_r path =
    if Sys.file_exists path then
      let remove_node node =
        if Sys.file_exists node then
          match (lstat node).st_kind with
          | S_DIR -> rmdir node
          | S_REG -> unlink node
          | _ -> unlink node
      in
      (* Walk the full tree first, then process results, prevent
      partially-computed results in case of an error walking. *)
      Array.iter remove_node (gather_nodes path)

  let name path = match Filename.dirname path with "." -> "" | p -> p
end
