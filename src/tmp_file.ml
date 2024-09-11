open Lwt.Infix

(* COURTESY OF Current.Process (OCurrent)*)

let win32_unlink fn =
  Lwt.catch
    (fun () -> Lwt_unix.unlink fn)
    (function
      | Unix.Unix_error (Unix.EACCES, _, _) as exn ->
          (* Try removing the read-only attribute before retrying unlink. We catch
             any exception here and ignore it in favour of the original [exn]. *)
          Lwt.catch
            (fun () ->
              Lwt_unix.lstat fn >>= fun { st_perm; _ } ->
              Lwt_unix.chmod fn 0o666 >>= fun () ->
              Lwt.catch
                (fun () -> Lwt_unix.unlink fn)
                (function
                  | _ ->
                      (* If everything succeeded but the final removal still failed,
                         restore original permissions *)
                      Lwt_unix.chmod fn st_perm >>= fun () -> Lwt.fail exn))
            (fun _ -> Lwt.fail exn)
      | exn -> Lwt.fail exn)

let unlink = if Sys.win32 then win32_unlink else Lwt_unix.unlink

let make_tmp_dir ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> failwith "Failed to generate temporary directory name!"
    | n -> (
        let tmppath =
          Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff)
        in
        try
          Unix.mkdir tmppath mode;
          tmppath
        with Unix.Unix_error (Unix.EEXIST, _, _) -> mktmp (n - 1))
  in
  mktmp 10

let rm_f_tree root =
  let rec rmtree path =
    Lwt_unix.lstat path >>= fun info ->
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
        unlink path
    | Unix.S_DIR ->
        Lwt_unix.chmod path 0o700 >>= fun () ->
        Lwt_unix.files_of_directory path
        |> Lwt_stream.iter_s (function
             | "." | ".." -> Lwt.return_unit
             | leaf -> rmtree (Filename.concat path leaf))
        >>= fun () -> Lwt_unix.rmdir path
  in
  rmtree root

let with_tmpfile ?prefix ~content fn =
  let tmpdir =
    make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ())
  in
  let filepath = Fpath.(v tmpdir / "tmp.xlsx") in
  Lwt.finalize
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string filepath) (fun oc ->
          Lwt_io.write oc content)
      >>= fun () -> fn filepath)
    (fun () -> rm_f_tree tmpdir)
