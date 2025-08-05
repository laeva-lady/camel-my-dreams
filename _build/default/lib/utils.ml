let readcsv path_to_csv : Data.info list =
  path_to_csv |> Csv.load |> List.map Data.info_of_list
;;

let getPath () =
  let homeDir = Sys.getenv "HOME" in
  let cacheDir = ".cache/camel-my-dreams/daily/" in
  homeDir ^ "/" ^ cacheDir
;;

let create_file_if_not_exists filename =
  let flags = [ Open_creat; Open_excl; Open_wronly ] in
  let perm = 0o644 in
  try
    let oc = open_out_gen flags perm filename in
    close_out oc;
    ()
  with
  | Sys_error _ -> ()
;;

let writecsv (pinfos : Data.info list) =
  let now = Unix.gettimeofday () in
  let times = Unix.localtime now in
  let date =
    Printf.sprintf
      "%02d-%02d-%02d"
      (times.tm_year + 1900)
      (times.tm_mon + 1)
      times.tm_mday
  in
  let file = getPath () ^ date ^ ".csv" in
  create_file_if_not_exists file;
  let out_channel = open_out file in
  pinfos
  |> Data.csv_string_of_infos
  |> List.iter (fun row -> Printf.fprintf out_channel "%s\n" row);
  close_out out_channel;
  ()
;;

type flags =
  { help : bool
  ; server : bool
  ; watch : bool
  ; monthly : bool
  }

let new_flag h s w m = { help = h; server = s; watch = w; monthly = m }

let handle_flags args =
  let helpq = Array.exists (fun x -> x = "help") args in
  if not helpq
  then (
    let startserver = ref false in
    let startwatch = ref false in
    let isMonthly = ref false in
    args
    |> Array.iter (fun arg ->
      match arg with
      | "server" -> startserver := true
      | "watch" -> startwatch := true
      | "month" -> isMonthly := true
      | _ -> ());
    new_flag false !startserver !startwatch !isMonthly)
  else new_flag true false false false
;;
