let getSocketPath () =
  Sys.getenv "XDG_RUNTIME_DIR"
  ^ "/hypr/"
  ^ Sys.getenv "HYPRLAND_INSTANCE_SIGNATURE"
  ^ "/.socket2.sock"
;;

let active_window = ref (Clients.get_active_client ())

let rec socketloop_activewindow sock =
  (* read from the buffer *)
  let buffer =
    try Bytes.create 1024 with
    | Invalid_argument e -> failwith ("Invalid argument or something: " ^ e)
  in
  let bytes_read = Unix.read sock buffer 0 1024 in
  (* Return the data as a string *)
  let msg = Bytes.sub_string buffer 0 bytes_read in
  let lines = String.split_on_char '\n' msg in
  List.iter
    (fun line ->
       if String.starts_with ~prefix:"activewindow>>" line
       then (
         match
           String.split_on_char ',' (String.sub line 14 (String.length line - 14))
         with
         | name :: _ -> if !active_window <> name then active_window := name
         | _ -> ()))
    lines;
  socketloop_activewindow sock
;;

let cacheDir = Utils.getPath ()

let rec update_data_loop (procs : Data.info list) =
  let current_clients : string list = Clients.get_clients () in
  let current_client = !active_window in
  current_clients
  |> List.map (fun current_window ->
    try
      let matching_window =
        List.find (fun (proc : Data.info) -> proc#get_name == current_window) procs
      in
      matching_window#update_time (Data.new_time 0 0 1) (Data.new_time 0 0 1);
      matching_window#set_is_active (current_window == current_client);
      matching_window
    with
    | Not_found ->
      new Data.info
        current_window
        (Data.new_time 0 0 1)
        (Data.new_time 0 0 1)
        (current_window == current_client))
  |> Utils.writecsv;
  Unix.sleep 1;
  update_data_loop procs
;;

let rec wait_forever () =
  Thread.delay 60.0;
  wait_forever ()
;;

let start_socket_server () =
  (* create a local socket *)
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* binds the local socket to the socketPath and connect to it*)
  Unix.connect sock (Unix.ADDR_UNIX (getSocketPath ()));
  (*
     |
     |
     |
     |
  *)
  let initial_processes = Utils.getPath () |> Utils.readcsv in
  let _activewindow_thread = Thread.create (fun () -> socketloop_activewindow sock) () in
  let _update_stuff_thread =
    Thread.create (fun () -> update_data_loop initial_processes) ()
  in
  wait_forever ()
;;
