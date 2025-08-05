type time =
  { hour : int
  ; minute : int
  ; second : int
  }

let new_time h m s = { hour = h; minute = m; second = s }
let string_of_time t = Printf.sprintf "%02d:%02d:%02d" t.hour t.minute t.second

let time_of_list xs =
  xs
  |> List.map int_of_string
  |> function
  | [ a; b; c ] -> { hour = a; minute = b; second = c }
  | _ -> failwith "List must have exactly 3 elements"
;;

let time_of_string s =
  let _ = s in
  new_time 0 0 0
;;

let compare_time (t1 : time) (t2 : time) : int =
  match compare t1.hour t2.hour with
  | 0 ->
    (match compare t1.minute t2.minute with
     | 0 -> compare t1.second t2.second
     | c -> c)
  | c -> c
;;

let add_time t1 t2 =
  let total_second = t1.second + t2.second in
  let carry_minute, second = total_second / 60, total_second mod 60 in
  let total_minute = t1.minute + t2.minute + carry_minute in
  let carry_hours, minute = total_minute / 60, total_minute mod 60 in
  let hour = t1.hour + t2.hour + carry_hours in
  { hour; minute; second }
;;

let blue = "\027[34m"
let active_color = "\027[0;95m"
let green = "\027[32m"
let red = "\027[31m"
let reset = "\027[0m"
let yellow = "\027[33m"
let width = 90
let text = "Camel my dreams"
let pad_total = width - String.length text - 2 (* for the pipes *)
let pad_left = pad_total / 2
let pad_right = pad_total - pad_left

let sum_active_time infos =
  infos
  |> List.fold_left (fun acc pinfo -> add_time acc pinfo#get_active_time) (new_time 0 0 0)
;;

let sum_usage_time infos =
  infos
  |> List.fold_left (fun acc pinfo -> add_time acc pinfo#get_usage_time) (new_time 0 0 0)
;;

class info name_input ut_input at_input is_active =
  object
    val name : string = name_input
    val mutable usage_time : time = ut_input
    val mutable active_time : time = at_input
    val mutable is_active : bool = is_active

    method update_time new_ut new_at =
      usage_time <- new_ut;
      active_time <- new_at

    method get_strings = [ name; string_of_time usage_time; string_of_time active_time ]
    method get_name = name
    method get_usage_time_string = string_of_time usage_time
    method get_active_time_string = string_of_time active_time
    method get_is_active_string = string_of_bool is_active
    method get_usage_time = usage_time
    method get_active_time = active_time
    method get_is_active = is_active
    method set_is_active new_value = is_active <- new_value
  end

let info_of_list xs =
  match xs with
  | [ a; b; c; d ] -> new info a (time_of_string b) (time_of_string c) (bool_of_string d)
  | _ -> failwith "List must have exactly 4 elements"
;;

let csv_string_of_infos (infos : info list) =
  infos
  |> List.map (fun row ->
    row#get_name
    ^ ","
    ^ row#get_usage_time_string
    ^ ","
    ^ row#get_active_time_string
    ^ ","
    ^ row#get_is_active_string)
;;

let pretty_print pinfos =
  Printf.printf "\027[H\027[2J";
  let line = red ^ "|" ^ String.make 80 '-' ^ red ^ "|" ^ reset in
  Printf.printf "%s\n" line;
  Printf.printf "%s|%s%*s%s%*s%s|%s\n" red blue pad_left "" text pad_right "" red reset;
  Printf.printf "%s\n" line;
  Printf.printf
    "%s|%s Today's Active Time : %10s %-51s|%s\n"
    red
    reset
    (sum_active_time pinfos |> string_of_time)
    red
    reset;
  Printf.printf
    "%s|%s Today's Total Usage : %10s %-51s|%s\n"
    red
    reset
    (sum_usage_time pinfos |> string_of_time)
    red
    reset;
  Printf.printf "%s\n" line;
  Printf.printf
    "%s|%s%-30s%20s%30s%s|%s\n"
    red
    yellow
    "Clients"
    "lifetime"
    "Active Time"
    red
    reset;
  Printf.printf "%s\n" line;
  pinfos
  |> List.sort (fun (x : info) (y : info) ->
    compare_time x#get_active_time y#get_active_time)
  |> List.iter (fun (pinfo : info) ->
    Printf.printf
      "%s|%s%-30s%s%s%20s%s%s%30s%s|%s\n"
      red
      (if pinfo#get_is_active then active_color else blue)
      pinfo#get_name
      reset
      green
      pinfo#get_usage_time_string
      reset
      green
      pinfo#get_active_time_string
      red
      reset);
  Printf.printf "%s\n" line;
  flush stdout
;;
