open Camel_my_dreams.Data

let () =
  let i = new info "zen" (new_time 2 4 0) (new_time 5 23 50) true in

  print_endline i#get_name;
  print_endline i#get_usage_time_string;
  print_endline i#get_active_time_string;
  print_endline i#get_is_active_string;
;;
