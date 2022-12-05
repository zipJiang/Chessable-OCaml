open Core;;

let int_to_char (i: int): char =
  match i with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> failwith "number cannot be cast to single digit char."

let filter_option (olist: 'a option list): 'a list =
  (* This function takes in a list of options and remove nones and option side to result *)
  let olist = List.filter ~f:(fun x -> match x with None -> false | Some _ -> true) olist in
  List.map ~f:(fun x -> match x with Some xx -> xx | None -> failwith "None should have been filtered!") olist