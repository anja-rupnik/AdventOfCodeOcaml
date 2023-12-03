open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  
  let string_to_charlist string = 
    String.to_seq string |> List.of_seq |> List.map (String.make 1);;
  let is_int = function
    |a when int_of_string_opt a != None -> true
    |_ -> false
      
  let rec compare_lists a b =
    match a with
    |[] -> true
    |x::xs -> match b with
      |y::ys when y = x -> compare_lists xs ys
      |_-> false
  
  let stevilke = [["o"; "n"; "e"];["t"; "w"; "o"];["t"; "h"; "r"; "e"; "e"];["f"; "o"; "u"; "r"];["f"; "i"; "v"; "e"];["s"; "i"; "x"];["s"; "e"; "v"; "e"; "n"];["e"; "i"; "g"; "h"; "t"];["n"; "i"; "n"; "e"]]
  let rec v_st el list2 ind =
    match list2 with
    |x::xs when compare_lists x el-> ind
    |x::xs -> v_st el xs ind+1
    |[] -> -1
  
  let rec pretvorjen_seznam acc = function
    |x::y::z::w::u::xs when List.mem (x::y::z::w::[u]) stevilke  -> pretvorjen_seznam (string_of_int(v_st (x::y::z::w::[u]) stevilke 1) :: acc) (y::z::w::u::xs)
    |x::y::z::w::xs when List.mem (x::y::z::[w]) stevilke  -> pretvorjen_seznam (string_of_int(v_st (x::y::z::[w]) stevilke 1) :: acc) (y::z::w::xs)
    |x::y::z::xs when List.mem (x::y::[z]) stevilke  -> pretvorjen_seznam (string_of_int(v_st (x::y::[z]) stevilke 1) :: acc) (y::z::xs)
    |x::xs -> pretvorjen_seznam (x::acc) xs
    |[] -> acc
          
  let prvaa line =
    let list = pretvorjen_seznam [] (string_to_charlist line) in
    List.find is_int (List.rev list)
  let zadnjaa line =
    let list = pretvorjen_seznam [] (string_to_charlist line) in
    List.find is_int list  
    
  let prva line =
    let list = string_to_charlist line in
    List.find is_int list
  let zadnja line =
    let list = string_to_charlist line in
    List.find is_int (List.rev list)
      
  let rec sum = function
    |[] -> 0
    |x :: xs -> x + sum xs
  let vrni line =
    10*(int_of_string (prva line)) + (int_of_string (zadnja line))
                                     
  let vrnia line =
    10*(int_of_string (prvaa line)) + (int_of_string (zadnjaa line))
  
  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    sum (List.map vrni lines) 
    
  let naloga2 data =
    let lines = String.split_on_char '\n' data in
    sum (List.map vrnia lines)
end