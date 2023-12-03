open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct

  let razrezi line =
    String.split_on_char ' ' line
  
  let rec vredu list =
    match list with
    |[] -> 1
    |x::y::xs -> match (List.hd (String.split_on_char ';' (List.hd (String.split_on_char ',' y)))) with
      |"red" when int_of_string(x) < 13 -> vredu xs
      |"blue" when int_of_string(x) < 15 -> vredu xs
      |"green" when int_of_string(x) < 14 -> vredu xs
      |_ -> 0
  let rec druga list r g b =
    match list with
    |[] -> r*b*g
    |x::y::xs -> match (List.hd (String.split_on_char ';' (List.hd (String.split_on_char ',' y)))) with
      |"red" when int_of_string(x) > r -> druga xs (int_of_string(x)) g b
      |"blue" when int_of_string(x) > b -> druga xs r g (int_of_string(x))
      |"green"when int_of_string(x) > g -> druga xs r (int_of_string(x)) b
      |_ -> druga xs r g b
  

  let prva list =
    match list with
    |x::y::xs -> (int_of_string(List.hd (String.split_on_char ':' y))) * (vredu xs)
    |_ -> 0
    
    let drugaa list =
      match list with
      |x::y::xs -> (druga xs 0 0 0)
      |_ -> 0
      
  let rec sum = function
    |[] -> 0
    |x :: xs -> x + sum xs
    
  let naloga1 data =
    let lines = String.split_on_char '\n' data in
    sum (List.map prva (List.map razrezi lines))

let naloga2 data =
  let lines = String.split_on_char '\n' data in
  sum (List.map drugaa (List.map razrezi lines))


  end