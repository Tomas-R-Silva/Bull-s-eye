open Types


let points_of_throws t =                     (* dependendo se é um S D ou T duplica triplica ou mantém igual*)
  match t with
  | S x -> x          
  | D x -> x *2
  | T x -> x *3
;;

let all_possible_throws =
  let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10;                      (* vai criar a lista com todas as opcoes possiveis de lancamentos como no exemplo das cartas*)
               11; 12; 13; 14; 15; 16; 17; 18; 19; 20] in
                let singles = List.map( fun n -> S n) numbers in
                let doubles = List.map( fun n -> D n) numbers in
                let triples = List.map (fun n -> T n) numbers in
                let bulls = [S 25; D 25] in
                singles @ doubles @ triples @ bulls
;;
        


let  compute_checkouts (score : int) : checkouts =
  let all_p_throws = all_possible_throws in 

 if score < 0 then []
  else
    match all_p_throws with
    |[] -> []
    |x::xs ->  compute_checkouts score 


;;


let rec rec_compute_checkouts score all_possible_throws =

  if score < 0 then []
  else 
    match all_possible_throws with
  |[] ->  
    if score = 0 then [[]] else []
  |x::xs -> 
    let without_throw = rec_compute_checkouts score xs in
    let with_throw = rec_compute_checkouts ( score - points_of_throws x) xs in
    let with_throw_added = List.map (fun f -> x::f) with_throw in
    without_throw @ with_throw_added

