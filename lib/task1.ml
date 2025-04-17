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
                singles @doubles @ triples@ bulls
;;


        
let rec rec_compute_checkouts score remaining_throws  counter =

    if score < 0 || counter > 3 then []
    else 
      match remaining_throws with
    |[] ->  
      if score = 0 then [[]] else []
    |x::xs -> 
      let with_throw_added =
        if score - points_of_throws x = 0 && counter + 1 <= 3 then
          match x with
          | D _ -> [[x]]
          | _ -> []
        else
        let tails = rec_compute_checkouts (score - points_of_throws x) all_possible_throws (counter + 1) in
        List.map (fun f -> x::f) tails in
      with_throw_added @ rec_compute_checkouts score xs counter;;


let compute_checkouts (score : int) : checkouts =
  
  rec_compute_checkouts score all_possible_throws 0;;






     

