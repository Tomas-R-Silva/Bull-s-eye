open Types

(* is_D_Last_Same e arePermutations são funções que foram criadas com assistência de ferramentas de AI*)

let points_of_throws t =                     (* dependendo se é um S D ou T duplica triplica ou mantém igual*)
  match t with
  | S x -> x          
  | D x -> x *2
  | T x -> x *3
;;

let all_possible_throws =
  let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10;                      (* vai criar a lista com todas as opcoes possiveis de lancamentos*)
               11; 12; 13; 14; 15; 16; 17; 18; 19; 20] in
                let singles = List.map( fun n -> S n) numbers in
                let doubles = List.map( fun n -> D n) numbers in
                let triples = List.map (fun n -> T n) numbers in
                let bulls = [S 25; D 25] in
                singles @doubles@triples @bulls
;;


let is_D_Last_Same x y =                   (* Recebe um conjunto de throws, retorna true se tiver o mesmo D final, nunca vai chegar a _, _ -> pois têm sempre que ter D x no final*)
  match List.rev x , List.rev y with
  | D x::_ , D y::_ -> x = y
  | _ , _ -> false

let arePermutations l1 l2 =     (* vai ver se l1 e l2 são permutações um do outro*)
  List.sort compare l1 = List.sort compare l2;;


let rec eliminate_permutation list =   (*Recebe a mesma lista de listas da task 1 mas elimina as permutações*)
  match list with
  | [] -> []
  | x::xs ->      
    if List.exists ( fun y -> 
      List.length x = List.length y &&
      arePermutations x y && 
      is_D_Last_Same x y
      ) xs then
        eliminate_permutation xs
    else 
      x :: eliminate_permutation xs
  ;;


(*cria a lista com todas as combinações possíveis de throws com o score indicado que têm até 3 de length
Requires : [score >= 0, counter >= 0]*)
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
  eliminate_permutation (rec_compute_checkouts score all_possible_throws 0);;
