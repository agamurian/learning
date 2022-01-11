(* 3,6 --> [3,4,5,6] *)
(* fun counter (from: int, to: int) = *)
(*     if from=to *)
(*     then to::[] *)
(*     else from :: counter(from+1, to); *)

(* fun countup_from1 (x: int) = *)
(*     counter(1,x); *)


(* fun countup_from1 (x: int) = *)
(*     let *)
(*         fun counter (from: int, to: int) = *)
(*             if from=to *)
(*             then to::[] *)
(*             else from :: counter(from+1, to) *)
(*     in *)
(*         counter(1,x) *)
(*     end *)


fun countup_from1 (x: int) =
    let
        fun counter (from: int) =
            if from=x
            then to::[]
            else from :: counter(from+1)
    in
        counter(1)
    end
