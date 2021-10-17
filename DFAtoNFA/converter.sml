structure Converter =
struct
    type S = string
    type Q = int
    type delta = (Q * (S * Q list) list) list
    type NFA = {Q:Q list, S:S list, delta:delta, qQ:Q, F:Q list}
    type state = Q list
    type Delta = (state * (S * state) list) list
    type DFA = {Q:state list, S:S list, Delta:Delta, Q0:state, F:state list}
    type Omega = (S * Q list) list


    (*Listをuniqueな要素にする*)
    fun isolate [] = []
      | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs)

    fun Cl (d, nil) = nil
      | Cl (d, h::ts) =  [(*(h, q) \in d*)] @ Cl (d, ts)
                                                 (* TODO: ここ、どうやって実装しよう *)

    fun targetOf R a = foldr (fn ((x,y), r) => if x = a then y::r else r) [] R;
    fun sourceOf R a = foldr (fn ((x,y), r) => if y = a then x::r else r) [] R;
    fun flatten list = case list of nil => []
                                  | (h::t) => h@(flatten t);

    (* val deltaD : delta -> (state * S) -> Q list *)
    fun deltaD (d:delta) (P, ws:string) =
        let
            val listOfSQListMatchedP:(S * Q list) list list = flatten(List.map(fn p => targetOf d p)P) (* deltaの第一番目の要素が一致する(S * Q list) listの配列 *)
            val matchedSqss = List.map
                                  (fn sqlist => targetOf sqlist ws)
                                  listOfSQListMatchedP
        in
            flatten (flatten matchedSqss)
        end
                (* deltaの第一の要素がPの要素と一致するもの *)
                (* 第二引数のリストのうち*)
    fun mem (_, []) = false
      | mem (x, y::ys) = if x = y then true else mem(x, ys)
    (* val addS : delta -> (state * S) -> (state list * state list * Omega) -> (state list * Omega)*)
    fun addS d (A:state, s) (Q1:state list, Q2: state list, O) =
        let
            val A' = deltaD d (A, s)
            val Q1' = if mem(A, A::Q1@Q2)  then Q1
                      else A'::Q1
        in
            (Q1', (s, A')::O)
        end

    (* val addQ : delta -> state -> (state list * state list * Delta) -> (state list * state list * Delta) *)
    fun addQ d A (Q1:state list, Q2:state list, D) =
        let
            val (Q11, O1) = addS d (A, "a") (Q1, Q2, [])
            val (Q12, O2) = addS d (A, "b") (Q11, Q2, O1)
        in
            (Q12: state list, A::Q2, (A, O2)::D)
        end

    (* val subsets : delta -> ( state list * state list * Delta ) -> (state list * Delta) *)
    fun subsets (d:delta) (nil, Q, D) = (Q, D)
      | subsets (d:delta) (A::Q1, Q2, D) = subsets d (addQ d A (Q1, Q2, D))
    (* val Cl : (delta * Q list) -> Q list*)


    fun intersection ([], _) = []
      | intersection (x::xs, ys) =
        if mem (x, ys) then x :: intersection (xs, ys)
        else intersection (xs, ys)
    fun toDFA {Q = qList, S = SList, delta = d, qQ = qQ, F = F} =
        let
            val A = Cl (d, [qQ])
            val (QList, dList) = subsets d ([A], nil, nil)
            val FF = List.filter
                         (fn q => not(intersection (q, F) = []))
                         QList
        in
            {Q = QList, S = SList, Delta = dList, Q0 = A, F = FF}
        end
end
