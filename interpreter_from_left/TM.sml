structure TM =
struct
    datatype D = R | L
    datatype S = B | I | O
    datatype Q = M | N | START | CLEAN | BACK | H
    type delta = ((Q * S) * (Q * S * D)) list
    type program = Q * delta
    type tape = S list * S * S list
    val P = (START,
             [
               ((START, B), (N, B, R)),
               ((N, I), (N, I, R)),
               ((N, O), (N, O, R)),
               ((N, B), (M, B, L)),
               ((M, I), (M, O, L)),
               ((M, O), (CLEAN, I, L)),
               ((M, B), (CLEAN, I, L)),
               ((CLEAN, I), (CLEAN, I, L)),
               ((CLEAN, O), (CLEAN, O, L)),
               ((CLEAN, B), (BACK, B, L)),
               ((BACK, B), (H, B, R))
             ]
            )
end
