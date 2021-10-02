structure TM =
struct
    datatype D = R | L
    datatype S = B | I | O
    datatype Q = M | N | H
    datatype A = Move of D | Write of S
    type delta = ((Q * S) * (Q * A)) list
    type program = Q * delta
    type tape = S list * S * S list
    val P = (M, [((M, I), (N, Write(O))),
                 ((M, O), (H, Write(I))),
                 ((M, B), (H, Write(I))),
                 ((N, I), (M, Move(L))),
                 ((N, O), (M, Move(L)))
            ])
end
