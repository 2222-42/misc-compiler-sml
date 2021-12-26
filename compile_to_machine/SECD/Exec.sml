structure Exec =
struct
    open Instruction Value
    exception RuntimeError
    fun exec (v::S, _, nil, nil) = v
      | exec (S, E, PushI int :: C, D) =
        exec (INT int :: S, E, C, D)
      | exec (S, E, PushS string :: C, D) =
        exec (STRING string :: S, E, C, D)
      | exec (S, E, PushB bool :: C, D) =
        exec (BOOL bool :: S, E, C, D)
      | exec (S, E, Acc x :: C, D) =
        (case SEnv.find(E, x)
          of SOME v => exec (v::S, E, C, D)
           | NONE => raise RuntimeError)
      | exec (S, E, MkCLS(x, C0)::C, D) =
        exec (CLS(E,x, C0):: S, E, C, D)
      | exec (S, E, MkREC(f, x, C0)::C, D) =
        exec (REC(E,f,x,C0):: S, E, C, D)
      | exec (v::CLS(E0, x, C0)::S, E,                     App::C, D) =
        exec (S,                    SEnv.insert(E0, x, v), C0,     (C,E)::D)
      | exec (v::REC(E0, f, x, C0)::S, E,                     App::C, D) =
        exec (S, SEnv.insert (SEnv.insert(E0, f, REC(E0, f, x, C0)), x, v), C0,     (C,E)::D)
      | exec (S, E, Ret :: C, (C0, E0)::D) = exec(S, E0, C0, D)
      | exec (v1 :: v2 :: S, E, Pair :: C, D) = exec(PAIR (v2, v1) :: S, E, C, D)
      | exec (PAIR (v1, v2)::S, E, Proj1 :: C, D) =  exec (v1 :: S, E, C, D)
      | exec (PAIR (v1, v2)::S, E, Proj2 :: C, D) =  exec (v2 :: S, E, C, D)
      | exec (INT n1 :: INT n2 :: S, E, Prim p :: C, D) =
        (case p
          of EQ => exec (BOOL (n2 = n1) :: S, E, C, D)
           | ADD => exec (INT (n2 + n1) :: S, E, C, D)
           | SUB => exec (INT (n2 - n1) :: S, E, C, D)
           | MUL => exec (INT (n2 * n1) :: S, E, C, D)
           | DIV => exec (INT (n2 div n1) :: S, E, C, D)
        )
      | exec (BOOL true :: S, E, If (C1, C2) :: C, D) =
        exec (S, E, C1 @ C, D)
      | exec (BOOL false :: S, E, If (C1, C2) :: C, D) =
        exec (S, E, C2 @ C, D)
      | exec (_, _, C, _) = raise RuntimeError


    fun run env (id, code) =
        let
            val v = exec (nil, env, code, nil)
            val newEnv = SEnv.insert (env, id, v)
        in
            print ("Execution result: \n" ^
                   "val " ^ id ^ " = " ^ valueToString v ^ "\n");
            newEnv
        end
end
