structure Comp =
struct
    structure S = Syntax
    structure I = Instruction
    fun comp e K =
        case e
         of S.INT int => I.PushI int :: K
          | S.STRING str => I.PushS str :: K
          | S.TRUE => I.PushB true :: K
          | S.FALSE => I.PushB false :: K
          | S.EXPID string => I.Acc string :: K
          | S.EXPFN (x, e) => I.MkCLS(x, comp e [I.Ret]) :: K
          | S.EXPFIX (f, x, e) => I.MkREC(f, x, comp e [I.Ret]) :: K
          | S.EXPAPP (e1, e2) => comp e1 (comp e2 (I.App :: K))
          | S.EXPPAIR (e1, e2) => comp e1 (comp e2 (I.Pair :: K))
          | S.EXPPROJ1 e => comp e (I.Proj1 :: K)
          | S.EXPPROJ2 e => comp e (I.Proj2 :: K)
          | S.EXPPRIM (p, e1, e2) =>
            let
                val ip = case p
                          of S.EQ => I.EQ
                           | S.ADD => I.ADD
                           | S.SUB => I.SUB
                           | S.MUL => I.MUL
                           | S.DIV => I.DIV
            in
                comp e1 (comp e2 (I.Prim ip :: K))
            end
          (*| EXPPRIM1  (p, e) => *)
          | S.EXPIF (e1, e2, e3) =>
            comp e1 (I.If (comp e2 nil, comp e3 nil) :: K)
    fun compile (S.VAL (id, e)) =
        let
            val C = comp e nil
        in
            print ("Compiled to: \n"^
                   I.codeToString C ^ "\n");
            (id, C)
        end

end
