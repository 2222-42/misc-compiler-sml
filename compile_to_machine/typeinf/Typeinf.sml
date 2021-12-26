structure Typeinf =
struct
    open Type Syntax TypeUtils UnifyTy
    exception TypeError

    fun W gamma exp =
        case exp
         of INT int => (emptySubst, INTty)
          | STRING string => (emptySubst, STRINGty)
          | TRUE => (emptyTyEnv, BOOLty)
          | FALSE => (emptyTyEnv, BOOLty)
          | EXPID string =>
            (case SEnv.find(gamma, string)
             of SOME ty => (emptySubst, freshInst ty)
              | NONE => raise TypeError)
          | EXPFN (string, exp) =>
            let
                val ty1 = newTy()
                val newGamma = SEnv.insert(gamma, string, ty1)
                val (S, ty2) = W newGamma exp
            in
                (S, FUNty(substTy S ty1, ty2))
            end
          | EXPAPP (exp1, exp2) =>
            let
                val (S1, ty1) = W gamma exp1
                val (S2, ty2) = W (substTyEnv S1 gamma) exp2
                val ty3 = newTy ()
                val S3 = unify [(FUNty(ty2, ty3), substTy S2 ty1)]
                val S4 = composeSubst S3 (composeSubst S2 S1)
            in
                (S4, substTy S4 ty3)
            end
          | EXPPAIR (exp1, exp2) =>
            let
                val (S1, ty1) = W gamma exp1
                val (S2, ty2) = W (substTyEnv S1 gamma) exp2
            in
                (composeSubst S2 S1, PAIRty(substTy S2 ty1 , ty2))
            end
          | EXPPROJ1 exp =>
            let
                val (S1, ty) = W gamma exp
                val ty1 = newTy()
                val ty2 = newTy()
                val S2 = unify [(ty, PAIRty(ty1, ty2))]
            in
                (composeSubst S2 S1, substTy S2 ty1)
            end
          | EXPPROJ2 exp =>
            let
                val (S1, ty) = W gamma exp
                val ty1 = newTy()
                val ty2 = newTy()
                val S2 = unify [(ty, PAIRty(ty1, ty2))]
            in
                (composeSubst S2 S1, substTy S2 ty2)
            end
          | EXPPRIM (p, exp1, exp2) =>
            let
                val (S1, ty1) = W gamma exp1
                val (S2, ty2) = W (substTyEnv S1 gamma) exp2
                val S3 = unify ((ty1, INTty) :: (ty2, INTty) :: [])
                val ty3 = case p
                           of EQ => BOOLty
                            | _ => INTty
                val S4 = composeSubst S3 (composeSubst S2 S1)
            in
                (S4, ty3)
            end
          | EXPIF (exp1, exp2, exp3) =>
            let
                val (S1, ty1) = W gamma exp1
                val (S2, ty2) = W (substTyEnv S1 gamma) exp2
                val (S3, ty3) = W (substTyEnv (composeSubst S2 S1) gamma) exp3
                val S4 = unify ((substTy (composeSubst S2 S1) ty1, BOOLty) :: (substTy S3 ty2, ty3) :: [])
            in
                (composeSubst S4 (composeSubst S3 (composeSubst S2 S1)), substTy S4 ty3)
            end
          | EXPFIX (fid1, arg1, exp) =>
            let
                (*fの型とxの型はgammaには入ってないから追加する必要がある*)
                val argty = newTy()
                val returnty = newTy()
                val funty = FUNty(argty, returnty)
                val newGamma = SEnv.insert(SEnv.insert(gamma, fid1, funty), arg1, argty)
                val (S1, expty) = W newGamma exp
                val S2 = unify [(expty, returnty)]                    (* expty と funtyの返り値の型が一致する必要がある*)
                val S = composeSubst S2 S1
            in
                (S, substTy S funty) (* fid の型がそのまま入る *)
            end

    fun typeinf gamma dec =
        let
            val (id, exp) =
                case dec
                 of Syntax.VAL (id, exp) => (id, exp)
            val (subst, ty) = W gamma exp
            val tids = SSet.listItems (FTV ty)
            val newty =
                if null tids then ty
                else POLYty (tids, ty)
            val _ = print
                        ("Inferred Typing: \n"
                        ^ "val " ^ id ^ " : "
                        (*^ " |- " ^ Syntax.expToString exp
                        ^ " : "*) ^ Type.tyToString newty
                        ^ "\n")
        in
            SEnv.insert (gamma, id, newty)
        end
        handle UnifyTy.UnifyTy => raise TypeError
end
