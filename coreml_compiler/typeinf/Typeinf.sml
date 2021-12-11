structure Typeinf =
struct
    open Type Syntax TypeUtils UnifyTy
    exception TypeError
    fun PTS absyn =
        case absyn
         of INT int => (emptyTyEnv, INTty)
          | STRING string => (emptyTyEnv, STRINGty)
          | TRUE => (emptyTyEnv, BOOLty)
          | FALSE => (emptyTyEnv, BOOLty)
          | EXPID string =>
            let
                val newty = newTy()
            in
                (singletonTyEnv(string, newty), newty)
            end
          | EXPFN (string, exp) =>
            let
                val (tyEnv, ty) = PTS exp
            in
                case findTyEnv(tyEnv, string)
                 of SOME domty => (removeTyEnv(tyEnv, string), FUNty(domty, ty))
                 | NONE => (tyEnv, FUNty(newTy(), ty))
            end
          | EXPAPP (exp1, exp2) =>
            let
                val (tyEnv1, ty1) = PTS exp1
                val (tyEnv2, ty2) = PTS exp2
                val tyEquations = matches (tyEnv1, tyEnv2)
                val newty = newTy()
                val subst = unify ((FUNty(ty2, newty), ty1) :: tyEquations)
                val tyEnv3 =
                    unionTyEnv (substTyEnv subst tyEnv1, substTyEnv subst tyEnv2)
            in
                (tyEnv3, substTy subst newty)
            end
          | EXPPAIR (exp1, exp2) =>
            let
                val (tyEnv1, ty1) = PTS exp1
                val (tyEnv2, ty2) = PTS exp2
                val tyEquations = matches (tyEnv1, tyEnv2)
                val subst = unify (tyEquations)
                val tyEnv3 =
                    substTyEnv subst (unionTyEnv(tyEnv1, tyEnv2))
            in
                (tyEnv3, PAIRty(substTy subst ty1, substTy subst ty2))
            end
          | EXPPROJ1 exp =>
            let
                val (tyEnv1, ty) = PTS exp
                val ty1 = newTy()
                val ty2 = newTy()
                val subst = unify ((ty, PAIRty(ty1, ty2)) :: [])
            in
                (substTyEnv subst tyEnv1, substTy subst ty1)
            end
          | EXPPROJ2 exp =>
            let
                val (tyEnv1, ty) = PTS exp
                val ty1 = newTy()
                val ty2 = newTy()
                val subst = unify ((ty, PAIRty(ty1, ty2)) :: [])
            in
                (substTyEnv subst tyEnv1, substTy subst ty2)
            end
          | EXPPRIM (p, exp1, exp2) =>
            let
                val (tyEnv1, ty1) = PTS exp1
                val (tyEnv2, ty2) = PTS exp2
                val tyEquations = matches (tyEnv1, tyEnv2)
                val subst = unify ((ty1, INTty) :: (ty2, INTty) :: [] )
                val ty3 = case p
                           of EQ => BOOLty
                            | _ => INTty
            in
                (unionTyEnv (substTyEnv subst tyEnv1, substTyEnv subst tyEnv2), ty3)
            end
          | EXPIF (exp1, exp2, exp3) =>
            let
                val (tyEnv1, ty1) = PTS exp1
                val (tyEnv2, ty2) = PTS exp2
                val (tyEnv3, ty3) = PTS exp3
                val tyEquations1 = matches (tyEnv1, tyEnv2)
                val tyEquations2 = matches (tyEnv1, tyEnv3)
                val tyEquations3 = matches (tyEnv2, tyEnv3)
                val subst = unify((ty1, BOOLty) :: (ty2, ty3) :: tyEquations1 @ tyEquations2 @ tyEquations3)
            in
                (unionTyEnv (substTyEnv subst tyEnv1, unionTyEnv(substTyEnv subst tyEnv2, substTyEnv subst tyEnv3)), substTy subst ty2)
            end
          (*| _ => (print ("pts error. exp: " ^ Syntax.expToString absyn  ^"\n");raise TypeError)*) (*網羅しているからこれは要らない。*)
    fun typeinf dec =
        let
            val exp =
                case dec
                 of Syntax.VAL (id, exp) => exp
                  | Syntax.FUN (f, x, exp) => exp
            val (tyEnv, ty) = PTS exp
            val _ = print
                        ("Inferred Typing: \n"
                        ^ TypeUtils.tyEnvToString tyEnv
                        ^ " |- " ^ Syntax.expToString exp
                        ^ " : " ^ Type.tyToString ty
                        ^ "\n")
        in
            ()
        end
        handle UnifyTy.UnifyTy => raise TypeError
end
