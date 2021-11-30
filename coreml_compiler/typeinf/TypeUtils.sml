structure TypeUtils =
struct
    type subst = Type.ty SEnv.map
    type tyEnv = Type.ty SEnv.map
    fun substTy subst ty = (* subst -> Type.ty -> Type.ty*)
        case ty
         of Type.TYVARty string =>  (case SEnv.find (subst, string)
                                      of SOME existingTy => existingTy
                                       | NONE => ty)
         | Type.INTty => ty
         | Type.STRINGty => ty
         | Type.BOOLty => ty
         | Type.FUNty (ty1, ty2) => Type.FUNty (substTy subst ty1, substTy subst ty2)
         | Type.PAIRty (ty1, ty2) => Type.PAIRty (substTy subst ty1, substTy subst ty2)
    val emptySubst = (* subst*)
        SEnv.empty
    fun substTyEnv subst tyEnv = (* subst -> tyEnv -> tyEnv*)
        SEnv.map (substTy subst) tyEnv
    fun composeSubst subst1 subst2 = (* subst -> subst -> subst*)
        SEnv.unionWith #1 (
              subst1,
              SEnv.map (substTy subst1) subst2 (* 型代入では前者が優先される *)
            )

    val emptyTyEnv = (* tyEnv*)
        SEnv.empty
    fun singletonTyEnv (str, ty) = (* string * Type.ty -> tyEnv*)
        SEnv.singleton (str, ty)
    fun findTyEnv (tyEnv, str)= (* tyEnv * string -> Type.ty option*)
        SEnv.find (tyEnv, str)
    fun matches (tyEnv1, tyEnv2) = (* tyEnv * tyEnv -> (Type.ty * Type.ty) lsit*)
        SEnv.listItems
            (SEnv.intersectWith (fn x => x) (tyEnv1, tyEnv2))
    fun unionTyEnv (tyEnv1, tyEnv2) = (* tyEnv * tyEnv -> tyEnv*)
        SEnv.unionWith #2 (tyEnv1, tyEnv2) (* 型環境では後者の方が優先される *)
    fun removeTyEnv (tyEnv, string) = (* tyEnv * string -> tyEnv*)
        #1 (SEnv.remove (tyEnv, string))
    fun tyEnvToString tyEnv = (* tyEnv -> string*)

        let
            val typeList = SEnv.listItemsi tyEnv
        in
            "{" ^
            (
              String.concatWith ", "
                                (map (fn (key, ty) => key ^ ":" ^ Type.tyToString ty) typeList)
            )
            ^ "}"(* x:y,... といった形の連続 *)
        end
end
