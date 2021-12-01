structure UnifyTy =
struct
    open Type TypeUtils
    exception UnifyTy
    fun FTV ty = SSet.empty (* todo: implement *)
                             (* やることはtyの中身を見て、その中に変数があったら追加していく *)
                             (* 追加していくものはTYVARtyのもの、 *)
                             (* あと、FUNtyやPAIRtyの中身を見て、そこをまた探索する *)
    fun occurs (TYVARty tv, ty) = SSet.member (FTV ty, tv)
      | occurs _ = false
    fun rewrite (nil, S) = S
      | rewrite ((ty1, ty2)::E, S) =
        if ty1 = ty2 then rewrite (E, S) else
        case (ty1, ty2)
         of (TYVARty tv, _) =>
            if occurs (ty1, ty2) then raise UnifyTy else
            let
                val S1 = SEnv.singleton(tv, ty2)
            in
                rewrite (map (fn (ty1, ty2) =>
                                 (substTy S1 ty1, substTy S1 ty2))
                             E,
                         composeSubst S1 S)
            end
          | (_, TYVARty tv) => rewrite((ty2, ty1)::E, S)
          | (FUNty (ty11, ty12), FUNty(ty21, ty22)) =>
            rewrite ((ty11, ty21)::(ty12, ty22)::E, S)
          | (PAIRty (ty11, ty12), PAIRty (ty21, ty22)) =>
            rewrite ((ty11, ty21)::(ty12, ty22)::E, S)
          | _ => raise UnifyTy
    fun unify E = rewrite (E, SEnv.empty)
end
