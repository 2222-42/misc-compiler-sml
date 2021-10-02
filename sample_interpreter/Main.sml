open TM
val T = ([I, I, I], I, nil);
val r = Eval.eval P T;
val _ = Dynamic.pp {T = T, r = r};

val T2 = (nil, B, [I, I, I, I]);
val r2 = Eval.eval P T2;
val _ = Dynamic.pp {T = T2, r = r2};
