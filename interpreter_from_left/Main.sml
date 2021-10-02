open TM
val T =  (nil, B, [I, I, I, I]);
val r = Eval.eval P T;
val _ = Dynamic.pp {T = T, r = r};
