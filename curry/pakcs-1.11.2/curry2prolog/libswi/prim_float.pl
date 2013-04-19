:-style_check(-singleton).
prim_Float_plus(_G430,_G431,_G432):-_G432 is _G431+_G430.
prim_Float_minus(_G0,_G1,_G2):-_G2 is _G1-_G0.
prim_Float_times(_G17,_G18,_G19):-_G19 is _G18*_G17.
prim_Float_div(_G52,_G53,_G54):-_G54 is _G53/_G52.
prim_i2f(_G87,_G88):-_G88 is _G87*1.0.
prim_truncate(_G120,_G121):-_G121 is integer(_G120).
prim_round(_G149,_G150):-_G150 is integer(round(_G149)).
prim_sqrt(_G180,_G181):-_G181 is sqrt(_G180).
prim_log(_G209,_G210):-_G210 is log(_G209).
prim_exp(_G238,_G239):-_G239 is exp(_G238).
prim_sin(_G267,_G268):-_G268 is sin(_G267).
prim_cos(_G296,_G297):-_G297 is cos(_G296).
prim_tan(_G325,_G326):-_G326 is tan(_G325).
prim_atan(_G354,_G355):-_G355 is atan(_G354).
