:-style_check(-singleton).
initGlobalVariable(_G1,_G2,_G3,_G4,_G5):-appendAtom('$GLOBAL_',_G1,_G9),user:nf(_G2,_G12,_G4,_G14),_G31=..[_G1,_G22,_G25,_G28],user:retractClause(_G31,_G35),_G3='GlobalVariable.GVarValue'(_G9,_G12),_G58=..[_G1,_G3,_G52,_G52],asserta(user:_G58),_G5=_G14,!.
prim_readGVar(_G250,_G251,_G252,_G253):-freeze(_G252,blocked_prim_readGVar(_G250,_G251,_G252,_G253)).
blocked_prim_readGVar(_G284,partcall(1,prim_readGVarWorld,[_G284]),_G293,_G293).
prim_readGVarWorld(_G453,_G454,_G455,_G456,_G457):-freeze(_G456,blocked_prim_readGVarWorld(_G453,_G454,_G455,_G456,_G457)).
blocked_prim_readGVarWorld('GlobalVariable.GVarValue'(_G13,_G14),_G19,'$io'(_G16),_G21,_G22):-_G21=eval(_G24), (findGVar(_G24,_G13,_G16)->_G22=_G21;_G16=_G14,_G22=eval([_G13/_G16|_G24])).
findGVar([_G95/_G96|_G99],_G95,_G96):-!.
findGVar([_G130|_G131],_G134,_G135):-findGVar(_G131,_G134,_G135).
prim_writeGVar(_G288,_G289,_G290,_G291,_G292):-freeze(_G291,blocked_prim_writeGVar(_G288,_G289,_G290,_G291,_G292)).
blocked_prim_writeGVar(_G330,_G327,partcall(1,prim_writeGVarWorld,[_G327,_G330]),_G340,_G340).
prim_writeGVarWorld(_G109,_G110,_G111,_G112,_G113,_G114):-freeze(_G113,blocked_prim_writeGVarWorld(_G109,_G110,_G111,_G112,_G113,_G114)).
blocked_prim_writeGVarWorld('GlobalVariable.GVarValue'(_G153,_G154),_G159,_G160,'$io'('Prelude.()'),_G162,_G163):-_G162=eval(_G165),updateGVar(_G165,_G153,_G159,_G173),_G163=eval(_G173).
updateGVar([],_G223,_G224,[_G223/_G224]).
updateGVar([_G257/_G258|_G261],_G257,_G264,[_G257/_G264|_G261]):-!.
updateGVar([_G303|_G304],_G310,_G311,[_G303|_G307]):-updateGVar(_G304,_G310,_G311,_G307).
