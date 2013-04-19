:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module(library(clpfd)).
prim_FD_domain(_G0,_G1,_G2,_G3):- (prolog(sicstus)->domain(_G0,_G1,_G2);ins(_G0,..(_G1,_G2))),_G3='Prelude.success'.
prim_FD_sum(_G37,_G38,_G39,_G40):-checkSICStusAndWarn('CLPFD.sum'),_G38=partcall(2,_G45,[]),translateFD_Rel(_G45,_G52),sum(_G37,_G52,_G39),_G40='Prelude.success'.
prim_FD_scalar_product(_G102,_G103,_G104,_G105,_G106):-checkSICStusAndWarn('CLPFD.scalar_product'),_G104=partcall(2,_G111,[]),translateFD_Rel(_G111,_G118),scalar_product(_G102,_G103,_G118,_G105),_G106='Prelude.success'.
prim_FD_count(_G173,_G174,_G175,_G176,_G177):-checkSICStusAndWarn('CLPFD.count'),_G175=partcall(2,_G182,[]),translateFD_Rel(_G182,_G189),count(_G173,_G174,_G189,_G176),_G177='Prelude.success'.
translateFD_Rel('CLPFD.=#',#=):-!.
translateFD_Rel('CLPFD./=#',#\=):-!.
translateFD_Rel('CLPFD.<#',#<):-!.
translateFD_Rel('CLPFD.<=#',#=<):-!.
translateFD_Rel('CLPFD.>#',#>):-!.
translateFD_Rel('CLPFD.>=#',#>=):-!.
translateFD_Rel(_G388,_G389):-writeErr('ERROR: Illegal FD constraint: '),writeErr(_G388),nlErr,!,fail.
prim_FD_all_different(_G428,_G429):-all_different(_G428),_G429='Prelude.success'.
prim_FD_indomain(_G460,_G461):- (prolog(sicstus)->indomain(_G460);label([_G460])),_G461='Prelude.success'.
prim_FD_labeling(_G1,_G2,_G3):-map2M(user:translateLabelingOption,_G1,_G10),labeling(_G10,_G2),_G3='Prelude.success'.
translateLabelingOption('CLPFD.LeftMost',leftmost).
translateLabelingOption('CLPFD.FirstFail',ff).
translateLabelingOption('CLPFD.FirstFailConstrained',ffc).
translateLabelingOption('CLPFD.Min',min).
translateLabelingOption('CLPFD.Max',max).
translateLabelingOption('CLPFD.Step',step).
translateLabelingOption('CLPFD.Enum',enum).
translateLabelingOption('CLPFD.Bisect',bisect).
translateLabelingOption('CLPFD.Up',up).
translateLabelingOption('CLPFD.Down',down).
translateLabelingOption('CLPFD.All',all):-sicsLabel.
translateLabelingOption('CLPFD.Minimize'(_G253),minimize(_G253)):-sicsLabel.
translateLabelingOption('CLPFD.Maximize'(_G281),maximize(_G281)):-sicsLabel.
translateLabelingOption('CLPFD.Assumptions'(_G309),assumptions(_G309)):-sicsLabel.
sicsLabel:-checkSICStusAndWarn('CLPFD.labeling: labeling options').
prim_FD_plus(_G351,_G352,_G353):- #=(_G353,_G352+_G351).
prim_FD_minus(_G386,_G387,_G388):- #=(_G388,_G387-_G386).
prim_FD_times(_G421,_G422,_G423):- #=(_G423,_G422*_G421).
prim_FD_equal(_G456,_G457,'Prelude.success'):- #=(_G457,_G456).
prim_FD_notequal(_G1,_G2,'Prelude.success'):- #\=(_G2,_G1).
prim_FD_le(_G30,_G31,'Prelude.success'):- #<(_G31,_G30).
prim_FD_leq(_G62,_G63,'Prelude.success'):- #=<(_G63,_G62).
prim_FD_ge(_G94,_G95,'Prelude.success'):- #>(_G95,_G94).
prim_FD_geq(_G126,_G127,'Prelude.success'):- #>=(_G127,_G126).
prim_FD_solve_reify(_G158,_G159):-translateConstraint(_G158,_G162),call(_G162),_G159='Prelude.success'.
translateConstraint(_G196,_G196):-var(_G196),!.
translateConstraint(_G225,_G225):-integer(_G225),!.
translateConstraint('CLPFD.FDEqual'(_G254,_G255),#=(_G257,_G258)):-translateConstraint(_G254,_G257),translateConstraint(_G255,_G258).
translateConstraint('CLPFD.FDNotEqual'(_G293,_G294),#\=(_G296,_G297)):-translateConstraint(_G293,_G296),translateConstraint(_G294,_G297).
translateConstraint('CLPFD.FDLess'(_G332,_G333),#<(_G335,_G336)):-translateConstraint(_G332,_G335),translateConstraint(_G333,_G336).
translateConstraint('CLPFD.FDLessOrEqual'(_G371,_G372),#=<(_G374,_G375)):-translateConstraint(_G371,_G374),translateConstraint(_G372,_G375).
translateConstraint('CLPFD.FDGreater'(_G410,_G411),#>(_G413,_G414)):-translateConstraint(_G410,_G413),translateConstraint(_G411,_G414).
translateConstraint('CLPFD.FDGreaterOrEqual'(_G449,_G450),#>=(_G452,_G453)):-translateConstraint(_G449,_G452),translateConstraint(_G450,_G453).
translateConstraint('CLPFD.FDNeg'(_G1),#\(_G3)):-translateConstraint(_G1,_G3).
translateConstraint('CLPFD.FDAnd'(_G29,_G30),#/\(_G32,_G33)):-translateConstraint(_G29,_G32),translateConstraint(_G30,_G33).
translateConstraint('CLPFD.FDOr'(_G68,_G69),#\/(_G71,_G72)):-translateConstraint(_G68,_G71),translateConstraint(_G69,_G72).
translateConstraint('CLPFD.FDImply'(_G107,_G108),_G111):-translateConstraint(_G107,_G114),translateConstraint(_G108,_G117), (prolog(sicstus)->_G111= #=>(_G114,_G117);_G111= #==>(_G114,_G117)).
translateConstraint('CLPFD.FDEquiv'(_G166,_G167),_G170):-translateConstraint(_G166,_G173),translateConstraint(_G167,_G176), (prolog(sicstus)->_G170= #<=>(_G173,_G176);_G170= #<==>(_G173,_G176)).
