:-style_check(-singleton).
:-use_module('../prologbasics').
:-prolog(sicstus)->use_module(library(clpb));onlySICStusMessage('CLPB constraints').
clpb_neg(_G448,_G449):- ~(_G448)=_G449.
clpb_and(_G0,_G1,_G2):-_G1*_G0=_G2.
clpb_or(_G29,_G30,_G31):-_G30+_G29=_G31.
clpb_xor(_G64,_G65,_G66):- #(_G65,_G64)=_G66.
clpb_card(_G99,_G100,_G101):-_G101=card(_G99,_G100).
clpb_exists(_G134,_G135,_G136):-_G134^_G135=_G136.
clpb_sat(_G169,_G170):-sat(_G169),_G170='Prelude.success'.
clpb_check(_G201,_G202):-taut(_G201,_G202).
clpb_labeling(_G228,_G229):-labeling(_G228),_G229='Prelude.success'.
