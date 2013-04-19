:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
:-dynamic globalAssoc/2.
prim_setAssoc(_G1,_G2,'Prelude.()'):-string2Atom(_G1,_G6), (retract(globalAssoc(_G6,_G9))->true;true),assertz(globalAssoc(_G6,_G2)),!.
prim_getAssoc(_G55,_G56):-string2Atom(_G55,_G59), (globalAssoc(_G59,_G62)->_G56='Prelude.Just'(_G62);_G56='Prelude.Nothing'),!.
prim_execCmd(_G109,'Prelude.(,,)'(_G105,_G106,_G107)):-string2Atom(_G109,_G113),execCommand(_G113,_G105,_G106,_G107).
prim_connectToCmd(_G149,'$stream'('$inoutstream'(_G144,_G145))):-string2Atom(_G149,_G153),execCommand(_G153,_G145,_G144,std).
prim_newIORef(_G302,_G303,_G304,_G305):-freeze(_G304,blocked_prim_newIORef(_G302,_G303,_G304,_G305)).
blocked_prim_newIORef(_G336,partcall(1,exec_newIORef,[_G336]),_G345,_G345).
exec_newIORef(_G16,_G17,_G18,_G19,_G20):-freeze(_G19,blocked_exec_newIORef(_G16,_G17,_G18,_G19,_G20)).
blocked_exec_newIORef(_G61,_G62,'$io'('IOExts.IORef'(share(_G55))),_G64,_G65):-var(_G61),!,create_mutable('$eval'(_G61),_G55),_G64=_G65.
blocked_exec_newIORef(_G125,_G126,'$io'('IOExts.IORef'(share(_G119))),_G128,_G129):-create_mutable(_G125,_G119),_G128=_G129.
prim_readIORef(_G294,_G295,_G296,_G297):-freeze(_G296,blocked_prim_readIORef(_G294,_G295,_G296,_G297)).
blocked_prim_readIORef(_G328,partcall(1,exec_readIORef,[_G328]),_G337,_G337).
exec_readIORef(_G28,_G29,_G30,_G31,_G32):-freeze(_G31,blocked_exec_readIORef(_G28,_G29,_G30,_G31,_G32)).
blocked_exec_readIORef(_G69,_G70,'$io'(_G67),_G72,_G73):-user:derefRoot(_G69,'IOExts.IORef'(share(_G75))),get_mutable(_G85,_G75), (_G85='$eval'(_G67)->true;create_mutable(_G85,_G97),update_mutable(share(_G97),_G75),_G67=share(_G97)),_G72=_G73.
prim_writeIORef(_G288,_G289,_G290,_G291,_G292):-freeze(_G291,blocked_prim_writeIORef(_G288,_G289,_G290,_G291,_G292)).
blocked_prim_writeIORef(_G330,_G327,partcall(1,exec_writeIORef,[_G327,_G330]),_G340,_G340).
exec_writeIORef(_G40,_G41,_G42,_G43,_G44,_G45):-freeze(_G44,blocked_exec_writeIORef(_G40,_G41,_G42,_G43,_G44,_G45)).
blocked_exec_writeIORef(_G84,_G85,_G86,_G87,_G88,_G89):-user:derefRoot(_G84,_G92),prim_writeIORef_exec(_G92,_G85,_G87),_G88=_G89.
prim_writeIORef_exec('IOExts.IORef'(share(_G147)),_G154,'$io'('Prelude.()')):-var(_G154),!,update_mutable('$eval'(_G154),_G147).
prim_writeIORef_exec('IOExts.IORef'(share(_G195)),_G202,'$io'('Prelude.()')):-update_mutable(_G202,_G195).
