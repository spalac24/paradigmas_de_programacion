:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
:-ensure_loaded(prim_ports).
prim_stdin(_G418):-prolog_flag(user_input,_G418).
prim_stdout(_G440):-prolog_flag(user_output,_G440).
prim_stderr(_G462):-prolog_flag(user_error,_G462).
prim_openFile(_G1,_G2,_G3):-string2Atom(_G1,_G6),curryFileMode2plmode(_G2,_G9),open(_G6,_G9,_G3).
curryFileMode2plmode('IO.ReadMode',read).
curryFileMode2plmode('IO.WriteMode',write).
curryFileMode2plmode('IO.AppendMode',append).
prim_hClose('$stream'('$inoutstream'(_G100,_G101)),'Prelude.()'):-!,flush_output(_G101),close(_G101), (_G100==_G101->true;close(_G100)).
prim_hClose(_G153,'Prelude.()'):- (isOutputStream(_G153)->flush_output(_G153);true),close(_G153).
prim_hFlush('$stream'('$inoutstream'(_G192,_G193)),'Prelude.()'):-!,flush_output(_G193).
prim_hFlush(_G226,'Prelude.()'):-isOutputStream(_G226)->flush_output(_G226);true.
prim_hIsEOF('$stream'('$inoutstream'(_G260,_G261)),_G266):-!, (atEndOfStream(_G260)->_G266='Prelude.True';_G266='Prelude.False').
prim_hIsEOF(_G306,_G307):-atEndOfStream(_G306)->_G307='Prelude.True';_G307='Prelude.False'.
prim_hSeek(_G344,_G345,_G346,'Prelude.()'):-currySeekMode2plmode(_G345,_G350),seek(_G344,_G346,_G350,_G355).
currySeekMode2plmode('IO.AbsoluteSeek',bof).
currySeekMode2plmode('IO.RelativeSeek',current).
currySeekMode2plmode('IO.SeekFromEnd',eof).
prim_hWaitForInputs(_G115,_G116,_G117,_G118,_G119):-freeze(_G118,blocked_prim_hWaitForInputs(_G115,_G116,_G117,_G118,_G119)).
blocked_prim_hWaitForInputs(_G157,_G154,partcall(1,exec_hWaitForInputs,[_G154,_G157]),_G167,_G167).
exec_hWaitForInputs(_G335,_G336,_G337,_G338,_G339,_G340):-freeze(_G339,blocked_exec_hWaitForInputs(_G335,_G336,_G337,_G338,_G339,_G340)).
blocked_exec_hWaitForInputs(_G381,_G382,_G383,'$io'(_G379),_G385,_G386):-user:derefAll(_G381,_G389),selectInstreams(_G389,_G395),user:derefRoot(_G382,_G398),waitForInputDataOnStreams(_G395,_G398,_G379),!,_G385=_G386.
selectInstreams([],[]).
selectInstreams(['$stream'('$inoutstream'(_G13,_G14))|_G19],[_G13|_G22]):-!,selectInstreams(_G19,_G22).
selectInstreams([_G51|_G52],[_G51|_G55]):-selectInstreams(_G52,_G55).
prim_hGetChar('$stream'('$inoutstream'(_G84,_G85)),_G90):-!,get_code(_G84,_G93),char_int(_G90,_G93).
prim_hGetChar(_G125,_G126):-get_code(_G125,_G129),char_int(_G126,_G129).
prim_hPutChar('$stream'('$inoutstream'(_G158,_G159)),_G164,'Prelude.()'):-!,char_int(_G164,_G168),put_code(_G159,_G168).
prim_hPutChar(_G204,_G205,'Prelude.()'):-char_int(_G205,_G209),put_code(_G204,_G209).
prim_hIsReadable('$stream'('$inoutstream'(_G242,_G243)),'Prelude.True'):-!.
exec_hIsReadable(_G271,_G272):-isInputStream(_G271)->_G272='Prelude.True';_G272='Prelude.False'.
prim_hIsWritable('$stream'('$inoutstream'(_G309,_G310)),'Prelude.True'):-!.
prim_hIsWritable(_G338,_G339):-isOutputStream(_G338)->_G339='Prelude.True';_G339='Prelude.False'.
prim_hWaitForInputsOrMsg(_G139,_G140,_G141,_G142,_G143):-freeze(_G142,blocked_prim_hWaitForInputsOrMsg(_G139,_G140,_G141,_G142,_G143)).
blocked_prim_hWaitForInputsOrMsg(_G181,_G178,partcall(1,exec_hWaitForInputsOrMsg,[_G178,_G181]),_G191,_G191).
exec_hWaitForInputsOrMsg(_G396,_G397,_G398,_G399,_G400,_G401):-freeze(_G400,freeze(_G397,blocked_exec_hWaitForInputsOrMsg(_G396,_G397,_G398,_G399,_G400,_G401))).
blocked_exec_hWaitForInputsOrMsg(_G27,share(_G25),_G29,_G30,_G31,_G32):-!,get_mutable(_G34,_G25), (_G34='$eval'(_G37)->_G31=_G32,_G30='$io'('Prelude.Right'(_G37));exec_hWaitForInputsOrMsg(_G27,_G34,_G29,_G61,_G31,_G32), (_G61='$io'('Prelude.Left'(_G65))->_G30=_G61;_G61='$io'('Prelude.Right'(_G78)), (compileWithSharing(variable)->user:propagateShare(_G78,_G88);_G78=_G88),_G30='$io'('Prelude.Right'(_G88)),update_mutable('$eval'(_G88),_G25))).
blocked_exec_hWaitForInputsOrMsg(_G185,[_G175|_G176],_G187,'$io'('Prelude.Right'([_G175|_G176])),_G189,_G190):-!,_G189=_G190.
blocked_exec_hWaitForInputsOrMsg(_G239,[],_G241,'$io'('Prelude.Left'(_G235)),_G243,_G244):-!,user:derefAll(_G239,_G247),selectInstreams(_G247,_G253),waitForInputDataOnStreams(_G253,-1,_G235),!,_G243=_G244.
blocked_exec_hWaitForInputsOrMsg(_G316,'Ports.basicServerLoop'(_G314),_G318,_G319,_G320,_G321):-_G314='Ports.internalPort'(_G323,_G324,_G325,_G326),checkIncomingPortStreams(_G325,_G332,_G333),!,readStreamUntilEndOfTerm(_G332,_G336), (parse_received_message(_G332,_G333,_G336,_G341)->ifTracePort((write(user_error,'TRACEPORTS: Received message: '),write(user_error,_G341),nl(user_error))),_G319='$io'('Prelude.Right'([_G341|'Ports.basicServerLoop'(_G314)])),_G320=_G321;write(user_error,'ERROR: Illegal message received (ignored): '),putChars(user_error,_G336),nl(user_error),exec_hWaitForInputsOrMsg(_G316,'Ports.basicServerLoop'(_G314),_G318,_G319,_G320,_G321)),!.
blocked_exec_hWaitForInputsOrMsg(_G27,'Ports.basicServerLoop'(_G25),_G29,_G30,_G31,_G32):-user:derefAll(_G27,_G35),selectInstreams(_G35,_G41),_G25='Ports.internalPort'(_G43,_G44,_G45,_G46),waitForSocketOrInputStreams(_G46,_G52,_G53,_G54,_G41,_G56), (_G52=no->_G30='$io'('Prelude.Left'(_G56)),_G31=_G32;ifTracePort((write(user_error,'TRACEPORTS: Connection to client: '),write(user_error,_G52),nl(user_error))), (readPortMessage(_G45,_G53,_G54,_G96)-> (parse_received_message(_G53,_G54,_G96,_G101)->ifTracePort((write(user_error,'TRACEPORTS: Received message: '),write(user_error,_G101),nl(user_error))),_G30='$io'('Prelude.Right'([_G101|'Ports.basicServerLoop'(_G25)])),_G31=_G32;write(user_error,'ERROR: Illegal message received (ignored): '),putChars(user_error,_G96),nl(user_error),exec_hWaitForInputsOrMsg(_G35,'Ports.basicServerLoop'(_G25),_G29,_G30,_G31,_G32));exec_hWaitForInputsOrMsg(_G35,'Ports.basicServerLoop'(_G25),_G29,_G30,_G31,_G32))),!.
