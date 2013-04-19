:-module(prim_ports,[prim_openPort/3,prim_basicServerLoop/4,prim_sendPort/5,prim_after/4,prim_openProcessPort/2,prim_ping/3,prim_timeoutOnStream/5,prim_openPortOnSocket/3,prim_connectPortAtSocket/4,prim_choiceSPEP/5,checkIncomingPortStreams/3,ifTracePort/1,readPortMessage/4,parse_received_message/4]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
:-ensure_loaded(user:prim_standard).
:-use_module(prim_readshowterm).
:-dynamic only_localhost/1,tracePorts/1,mySocket/1,incomingPortStreams/2.
only_localhost(_G152):- (getEnv('PAKCS_LOCALHOST',_G155)->true;_G155=no), (_G155=yes->asserta(only_localhost(yes)),_G152=yes;asserta(only_localhost(no)),_G152=no),retract((only_localhost(_G195):-_G197,_G198)),!.
tracePorts(unknown).
ifTracePort(_G244):-tracePorts(_G246), (_G246=unknown->retract(tracePorts(_G251)), (getEnv('PAKCS_TRACEPORTS',_G256)->true;_G256=no), (_G256=yes->asserta(tracePorts(yes)),_G244;asserta(tracePorts(no)));_G246=yes->_G244;true).
prim_openPort(_G328,_G329,_G330):-nonvar(_G328),!,writeErr('ERROR: openPort: port is not a free variable'),nlErr,fail.
prim_openPort(_G373,_G374,_G375):-nonvar(_G374),!,writeErr('ERROR: openPort: stream is not a free variable'),nlErr,fail.
prim_openPort('Ports.internalPort'([],0,0,_G421),_G421,'Prelude.success').
prim_openPortOnSocket(_G1,_G2,_G3):- (retract(mySocket('Ports.internalPort'(_G5,_G6,_G7,_G8)))->_G17 is _G7+1,asserta(mySocket('Ports.internalPort'(_G5,_G6,_G17,_G8))), (_G1=_G6,_G2=_G17->true;writeErr('ERROR: inconsistent socket/port numbers in openPortOnSocket!'),nlErr);trySetSocketNumber(_G1),on_exception(_G70,listenOnNewSocket(_G1,_G61,_G8), (write('ERROR: openPortOnSocket "'),write(_G1),write('":'),nl,printError(_G70),!,fail)),atom2String(_G61,_G5), (var(_G2)->_G2=0;true),asserta(mySocket('Ports.internalPort'(_G5,_G1,_G2,_G8)))),mySocket(_G132),_G132='Ports.internalPort'(_G134,_G135,_G136,_G137),ifTracePort((writeErr('TRACEPORTS: Listen on port '),writeErr(_G136),writeErr(' at socket '),writeErr(_G135),writeErr(' on host '), (only_localhost(yes)->writeErr(localhost);string2Atom(_G134,_G160),writeErr(_G160)),writeErr(...),nlErr)), (compileWithSharing(function)->makeShare('Ports.basicServerLoop'(_G132),_G3);_G3='Ports.basicServerLoop'(_G132)).
trySetSocketNumber(_G250):-var(_G250),getEnv('PAKCS_SOCKET',_G255),atom_codes(_G255,_G258),number_codes(_G250,_G258),!.
trySetSocketNumber(_G292).
prim_basicServerLoop(_G444,_G445,_G446,_G447):-freeze(_G446,blocked_prim_basicServerLoop(_G444,_G445,_G446,_G447)).
blocked_prim_basicServerLoop(_G7,_G8,_G9,_G10):-readPort(_G7,_G13,-1),_G8=[_G13|_G17], (compileWithSharing(function)->makeShare('Ports.basicServerLoop'(_G7),_G17);_G17='Ports.basicServerLoop'(_G7)),_G9=_G10.
prim_timeoutOnStream(_G221,_G222,_G223,_G224,_G225):-freeze(_G224,blocked_prim_timeoutOnStream(_G221,_G222,_G223,_G224,_G225)).
blocked_prim_timeoutOnStream(_G260,_G261,_G262,_G263,_G264):-user:derefRoot(_G260,_G267),prim_timeoutOnStream_exec(_G267,_G261,_G262,_G263,_G264).
prim_timeoutOnStream_exec(_G314,_G315,'Prelude.Nothing',_G317,_G318):-var(_G315),!,_G317=_G318.
prim_timeoutOnStream_exec(_G366,share(_G364),_G368,_G369,_G370):-!,get_mutable(_G372,_G364), (_G372='$eval'(_G375)->_G368=_G375,_G369=_G370;prim_timeoutOnStream_exec(_G366,_G372,_G394,_G369,_G370), (_G394='Prelude.Nothing'->_G368=_G394; (compileWithSharing(variable)->user:propagateShare(_G394,_G368);_G394=_G368),update_mutable('$eval'(_G368),_G364))).
prim_timeoutOnStream_exec(_G15,[],'Prelude.Just'([]),_G18,_G19):-!,_G18=_G19.
prim_timeoutOnStream_exec(_G65,[_G57|_G58],'Prelude.Just'([_G57|_G58]),_G68,_G69):-!,_G68=_G69.
prim_timeoutOnStream_exec(_G112,'Ports.basicServerLoop'(_G110),_G114,_G115,_G116):- (readPort(_G110,_G119,_G112)-> (compileWithSharing(function)->makeShare('Ports.basicServerLoop'(_G110),_G127);_G127='Ports.basicServerLoop'(_G110)),_G114='Prelude.Just'([_G119|_G127]);_G114='Prelude.Nothing'),_G115=_G116.
send_extvar_binding(_G335,_G336,_G337,_G338):-freeze(_G336,blocked_send_extvar_binding(_G335,_G336,_G337,_G338)).
blocked_send_extvar_binding(_G369,_G370,_G371,_G372):-evalToken(_G374),user:normalizeAndCheck(_G370,_G377,_G374,_G379),user:waitUntilGround(_G377,_G379,_G386),eval_send_extvar_binding(_G369,_G377,_G371,_G386,_G372).
eval_send_extvar_binding(_G148,_G149,_G150,_G151,_G152):-freeze(_G151,blocked_eval_send_extvar_binding(_G148,_G149,_G150,_G151,_G152)).
blocked_eval_send_extvar_binding(nostream,_G188,_G189,_G190,_G191):-writeErr('ERROR: cannot send binding for variable '),writeErr(_G189),writeErr(': '),writeErr(_G188),nlErr,!.
blocked_eval_send_extvar_binding(_G249,_G250,_G251,_G252,_G253):-show4sending(_G250,_G256,_G257),ifTracePort((writeErr('TRACEPORTS: send binding for variable '),writeErr(_G251),writeErr(': '),writeChars(user_error,_G256),nlErr)),number_codes(_G251,_G283),putChars(_G249,[86|_G283]),put_code(_G249,10),writeChars(_G249,_G256),put_code(_G249,10),flush_output(_G249),!,_G253=done.
readPort('Ports.internalPort'(_G365,_G366,_G367,_G368),_G371,_G372):-var(_G368),!,string2Atom(_G365,_G377),writeErr('ERROR: readPort: Port '),writeErr(_G366),writeErr(/),writeErr(_G367),writeErr(@),writeErr(_G377),writeErr(' only opened for writing!'),nlErr.
readPort('Ports.internalPort'(_G25,_G26,_G27,_G28),_G29,_G30):-on_exception(_G37,readFromSocket(_G27,_G28,_G29,_G30), (printError(_G37)->_G29=[];_G29=[])),!.
checkIncomingPortStreams(_G65,_G66,_G67):-retract(incomingPortStreams(_G65,[message(_G66,_G67)|_G73])),asserta(incomingPortStreams(_G65,_G73)),!.
checkIncomingPortStreams(_G116,_G117,_G118):-retract(incomingPortStreams(_G116,[ping(_G120,_G121)|_G124])),asserta(incomingPortStreams(_G116,_G124)),answerPingOnStream(_G120,_G121),checkIncomingPortStreams(_G116,_G117,_G118),!.
answerPingOnStream(_G180,_G181):-write(_G181,ok),put_code(_G181,10),flush_output(_G181),closeSocketStream(_G180,_G181),ifTracePort((writeErr('TRACEPORTS: Ping request answered.'),nlErr)).
readFromSocket(_G234,_G235,_G236,_G237):-checkIncomingPortStreams(_G234,_G240,_G241),!,readStreamLine(_G240,_G244), (parse_received_message(_G240,_G241,_G244,_G236)->ifTracePort((writeErr('TRACEPORTS: Received message: '),putChars(user_error,_G244),nlErr));writeErr('ERROR: Illegal message received (ignored): '),putChars(user_error,_G244),nlErr,readFromSocket(_G234,_G235,_G236,_G237)),!.
readFromSocket(_G330,_G331,_G332,_G333):-waitForSocketClientStream(_G331,_G333,_G337,_G338,_G339),ifTracePort((writeErr('TRACEPORTS: Connection to client: '),writeErr(_G337),nlErr)), (readPortMessage(_G330,_G338,_G339,_G356)-> (parse_received_message(_G338,_G339,_G356,_G332)->ifTracePort((writeErr('TRACEPORTS: Received message: '),putChars(user_error,_G356),nlErr));writeErr('ERROR: Illegal message received (ignored): '),putChars(user_error,_G356),nlErr,readFromSocket(_G330,_G331,_G332,_G333));readFromSocket(_G330,_G331,_G332,_G333)),!.
readFromSocket(_G450,_G451,_G452,_G453):-ifTracePort((writeErr('TRACEPORTS: Timeout!'),nlErr)),fail.
readPortMessage(_G25,_G26,_G27,_G28):-readStreamLine(_G26,_G31),_G31=[_G33|_G34],checkMessageHeader(_G33,_G26,_G27,_G42),number_codes(_G44,_G34),!, (_G44=_G25->readPortMessageBody(_G42,_G28); (retract(incomingPortStreams(_G44,_G57))->append(_G57,[_G42],_G66);_G66=[_G42]),asserta(incomingPortStreams(_G44,_G66)),!,fail).
checkMessageHeader(78,_G141,_G142,message(_G141,_G142)):-!.
checkMessageHeader(80,_G178,_G179,ping(_G178,_G179)):-!.
checkMessageHeader(_G215,_G216,_G217,_G218):-writeErr('ERROR: Illegal message header received.'),nlErr,!,fail.
readPortMessageBody(message(_G260,_G261),_G264):-readStreamLine(_G260,_G264).
readPortMessageBody(ping(_G290,_G291),_G294):-answerPingOnStream(_G290,_G291),!,fail.
prim_sendPort(_G445,_G446,_G447,_G448,_G449):-freeze(_G448,blocked_prim_sendPort(_G445,_G446,_G447,_G448,_G449)).
blocked_prim_sendPort(_G31,_G32,_G33,_G34,_G35):-user:derefAll(_G31,_G38),user:derefRoot(_G32,_G44),prim_sendPortExec(_G38,_G44,_G33,_G34,_G35).
prim_sendPortExec(_G99,'Ports.internalPort'(_G94,0,_G96,_G97),'Prelude.success',_G102,_G103):-!,add2Stream(_G97,_G99),_G102=_G103.
prim_sendPortExec('Ports.SP_Put'(_G150),'Ports.internalPort'(_G152,-1,_G154,_G155),'Prelude.success',_G160,_G161):-!,user:waitUntilGround(_G150,_G160,_G161),writeChars(_G155,_G150),nl(_G155),flush_output(_G155),ifTracePort((writeErr('TRACEPORTS: SP_Put: '),writeChars(user_error,_G150),nlErr)).
prim_sendPortExec('Ports.SP_GetLine'(_G238),'Ports.internalPort'(_G240,-1,_G242,_G243),'Prelude.success',_G248,_G249):-!,readStreamLine(_G240,_G252),map2M(basics:char_int,_G258,_G252),ifTracePort((writeErr('TRACEPORTS: SP_Get: '),writeChars(user_error,_G258),nlErr)),user:constrEq(_G258,_G238,_G276,_G248,_G249).
prim_sendPortExec('Ports.SP_GetChar'(_G328),'Ports.internalPort'(_G330,-1,_G332,_G333),'Prelude.success',_G338,_G339):-!,get_code(_G330,_G342),char_int(_G342,_G345),ifTracePort((writeErr('TRACEPORTS: SP_GetChar: '),writeErr(_G345),nlErr)),user:constrEq(_G345,_G328,_G361,_G338,_G339).
prim_sendPortExec('Ports.SP_EOF'(_G413),'Ports.internalPort'(_G415,-1,_G417,_G418),'Prelude.success',_G423,_G424):-!, (atEndOfStream(_G415)->_G428='Prelude.True';_G428='Prelude.False'),ifTracePort((writeErr('TRACEPORTS: SP_EOF: '),writeErr(_G428),nlErr)),user:constrEq(_G413,_G428,_G454,_G423,_G424).
prim_sendPortExec('Ports.SP_Close','Ports.internalPort'(_G31,-1,_G33,_G34),'Prelude.success',_G39,_G40):-!,close(_G34),close(_G31),_G39=_G40.
prim_sendPortExec(_G96,'Ports.internalPort'(_G91,-1,_G93,_G94),'Prelude.success',_G99,_G100):-!,writeErr('ERROR: wrong message received by stream port: '),writeErr(_G96),nlErr,_G99=_G100.
prim_sendPortExec(_G159,'Ports.internalPort'(_G154,_G155,_G156,_G157),'Prelude.success',_G162,_G163):-string2Atom(_G154,_G166),on_exception(_G183,send2Socket(_G166,_G155,_G156,_G159), (write('ERROR: send to port '),write(_G156),write(@),write(_G166),write(:),nl, (printError(_G183)->true;true))),_G162=_G163.
add2Stream(_G255,_G256):-var(_G255),!,_G255=[_G256|_G261].
add2Stream([],_G294):-write('ERROR: send: stream has not a free variable at the end'),nl.
add2Stream([_G322|_G323],_G326):-add2Stream(_G323,_G326).
prim_ping(_G359,'Ports.internalPort'(_G352,0,_G354,_G355),'Prelude.Just'(0)):-!.
prim_ping(_G395,'Ports.internalPort'(_G388,-1,_G390,_G391),'Prelude.Just'(0)):-!.
prim_ping(_G429,'Ports.internalPort'(_G424,_G425,_G426,_G427),_G431):-string2Atom(_G424,_G434),on_exception(_G445,ping2SocketPort(_G434,_G425,_G426,_G429,_G431),_G431='Prelude.Nothing').
ping2SocketPort(_G31,_G32,_G33,_G34,_G35):-connect2socket(_G31,_G32,_G39,_G40),number_codes(_G33,_G43),putChars(_G40,[80|_G43]),put_code(_G40,10),flush_output(_G40),getElapsedTime(_G56),ifTracePort((writeErr('TRACEPORTS: Ping to "'),writeErr(_G32),writeErr(/),writeErr(_G33),writeErr(@),writeErr(_G31),writeErr('"'),nlErr)),waitForInputDataOnStreams([_G39],_G34,_G100), (_G100=0->readStreamLine(_G39,_G106),closeSocketStream(_G39,_G40),getElapsedTime(_G111),_G116 is _G111-_G56,ifTracePort((writeErr('TRACEPORTS: Ping answer received in '),writeErr(_G116),writeErr(' ms'),nlErr)),_G35='Prelude.Just'(_G116);ifTracePort((writeErr('TRACEPORTS: No ping answer received'),nlErr)),_G35='Prelude.Nothing').
prim_connectPortAtSocket(_G237,_G238,_G239,'Ports.internalPort'(_G232,_G233,_G234,'Prelude.()')):-user:derefRoot(_G237,_G233),user:derefRoot(_G238,_G234),user:derefAll(_G239,_G232),!.
send2Socket(_G298,_G299,_G300,_G301):-connect2socket(_G298,_G299,_G305,_G306),number_codes(_G300,_G309),putChars(_G306,[78|_G309]),put_code(_G306,10),show4sending(_G301,_G321,_G322),writeChars(_G306,_G321),put_code(_G306,10),flush_output(_G306),ifTracePort((writeErr('TRACEPORTS: Sent to "'),writeErr(_G299),writeErr(/),writeErr(_G300),writeErr(@),writeErr(_G298),writeErr('": '),writeChars(user_error,_G321),nlErr)),rev(_G322,_G376),receive_extvar_bindings(_G305,_G376).
receive_extvar_bindings(_G440,[]):-!,ifTracePort((writeErr('TRACEPORTS: Closing send-connection'),nlErr)),close(_G440).
receive_extvar_bindings(_G31,_G32):-readStreamLine(_G31,_G35),_G35=[86|_G38],number_codes(_G43,_G38),readStreamLine(_G31,_G47), (deleteIndexVariable(_G43,_G32,_G51,_G52)->true;writeErr('ERROR: Illegal binding for logical variable received:'),nlErr,_G52=_G32), (parse_received_message(nostream,nostream,_G47,_G74)->ifTracePort((writeErr('TRACEPORTS: Received binding for variable '),writeErr(_G43),writeErr(': '),putChars(user_error,_G47),nlErr));writeErr('ERROR: Illegal message received (ignored): '),putChars(user_error,_G47),nlErr),!,_G51=_G74,receive_extvar_bindings(_G31,_G52).
show4sending(_G167,_G168,_G169):-numberVars(_G167,_G172,[],_G169),show_term(_G172,qualified,_G168,[]).
getVariableIndex(_G215,[ (_G209,_G210)|_G213],_G210):-_G215==_G209,!.
getVariableIndex(_G253,[_G250|_G251],_G255):-getVariableIndex(_G253,_G251,_G255).
getIndexVariable(_G287,[ (_G286,_G287)|_G290],_G286):-!.
getIndexVariable(_G324,[_G321|_G322],_G326):-getIndexVariable(_G324,_G322,_G326).
deleteIndexVariable(_G358,[ (_G357,_G358)|_G361],_G357,_G361):-!.
deleteIndexVariable(_G403,[_G397|_G398],_G405,[_G397|_G401]):-deleteIndexVariable(_G403,_G398,_G405,_G401).
parse_received_message(_G30,_G31,_G32,_G33):-readTerm(_G32,qualified,_G37,_G38),skipWhiteSpace(_G37,[]),extractVars(_G38,_G33,[],_G46),send_extvar_bindings(_G46,_G49,_G31),closeStreamAfterDones(_G49,_G30,_G31).
send_extvar_bindings([],[],_G93).
send_extvar_bindings([ (_G114,_G115)|_G118],[_G120|_G121],_G125):-send_extvar_binding(_G125,_G114,_G115,_G120),send_extvar_bindings(_G118,_G121,_G125).
closeStreamAfterDones([],nostream,_G166):-!.
closeStreamAfterDones([],_G194,_G195):-ifTracePort((writeErr('TRACEPORTS: Closing receive-connection'),nlErr)),closeSocketStream(_G194,_G195).
closeStreamAfterDones([_G235|_G236],_G239,_G240):-closeStreamAfterDones(_G235,_G236,_G239,_G240).
closeStreamAfterDones(_G414,_G415,_G416,_G417):-freeze(_G414,blocked_closeStreamAfterDones(_G414,_G415,_G416,_G417)).
blocked_closeStreamAfterDones(_G448,_G449,_G450,_G451):-closeStreamAfterDones(_G449,_G450,_G451).
prim_openProcessPort(_G42,'Ports.internalPort'(_G37,-1,0,_G40)):-user:derefAll(_G42,_G46),string2Atom(_G46,_G52),execCommand(_G52,_G40,_G37,_G57).
prim_choiceSPEP(_G232,_G233,_G234,_G235,_G236):-freeze(_G233,freeze(_G235,blocked_prim_choiceSPEP(_G232,_G233,_G234,_G235,_G236))).
blocked_prim_choiceSPEP(_G274,_G275,_G276,_G277,_G278):-user:derefRoot(_G274,_G281),prim_choiceSPEP_exec(_G281,_G275,_G276,_G277,_G278).
prim_choiceSPEP_exec(_G330,share(_G328),_G332,_G333,_G334):-!,get_mutable(_G336,_G328), (_G336='$eval'(_G339)->_G333=_G334,_G332='Prelude.Right'(_G339);prim_choiceSPEP_exec(_G330,_G336,_G360,_G333,_G334), (_G360='Prelude.Left'(_G364)->_G332=_G360;_G360='Prelude.Right'(_G375), (compileWithSharing(variable)->user:propagateShare(_G375,_G383);_G375=_G383),_G332='Prelude.Right'(_G383),update_mutable('$eval'(_G383),_G328))).
prim_choiceSPEP_exec(_G50,[_G43|_G44],'Prelude.Right'([_G43|_G44]),_G51,_G52):-!,_G51=_G52.
prim_choiceSPEP_exec('Ports.internalPort'(_G88,-1,_G90,_G91),[],_G95,_G96,_G97):-readStreamLine(_G88,_G100),map2M(basics:char_int,_G106,_G100),ifTracePort((writeErr('TRACEPORTS: SP_Get: '),writeChars(user_error,_G106),nlErr)),_G95='Prelude.Left'(_G106),_G96=_G97,!.
prim_choiceSPEP_exec(_G180,'Ports.basicServerLoop'(_G178),_G182,_G183,_G184):-_G178='Ports.internalPort'(_G186,_G187,_G188,_G189),checkIncomingPortStreams(_G188,_G195,_G196),!,readStreamLine(_G195,_G199), (compileWithSharing(function)->makeShare('Ports.basicServerLoop'(_G178),_G206);_G206='Ports.basicServerLoop'(_G178)), (parse_received_message(_G195,_G196,_G199,_G222)->ifTracePort((writeErr('TRACEPORTS: Received message: '),writeErr(_G222),nlErr)),_G182='Prelude.Right'([_G222|_G206]);writeErr('ERROR: Illegal message received (ignored): '),putChars(user_error,_G199),nlErr,prim_choiceSPEP_exec(_G180,_G206,_G182,_G183,_G184)),_G183=_G184,!.
prim_choiceSPEP_exec('Ports.internalPort'(_G43,-1,_G45,_G46),'Ports.basicServerLoop'(_G48),_G52,_G53,_G54):-_G48='Ports.internalPort'(_G56,_G57,_G58,_G59),waitForSocketOrInputStreams(_G59,_G68,_G69,_G70,[_G43],_G72), (compileWithSharing(function)->makeShare('Ports.basicServerLoop'(_G48),_G79);_G79='Ports.basicServerLoop'(_G48)), (_G68=no->readStreamLine(_G43,_G96),map2M(basics:char_int,_G102,_G96),ifTracePort((writeErr('TRACEPORTS: SP_Get: '),writeChars(user_error,_G102),nlErr)),_G52='Prelude.Left'(_G102);ifTracePort((writeErr('TRACEPORTS: Connection to client: '),writeErr(_G68),nlErr)), (readPortMessage(_G58,_G69,_G70,_G150)-> (parse_received_message(_G69,_G70,_G150,_G155)->ifTracePort((writeErr('TRACEPORTS: Received message: '),writeErr(_G155),nlErr)),_G52='Prelude.Right'([_G155|_G79]);writeErr('ERROR: Illegal message received (ignored): '),putChars(user_error,_G150),nlErr,prim_choiceSPEP_exec('Ports.internalPort'(_G43,-1,_G190,_G46),_G79,_G52,_G53,_G54));prim_choiceSPEP_exec('Ports.internalPort'(_G43,-1,_G216,_G46),_G79,_G52,_G53,_G54))),_G53=_G54,!.
numberVars(_G287,'VAR'(_G285),_G289,_G290):-var(_G287),!, (getVariableIndex(_G287,_G289,_G285)->_G290=_G289;length(_G289,_G285),_G290=[ (_G287,_G285)|_G289]).
numberVars(_G357,_G358,_G359,_G360):-isShowableArg(_G357),_G357=..[_G364|_G365],numberVarsList(_G365,_G371,_G359,_G360),_G358=..[_G364|_G371].
numberVarsList([],[],_G421,_G421).
numberVarsList([_G447|_G448],[_G450|_G451],_G455,_G456):-numberVars(_G447,_G450,_G455,_G461),numberVarsList(_G448,_G451,_G461,_G456).
extractVars(_G43,_G43,_G45,_G45):-var(_G43),!.
extractVars('VAR'(_G82),_G85,_G86,_G87):-!, (getIndexVariable(_G82,_G86,_G85)->_G87=_G86;_G87=[ (_G85,_G82)|_G86]).
extractVars(_G143,_G144,_G145,_G146):-_G143=..[_G148|_G149],extractVarsList(_G149,_G155,_G145,_G146),_G144=..[_G148|_G155].
extractVarsList([],[],_G202,_G202).
extractVarsList([_G228|_G229],[_G231|_G232],_G236,_G237):-extractVars(_G228,_G231,_G236,_G242),extractVarsList(_G229,_G232,_G242,_G237).
prim_after(_G390,_G391,_G392,_G393):-freeze(_G392,blocked_prim_after(_G390,_G391,_G392,_G393)).
blocked_prim_after(_G424,_G425,_G426,_G426):-raise_exception('Ports.after not implemented!').
