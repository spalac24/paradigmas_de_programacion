:-module(prim_time,[prim_getClockTime/1,prim_toCalendarTime/2,prim_toUTCTime/2,prim_toClockTime/2]).
:-style_check(-singleton).
:-use_module('../prologbasics').
prim_getClockTime('Time.CTime'(_G461)):-currentClockTime(_G461).
prim_toCalendarTime('Time.CTime'(_G1),'Time.CalendarTime'(_G3,_G4,_G5,_G6,_G7,_G8,_G9)):-clocktime2localtime(_G1,_G3,_G4,_G5,_G6,_G7,_G8,_G9).
prim_toUTCTime('Time.CTime'(_G44),'Time.CalendarTime'(_G46,_G47,_G48,_G49,_G50,_G51,0)):-clocktime2utctime(_G44,_G46,_G47,_G48,_G49,_G50,_G51).
prim_toClockTime('Time.CalendarTime'(_G86,_G87,_G88,_G89,_G90,_G91,_G92),'Time.CTime'(_G94)):-date2clocktime(_G86,_G87,_G88,_G89,_G90,_G91,_G92,_G94).
