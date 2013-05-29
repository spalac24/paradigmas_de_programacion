
import Prelude (lex,readsPrec,fromEnum,undefined,toEnum)
import qualified Prelude as P (Read(..),(+),(-),Integer) 
import qualified System.Time as T

instance P.Read ClockTime where 
  readsPrec p s0 = [(ClockTime (C1_1 (toData i)),s2) | 
                     ("CTime",s1) <- lex s0,
                     (i::P.Integer,s2) <- readsPrec p s1]

instance P.Read CalendarTime where 
  readsPrec p s  = [(CalendarTime (C 1 (toData i1) (toData i2) (toData i3) (toData i4) 
                                       [toData i5,toData i6,toData i7]),s7) 
                   | ("CalendarTime",s0) <- lex s,
                     (i1::P.Integer,s1) <- readsPrec p s0,
                     (i2::P.Integer,s2) <- readsPrec p s1,
                     (i3::P.Integer,s3) <- readsPrec p s2,
                     (i4::P.Integer,s4) <- readsPrec p s3,
                     (i5::P.Integer,s5) <- readsPrec p s4,
                     (i6::P.Integer,s6) <- readsPrec p s5,
                     (i7::P.Integer,s7) <- readsPrec p s6]


-- these need to be in league with ExternalFunctionsTime

instance Convert T.CalendarTime where
  toData (T.CalendarTime y m d h min s _ _ _ _ tz _) = 
          C 1 (toData y) 
              (toData (fromEnum m P.+ 1))
              (toData d)
              (toData h)
              [toData min,
               toData s,
               toData tz]
  fromData (C _ y m d h [min,s,tz]) = 
             T.CalendarTime (fromData y) 
                          (toEnum (fromData m P.- 1))
                          (fromData d)
                          (fromData h)
                          (fromData min)
                          (fromData s)
                          0 undefined undefined undefined
                          (fromData tz)
                          undefined

instance Convert T.ClockTime where
  toData (T.TOD i _) = C1_1 (toData i)
  fromData (C1_1 i)  = T.TOD (fromData i) 0

getClockTime :: Debug (IO ClockTime)
getClockTime = ioFunc0 "getClockTime" getNextExtVal

prim_toCalendarTime :: ClockTime -> Debug (IO CalendarTime)
prim_toCalendarTime = ioFunc1 "prim_toCalendarTime" (\_ -> getNextExtVal)

prim_toUTCTime :: ClockTime -> Debug CalendarTime
prim_toUTCTime = 
  trace1 T.toUTCTime

prim_toClockTime :: CalendarTime -> Debug ClockTime
prim_toClockTime = 
  trace1 T.toClockTime 
