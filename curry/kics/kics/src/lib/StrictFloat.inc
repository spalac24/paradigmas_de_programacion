
import qualified Prelude as P ((+),(-),(*),(/),(<),(>),(<=),(>=),(.),
			       fromInteger,truncate, round, sqrt, log, exp,
			       sin,cos,tan,atan)

prim_Float_plus ::  Float -> Float -> (Debug Float) 
prim_Float_plus = trace2 ((P.+) :: P.Float -> P.Float -> P.Float)

prim_Float_minus ::  Float -> Float -> (Debug Float) 
prim_Float_minus = trace2 ((P.-) :: P.Float -> P.Float -> P.Float)

prim_Float_times ::  Float -> Float -> (Debug Float) 
prim_Float_times = trace2 ((P.*) :: P.Float -> P.Float -> P.Float)

prim_Float_divide ::  Float -> Float -> (Debug Float) 
prim_Float_divide = trace2 ((P./) :: P.Float -> P.Float -> P.Float)

prim_Float_lt ::  Float -> Float -> (Debug Bool) 
prim_Float_lt = trace2 ((P.<) :: P.Float -> P.Float -> P.Bool)

prim_Float_gt ::  Float -> Float -> (Debug Bool) 
prim_Float_gt = trace2 ((P.>) :: P.Float -> P.Float -> P.Bool)

prim_Float_leq ::  Float -> Float -> (Debug Bool) 
prim_Float_leq = trace2 ((P.<=) :: P.Float -> P.Float -> P.Bool)

prim_Float_geq ::  Float -> Float -> (Debug Bool) 
prim_Float_geq = trace2 ((P.>=) :: P.Float -> P.Float -> P.Bool)

prim_i2f ::  Int -> (Debug Float) 
prim_i2f = trace1 (P.fromInteger :: P.Integer -> P.Float)

prim_truncate ::  Float -> (Debug Int) 
prim_truncate = trace1 (P.truncate :: P.Float -> P.Integer)

prim_round ::  Float -> (Debug Int) 
prim_round = trace1 (P.round :: P.Float -> P.Integer)

prim_sqrt ::  Float -> (Debug Float) 
prim_sqrt = trace1 (P.sqrt :: P.Float -> P.Float)

prim_log ::  Float -> (Debug Float) 
prim_log = trace1 (P.log :: P.Float -> P.Float)

prim_exp ::  Float -> (Debug Float) 
prim_exp = trace1 (P.exp :: P.Float -> P.Float)

prim_sin ::  Float -> (Debug Float) 
prim_sin = trace1 (P.sin :: P.Float -> P.Float)

prim_cos ::  Float -> (Debug Float) 
prim_cos = trace1 (P.cos :: P.Float -> P.Float)

prim_tan ::  Float -> (Debug Float) 
prim_tan = trace1 (P.tan :: P.Float -> P.Float)

prim_atan ::  Float -> (Debug Float) 
prim_atan = trace1 (P.atan :: P.Float -> P.Float)

