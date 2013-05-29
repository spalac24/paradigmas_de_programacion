{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Float (d_OP_plus_dot, d_OP_minus_dot, d_OP_star_dot, d_OP_slash_dot, d_C_i2f, d_C_truncate, d_C_round, d_C_sqrt, d_C_log, d_C_exp, d_C_sin, d_C_cos, d_C_tan, d_C_atan) where

import Basics
import qualified Curry_Prelude
import qualified Curry_Prelude as CP


d_OP_plus_dot :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_OP_plus_dot x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_Float_plus) x2 x3500) x1 x3500

d_OP_minus_dot :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_OP_minus_dot x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_Float_minus) x2 x3500) x1 x3500

d_OP_star_dot :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_OP_star_dot x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_Float_times) x2 x3500) x1 x3500

d_OP_slash_dot :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_OP_slash_dot x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_Float_div) x2 x3500) x1 x3500

d_C_i2f :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Float
d_C_i2f x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_i2f x1 x3500

d_C_truncate :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Int
d_C_truncate x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_truncate x1 x3500

d_C_round :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Int
d_C_round x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_round x1 x3500

d_C_sqrt :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_sqrt x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_sqrt x1 x3500

d_C_log :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_log x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_log x1 x3500

d_C_exp :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_exp x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_exp x1 x3500

d_C_sin :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_sin x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_sin x1 x3500

d_C_cos :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_cos x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_cos x1 x3500

d_C_tan :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_tan x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_tan x1 x3500

d_C_atan :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_atan x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_atan x1 x3500

d_C_prim_Float_plus :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_Float_plus x1 x2 x3500 = external_d_C_prim_Float_plus x1 x2 x3500

d_C_prim_Float_minus :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_Float_minus x1 x2 x3500 = external_d_C_prim_Float_minus x1 x2 x3500

d_C_prim_Float_times :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_Float_times x1 x2 x3500 = external_d_C_prim_Float_times x1 x2 x3500

d_C_prim_Float_div :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_Float_div x1 x2 x3500 = external_d_C_prim_Float_div x1 x2 x3500

d_C_prim_i2f :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_i2f x1 x3500 = external_d_C_prim_i2f x1 x3500

d_C_prim_truncate :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Int
d_C_prim_truncate x1 x3500 = external_d_C_prim_truncate x1 x3500

d_C_prim_round :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Int
d_C_prim_round x1 x3500 = external_d_C_prim_round x1 x3500

d_C_prim_sqrt :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_sqrt x1 x3500 = external_d_C_prim_sqrt x1 x3500

d_C_prim_log :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_log x1 x3500 = external_d_C_prim_log x1 x3500

d_C_prim_exp :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_exp x1 x3500 = external_d_C_prim_exp x1 x3500

d_C_prim_sin :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_sin x1 x3500 = external_d_C_prim_sin x1 x3500

d_C_prim_cos :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_cos x1 x3500 = external_d_C_prim_cos x1 x3500

d_C_prim_tan :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_tan x1 x3500 = external_d_C_prim_tan x1 x3500

d_C_prim_atan :: Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.C_Float
d_C_prim_atan x1 x3500 = external_d_C_prim_atan x1 x3500
external_d_C_prim_Float_plus :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_plus y x _ =
  toCurry ((fromCurry x + fromCurry y) :: Float)

external_d_C_prim_Float_minus :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_minus y x _ =
  toCurry ((fromCurry x - fromCurry y) :: Float)

external_d_C_prim_Float_times :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_times y x _ =
  toCurry ((fromCurry x * fromCurry y) :: Float)

external_d_C_prim_Float_div :: CP.C_Float -> CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_Float_div y x _ =
  toCurry ((fromCurry x / fromCurry y) :: Float)

external_d_C_prim_i2f :: CP.C_Int -> ConstStore -> CP.C_Float
external_d_C_prim_i2f x _ = toCurry (fromInteger (fromCurry x) :: Float)

external_d_C_prim_truncate :: CP.C_Float -> ConstStore -> CP.C_Int
external_d_C_prim_truncate x _ = toCurry (truncate (fromCurry x :: Float) :: Int)

external_d_C_prim_round :: CP.C_Float -> ConstStore -> CP.C_Int
external_d_C_prim_round x _ = toCurry (round (fromCurry x :: Float) :: Int)

external_d_C_prim_sqrt :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_sqrt x _ = toCurry (sqrt (fromCurry x :: Float))

external_d_C_prim_log :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_log x _ = toCurry (log (fromCurry x :: Float))

external_d_C_prim_exp :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_exp x _ = toCurry (exp (fromCurry x :: Float))

external_d_C_prim_sin :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_sin x _ = toCurry (sin (fromCurry x :: Float))

external_d_C_prim_cos :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_cos x _ = toCurry (cos (fromCurry x :: Float))

external_d_C_prim_tan :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_tan x _ = toCurry (tan (fromCurry x :: Float))

external_d_C_prim_atan :: CP.C_Float -> ConstStore -> CP.C_Float
external_d_C_prim_atan x _ = toCurry (atan (fromCurry x :: Float))


