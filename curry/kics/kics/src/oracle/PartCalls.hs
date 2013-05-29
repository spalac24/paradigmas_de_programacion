module PartCalls where

import DebuggerMonad
import Term

pc1 :: (ShowTerm x1,ShowTerm x2) => Term.Term -> (x1 -> DebuggerMonad.Debug x2) -> (DebuggerMonad.Func x1 x2) 
pc1 (Term n xs) f =
  Func (c []) (\ x1 ->
    f x1)
 where c = Term n . (xs++)

pc2 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3) => Term.Term -> (x1 -> x2 -> DebuggerMonad.Debug x3) -> (DebuggerMonad.Func x1 (DebuggerMonad.Func x2 x3)) 
pc2 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 ->
    f x1 x2)))
 where c = Term n . (xs++)

pc3
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4) => Term.Term -> (x1 -> x2 -> x3 -> DebuggerMonad.Debug x4) 
 -> (DebuggerMonad.Func x1 (DebuggerMonad.Func x2 (DebuggerMonad.Func x3 x4))) 
pc3 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 ->
    f x1 x2 x3)))))
 where c = Term n . (xs++)

pc4
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5) => Term.Term -> (x1 -> x2 -> x3 -> x4 -> DebuggerMonad.Debug x5) 
 -> (DebuggerMonad.Func x1 (DebuggerMonad.Func x2 (DebuggerMonad.Func x3 (DebuggerMonad.Func x4 x5)))) 
pc4 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 ->
    f x1 x2 x3 x4)))))))
 where c = Term n . (xs++)

pc5
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6) => Term.Term -> (x1 -> x2 -> x3 -> x4 -> x5 -> DebuggerMonad.Debug x6) 
 -> (DebuggerMonad.Func x1 (DebuggerMonad.Func x2 (DebuggerMonad.Func x3 (DebuggerMonad.Func x4 (DebuggerMonad.Func x5 x6))))) 
pc5 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 ->
    f x1 x2 x3 x4 x5)))))))))
 where c = Term n . (xs++)

pc6
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> DebuggerMonad.Debug x7) 
 -> (DebuggerMonad.Func x1 (DebuggerMonad.Func x2 (DebuggerMonad.Func x3 (DebuggerMonad.Func x4 (DebuggerMonad.Func x5 (DebuggerMonad.Func x6 x7)))))) 
pc6 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 ->
    f x1 x2 x3 x4 x5 x6)))))))))))
 where c = Term n . (xs++)

pc7
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> DebuggerMonad.Debug x8) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2 (DebuggerMonad.Func x3 (DebuggerMonad.Func x4 (DebuggerMonad.Func x5 (DebuggerMonad.Func x6 (DebuggerMonad.Func x7 x8))))))) 
pc7 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 ->
    f x1 x2 x3 x4 x5 x6 x7)))))))))))))
 where c = Term n . (xs++)

pc8
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> DebuggerMonad.Debug x9) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3 (DebuggerMonad.Func x4 (DebuggerMonad.Func x5 (DebuggerMonad.Func x6 (DebuggerMonad.Func x7 (DebuggerMonad.Func x8 x9)))))))) 
pc8 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 ->
    f x1 x2 x3 x4 x5 x6 x7 x8)))))))))))))))
 where c = Term n . (xs++)

pc9
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> DebuggerMonad.Debug x10) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5 (DebuggerMonad.Func x6 (DebuggerMonad.Func x7 (DebuggerMonad.Func x8 (DebuggerMonad.Func x9 x10))))))))) 
pc9 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9)))))))))))))))))
 where c = Term n . (xs++)

pc10
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> DebuggerMonad.Debug x11) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6 (DebuggerMonad.Func x7 (DebuggerMonad.Func x8 (DebuggerMonad.Func x9 (DebuggerMonad.Func x10 x11)))))))))) 
pc10 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)))))))))))))))))))
 where c = Term n . (xs++)

pc11
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11
    ,ShowTerm x12) => Term.Term -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> DebuggerMonad.Debug x12) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7 (DebuggerMonad.Func x8 (DebuggerMonad.Func x9 (DebuggerMonad.Func x10 (DebuggerMonad.Func x11 x12))))))))))) 
pc11 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)))))))))))))))))))))
 where c = Term n . (xs++)

pc12
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13) => Term.Term -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> DebuggerMonad.Debug x13) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8 (DebuggerMonad.Func x9 (DebuggerMonad.Func x10 (DebuggerMonad.Func x11 (DebuggerMonad.Func x12 x13)))))))))))) 
pc12 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)))))))))))))))))))))))
 where c = Term n . (xs++)

pc13
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14) => Term.Term -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> DebuggerMonad.Debug x14) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9 (DebuggerMonad.Func x10 (DebuggerMonad.Func x11 (DebuggerMonad.Func x12 (DebuggerMonad.Func x13 x14))))))))))))) 
pc13 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)))))))))))))))))))))))))
 where c = Term n . (xs++)

pc14
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> DebuggerMonad.Debug x15) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10 (DebuggerMonad.Func x11 (DebuggerMonad.Func x12 (DebuggerMonad.Func x13 (DebuggerMonad.Func x14 x15)))))))))))))) 
pc14 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc15
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> DebuggerMonad.Debug x16) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12 (DebuggerMonad.Func x13 (DebuggerMonad.Func x14 (DebuggerMonad.Func x15 x16))))))))))))))) 
pc15 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc16
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> DebuggerMonad.Debug x17) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13 (DebuggerMonad.Func x14 (DebuggerMonad.Func x15 (DebuggerMonad.Func x16 x17)))))))))))))))) 
pc16 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc17
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> DebuggerMonad.Debug x18) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14 (DebuggerMonad.Func x15 (DebuggerMonad.Func x16 (DebuggerMonad.Func x17 x18))))))))))))))))) 
pc17 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc18
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> DebuggerMonad.Debug x19) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15 (DebuggerMonad.Func x16 (DebuggerMonad.Func x17 (DebuggerMonad.Func x18 x19)))))))))))))))))) 
pc18 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18)))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc19
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> DebuggerMonad.Debug x20) 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16 (DebuggerMonad.Func x17 (DebuggerMonad.Func x18 (DebuggerMonad.Func x19 x20))))))))))))))))))) 
pc19 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19)))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc20
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20
      -> DebuggerMonad.Debug x21) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17 (DebuggerMonad.Func x18 (DebuggerMonad.Func x19 (DebuggerMonad.Func x20 x21)))))))))))))))))))) 
pc20 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc21
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21
      -> DebuggerMonad.Debug x22) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18 (DebuggerMonad.Func x19 (DebuggerMonad.Func x20 (DebuggerMonad.Func x21 x22))))))))))))))))))))) 
pc21 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21)))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc22
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22
    ,ShowTerm x23) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22
      -> DebuggerMonad.Debug x23) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20 (DebuggerMonad.Func x21 (DebuggerMonad.Func x22 x23)))))))))))))))))))))) 
pc22 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22)))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc23
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23
    ,ShowTerm x24) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23
      -> DebuggerMonad.Debug x24) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21 (DebuggerMonad.Func x22 (DebuggerMonad.Func x23 x24))))))))))))))))))))))) 
pc23 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23)))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc24
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> DebuggerMonad.Debug x25) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22 (DebuggerMonad.Func x23 (DebuggerMonad.Func x24 x25)))))))))))))))))))))))) 
pc24 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24)))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc25
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> DebuggerMonad.Debug x26) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23 (DebuggerMonad.Func x24 (DebuggerMonad.Func x25 x26))))))))))))))))))))))))) 
pc25 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25)))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc26
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26,ShowTerm x27) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> x26 -> DebuggerMonad.Debug x27) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23
                                                  (DebuggerMonad.Func x24 (DebuggerMonad.Func x25 (DebuggerMonad.Func x26 x27)))))))))))))))))))))))))) 
pc26 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 -> do
  let sx25 = showTerm x25
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25]) (\ x26 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26)))))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc27
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26,ShowTerm x27,ShowTerm x28) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> x26 -> x27 -> DebuggerMonad.Debug x28) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23
                                                  (DebuggerMonad.Func x24
                                                    (DebuggerMonad.Func x25 (DebuggerMonad.Func x26 (DebuggerMonad.Func x27 x28))))))))))))))))))))))))))) 
pc27 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 -> do
  let sx25 = showTerm x25
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25]) (\ x26 -> do
  let sx26 = showTerm x26
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26]) (\ x27 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27)))))))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc28
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26,ShowTerm x27,ShowTerm x28,ShowTerm x29) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> x26 -> x27 -> x28 -> DebuggerMonad.Debug x29) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23
                                                  (DebuggerMonad.Func x24
                                                    (DebuggerMonad.Func x25
                                                      (DebuggerMonad.Func x26 (DebuggerMonad.Func x27 (DebuggerMonad.Func x28 x29)))))))))))))))))))))))))))) 
pc28 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 -> do
  let sx25 = showTerm x25
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25]) (\ x26 -> do
  let sx26 = showTerm x26
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26]) (\ x27 -> do
  let sx27 = showTerm x27
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27]) (\ x28 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28)))))))))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc29
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26,ShowTerm x27,ShowTerm x28,ShowTerm x29,ShowTerm x30) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> x26 -> x27 -> x28 -> x29 -> DebuggerMonad.Debug x30) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23
                                                  (DebuggerMonad.Func x24
                                                    (DebuggerMonad.Func x25
                                                      (DebuggerMonad.Func x26
                                                        (DebuggerMonad.Func x27
                                                          (DebuggerMonad.Func x28 (DebuggerMonad.Func x29 x30))))))))))))))))))))))))))))) 
pc29 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 -> do
  let sx25 = showTerm x25
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25]) (\ x26 -> do
  let sx26 = showTerm x26
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26]) (\ x27 -> do
  let sx27 = showTerm x27
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27]) (\ x28 -> do
  let sx28 = showTerm x28
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27,sx28]) (\ x29 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

pc30
 :: (ShowTerm x1,ShowTerm x2,ShowTerm x3,ShowTerm x4,ShowTerm x5,ShowTerm x6,ShowTerm x7,ShowTerm x8,ShowTerm x9,ShowTerm x10,ShowTerm x11,ShowTerm x12
    ,ShowTerm x13,ShowTerm x14,ShowTerm x15,ShowTerm x16,ShowTerm x17,ShowTerm x18,ShowTerm x19,ShowTerm x20,ShowTerm x21,ShowTerm x22,ShowTerm x23,ShowTerm x24
    ,ShowTerm x25,ShowTerm x26,ShowTerm x27,ShowTerm x28,ShowTerm x29,ShowTerm x30,ShowTerm x31) => Term.Term 
 -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24
      -> x25 -> x26 -> x27 -> x28 -> x29 -> x30 -> DebuggerMonad.Debug x31) 
 
 -> (DebuggerMonad.Func x1
      (DebuggerMonad.Func x2
        (DebuggerMonad.Func x3
          (DebuggerMonad.Func x4
            (DebuggerMonad.Func x5
              (DebuggerMonad.Func x6
                (DebuggerMonad.Func x7
                  (DebuggerMonad.Func x8
                    (DebuggerMonad.Func x9
                      (DebuggerMonad.Func x10
                        (DebuggerMonad.Func x11
                          (DebuggerMonad.Func x12
                            (DebuggerMonad.Func x13
                              (DebuggerMonad.Func x14
                                (DebuggerMonad.Func x15
                                  (DebuggerMonad.Func x16
                                    (DebuggerMonad.Func x17
                                      (DebuggerMonad.Func x18
                                        (DebuggerMonad.Func x19
                                          (DebuggerMonad.Func x20
                                            (DebuggerMonad.Func x21
                                              (DebuggerMonad.Func x22
                                                (DebuggerMonad.Func x23
                                                  (DebuggerMonad.Func x24
                                                    (DebuggerMonad.Func x25
                                                      (DebuggerMonad.Func x26
                                                        (DebuggerMonad.Func x27
                                                          (DebuggerMonad.Func x28
                                                            (DebuggerMonad.Func x29 (DebuggerMonad.Func x30 x31)))))))))))))))))))))))))))))) 
pc30 (Term n xs) f =
  Func (c []) (\ x1 -> do
  let sx1 = showTerm x1
  return (Func (c [sx1]) (\ x2 -> do
  let sx2 = showTerm x2
  return (Func (c [sx1,sx2]) (\ x3 -> do
  let sx3 = showTerm x3
  return (Func (c [sx1,sx2,sx3]) (\ x4 -> do
  let sx4 = showTerm x4
  return (Func (c [sx1,sx2,sx3,sx4]) (\ x5 -> do
  let sx5 = showTerm x5
  return (Func (c [sx1,sx2,sx3,sx4,sx5]) (\ x6 -> do
  let sx6 = showTerm x6
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6]) (\ x7 -> do
  let sx7 = showTerm x7
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7]) (\ x8 -> do
  let sx8 = showTerm x8
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8]) (\ x9 -> do
  let sx9 = showTerm x9
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9]) (\ x10 -> do
  let sx10 = showTerm x10
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10]) (\ x11 -> do
  let sx11 = showTerm x11
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11]) (\ x12 -> do
  let sx12 = showTerm x12
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12]) (\ x13 -> do
  let sx13 = showTerm x13
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13]) (\ x14 -> do
  let sx14 = showTerm x14
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14]) (\ x15 -> do
  let sx15 = showTerm x15
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15]) (\ x16 -> do
  let sx16 = showTerm x16
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16]) (\ x17 -> do
  let sx17 = showTerm x17
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17]) (\ x18 -> do
  let sx18 = showTerm x18
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18]) (\ x19 -> do
  let sx19 = showTerm x19
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19]) (\ x20 -> do
  let sx20 = showTerm x20
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20]) (\ x21 -> do
  let sx21 = showTerm x21
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21]) (\ x22 -> do
  let sx22 = showTerm x22
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22]) (\ x23 -> do
  let sx23 = showTerm x23
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23]) (\ x24 -> do
  let sx24 = showTerm x24
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24]) (\ x25 -> do
  let sx25 = showTerm x25
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25]) (\ x26 -> do
  let sx26 = showTerm x26
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26]) (\ x27 -> do
  let sx27 = showTerm x27
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27]) (\ x28 -> do
  let sx28 = showTerm x28
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27,sx28]) (\ x29 -> do
  let sx29 = showTerm x29
  return (Func (c [sx1,sx2,sx3,sx4,sx5,sx6,sx7,sx8,sx9,sx10,sx11,sx12,sx13,sx14,sx15,sx16,sx17,sx18,sx19,sx20,sx21,sx22,sx23,sx24,sx25,sx26,sx27,sx28,sx29]) (\ x30 ->
    f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 where c = Term n . (xs++)

