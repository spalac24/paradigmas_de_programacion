import System.Mem (performGC)
import System.CPUTime
import qualified Curry_Prelude as CP

external_d_C_getProcessInfos :: Cover -> ConstStore -> 
   CP.C_IO (CP.OP_List (CP.OP_Tuple2 C_ProcessInfo CP.C_Int))
external_d_C_getProcessInfos _ _ = fromIO $ do
  t <- getCPUTime
  return (CP.OP_Cons (CP.OP_Tuple2 C_RunTime (toCurry (t `div` (10^9))))
                     CP.OP_List)

external_d_C_garbageCollectorOff :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOff _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollectorOn :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOn _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollect :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollect _ _ = toCurry performGC
