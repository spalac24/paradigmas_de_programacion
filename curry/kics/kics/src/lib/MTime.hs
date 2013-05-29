{- Auto generated stubs for external functions and types
   Remove this comment to suppress warnings. -}
 
strict_getClockTime ::
                    (DebugMonad.DM dm) => dm (SPrelude.IO dm (ClockTime dm))
strict_getClockTime
  = hook_strict_getClockTime (Prelude.error "not implemented")
 
strict_prim_toCalendarTime ::
                           (DebugMonad.DM dm) =>
                             ClockTime dm -> dm (SPrelude.IO dm (CalendarTime dm))
strict_prim_toCalendarTime x0
  = hook_strict_prim_toCalendarTime x0
      (Prelude.error "not implemented")
 
strict_prim_toUTCTime ::
                      (DebugMonad.DM dm) => ClockTime dm -> dm (CalendarTime dm)
strict_prim_toUTCTime x0
  = hook_strict_prim_toUTCTime x0 (Prelude.error "not implemented")
 
strict_prim_toClockTime ::
                        (DebugMonad.DM dm) => CalendarTime dm -> dm (ClockTime dm)
strict_prim_toClockTime x0
  = hook_strict_prim_toClockTime x0 (Prelude.error "not implemented")
