{- Auto generated stubs for external functions and types
   Remove this comment to suppress warnings. -}
 
strict_prim_doesFileExist ::
                          (DebugMonad.DM dm) =>
                            SPrelude.List dm (SPrelude.Char dm) ->
                              dm (SPrelude.IO dm (SPrelude.Bool dm))
strict_prim_doesFileExist x0
  = hook_strict_prim_doesFileExist x0
      (Prelude.error "not implemented")
 
strict_prim_doesDirectoryExist ::
                               (DebugMonad.DM dm) =>
                                 SPrelude.List dm (SPrelude.Char dm) ->
                                   dm (SPrelude.IO dm (SPrelude.Bool dm))
strict_prim_doesDirectoryExist x0
  = hook_strict_prim_doesDirectoryExist x0
      (Prelude.error "not implemented")
 
strict_prim_fileSize ::
                     (DebugMonad.DM dm) =>
                       SPrelude.List dm (SPrelude.Char dm) ->
                         dm (SPrelude.IO dm (SPrelude.Int dm))
strict_prim_fileSize x0
  = hook_strict_prim_fileSize x0 (Prelude.error "not implemented")
 
strict_prim_getModificationTime ::
                                (DebugMonad.DM dm) =>
                                  SPrelude.List dm (SPrelude.Char dm) ->
                                    dm (SPrelude.IO dm (STime.ClockTime dm))
strict_prim_getModificationTime x0
  = hook_strict_prim_getModificationTime x0
      (Prelude.error "not implemented")
 
strict_getCurrentDirectory ::
                           (DebugMonad.DM dm) =>
                             dm (SPrelude.IO dm (SPrelude.List dm (SPrelude.Char dm)))
strict_getCurrentDirectory
  = hook_strict_getCurrentDirectory (Prelude.error "not implemented")
 
strict_prim_setCurrentDirectory ::
                                (DebugMonad.DM dm) =>
                                  SPrelude.List dm (SPrelude.Char dm) ->
                                    dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_setCurrentDirectory x0
  = hook_strict_prim_setCurrentDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_getDirectoryContents ::
                                 (DebugMonad.DM dm) =>
                                   SPrelude.List dm (SPrelude.Char dm) ->
                                     dm
                                       (SPrelude.IO dm
                                          (SPrelude.List dm (SPrelude.List dm (SPrelude.Char dm))))
strict_prim_getDirectoryContents x0
  = hook_strict_prim_getDirectoryContents x0
      (Prelude.error "not implemented")
 
strict_prim_createDirectory ::
                            (DebugMonad.DM dm) =>
                              SPrelude.List dm (SPrelude.Char dm) ->
                                dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_createDirectory x0
  = hook_strict_prim_createDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_removeFile ::
                       (DebugMonad.DM dm) =>
                         SPrelude.List dm (SPrelude.Char dm) ->
                           dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_removeFile x0
  = hook_strict_prim_removeFile x0 (Prelude.error "not implemented")
 
strict_prim_removeDirectory ::
                            (DebugMonad.DM dm) =>
                              SPrelude.List dm (SPrelude.Char dm) ->
                                dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_removeDirectory x0
  = hook_strict_prim_removeDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_renameFile ::
                       (DebugMonad.DM dm) =>
                         SPrelude.List dm (SPrelude.Char dm) ->
                           SPrelude.List dm (SPrelude.Char dm) ->
                             dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_renameFile x0 x1
  = hook_strict_prim_renameFile x0 x1
      (Prelude.error "not implemented")
 
strict_prim_renameDirectory ::
                            (DebugMonad.DM dm) =>
                              SPrelude.List dm (SPrelude.Char dm) ->
                                SPrelude.List dm (SPrelude.Char dm) ->
                                  dm (SPrelude.IO dm (SPrelude.Unit dm))
strict_prim_renameDirectory x0 x1
  = hook_strict_prim_renameDirectory x0 x1
      (Prelude.error "not implemented")
