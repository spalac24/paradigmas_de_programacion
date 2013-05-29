import Distribution.Simple

main = defaultMainWithHooks 
         simpleUserHooks{hookedPreProcessors=
                           ("curry",\_ _ -> mkCurryLibrary):
                           hookedPreProcessors simpleUserHooks}

mkCurryLibrary :: PreProcessor
mkCurryLibrary = PreProcessor {
  platformIndependent = True,
  runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
    do info verbosity (inFile++" has been preprocessed to "++outFile)
       stuff <- readFile inFile
       writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
       return ExitSuccess
