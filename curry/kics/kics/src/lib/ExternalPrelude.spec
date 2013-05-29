[ForType "IO" Nothing
,ForType "Float" Nothing
,ForType "Char" Nothing
,ForType "[]" (Just [Show,Read,BaseCurry,Curry])
,ForType "Nat" (Just [Show,Read])
,ForType "Int" (Just [Show,Read])
,ForType "Success" (Just [Show,Read,BaseCurry])
,ForType "Bool" (Just [Declaration,BaseCurry])
,SomeFunctions
]
