import Monad

path="/home/bbr/kics/src/lib/"
prelude="StrictPrelude.hs"
include="StrictPrelude.inc"

main = do
  p1 <- readFile $ path++prelude
  p2 <- readFile $ path++include
  when (p1==p1) (writeFile (path++prelude) (unlines $ merge (lines p1) (lines p2)))

merge (('-':'-':' ':'#':_):lss) p2 = p2++drp lss 
merge (l:lss) p2 = l : merge lss p2

drp (('-':'-':' ':'#':_):lss) = lss
drp (_:lss) = drp lss