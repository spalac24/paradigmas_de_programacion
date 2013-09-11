myMember :: a -> [a] -> Success
myMember x l | l =:= _++[x]++_ = success

intersection:: [a] -> [a] -> [a]
intersection [] _ = []
intersection (x:xs) y = 
