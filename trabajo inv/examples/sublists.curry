sublists:: [a] -> [a]
sublists x
	 |(_++sub)++_ =:= x & sub =:= _:_ = sub where sub free
