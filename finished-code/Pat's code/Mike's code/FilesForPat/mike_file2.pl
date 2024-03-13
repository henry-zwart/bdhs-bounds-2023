lbComputation(f2e_lower_bound_pair).
problem("/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/prologFile/pancake3_new_prolog_files_f2e/pancake3_p5f2e.plmike-output.csv").
lb(Node1, Node2, LBVal) :- once(f2e_lower_bound_pair(Node1,Node2,LBVal)).
