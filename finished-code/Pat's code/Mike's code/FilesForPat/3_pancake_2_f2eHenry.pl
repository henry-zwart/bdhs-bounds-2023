%%%% 3_pancake_2_f2e.pl 
domain(pancake).
mode(unit).
heuristic(gap).

size(3).
c_star(1).
special_nodes(["f25","b30"]).
g("f25",0).
h("f25",1).
d("f25",0).
g("f26",1).
h("f26",0).
d("f26",0).
g("f27",1).
h("f27",2).
d("f27",0).
g("b30",0).
h("b30",1).
d("b30",0).
g("b31",1).
h("b31",0).
d("b31",0).
g("b32",1).
h("b32",2).
d("b32",0).
parent("f25","f26",1).
parent("f25","f27",1).
parent("b30","b31",1).
parent("b30","b32",1).
opposite("f25","b31").
opposite("f26","b30").
h_f2f("f25","b30",1).
h_f2f("f25","b31",0).
h_f2f("f26","b30",0).
starting_openlistF(["f25"]).
starting_openlistB(["b30"]).
f2e_lower_bound_pair("f25","b30",1).
f2f_lower_bound_pair("f25","b30",1).
f2f_lower_bound_pair("f25","b31",1).
f2f_lower_bound_pair("f26","b30",1).
vidal_lower_bound_pair("f25","b30",1).
