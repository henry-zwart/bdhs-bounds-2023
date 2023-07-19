import json

# Read JSON data from file
with open('5_pancake_search_data.json', 'r') as json_file:
    json_data = json.load(json_file)

data_folder = "./pancake5_new_prolog_files_f2e/"
data_folder2 = "./pancake5_new_prolog_files_f2f/"
data_folder3 = "./pancake5_new_prolog_files_vidal/"
num = 0
name = "pancake5_p"
epsilon = 1

# Iterate over each problem in the JSON file
for problem_data in json_data['problems_data']:

    # Extract relevant information from JSON
    goal_state_idx = problem_data['goal_state_idx']
    initial_state_idx = problem_data['initial_state_idx']
  #  special_nodes = ["f"+str(initial_state_idx), "b"+str(goal_state_idx)]
#    special_nodes = [f'\"f{initial_state_idx}\"', f'\"b{goal_state_idx}\"']
    special_nodes = ['\"f'+str(initial_state_idx)+'\"', '\"b'+str(goal_state_idx)+'\"']
#    temp1 = temp1.replace("\'","")
#    temp2 = temp2.replace("\'","")
#    special_nodes = [temp1, temp2]
#    print("SPECIAL")
#    print(special_nodes)

    # Generate Prolog axioms
    prolog_axioms = []
    prolog_f2e_axioms = []
    prolog_vidal_axioms = []
    prolog_f2f_axioms = []
    openlistF_f2f = []
    openlistB_f2f = []
    openlistF_f2e = []
    openlistB_f2e = []
    openlistF_vidal = []
    openlistB_vidal = []
    glistF = []
    hlistF = []
    dlistF = []
    parentlistF = []
    statelistF = []
    glistB = []
    hlistB = []
    dlistB = []
    parentlistB = []
    statelistB = []
    state_by_idx = problem_data['state_by_idx']
    f2f_by_idx = problem_data['front_to_front_h']

    # Generate starting_openlistF with direction 0 nodes
    for idx,real_idx in state_by_idx.items():
        values = real_idx['values']
        parents = real_idx['parent_indexes']

        if real_idx['direction'] == 0:
#            openlistF.append('f'+str(real_idx['idx'])) ##remove this 1
            glistF.append([idx, values['g']])
            hlistF.append([idx, values['h']])
            dlistF.append([idx, values['d']])
            statelistF.append([idx, real_idx['state']])
            for idx2 in parents:
                parentlistF.append([idx,idx2])
        else:
#            openlistB.append('b'+str(real_idx['idx'])) ## remove this 2
            glistB.append([idx, values['g']])
            hlistB.append([idx, values['h']])
            dlistB.append([idx, values['d']])
            statelistB.append([idx, real_idx['state']])
            for idx2 in parents:
                parentlistB.append([idx,idx2])
    prolog_axioms.append('c_star('+str(problem_data["solution_cost"])+'). ')
    temp = 'special_nodes('+str(special_nodes)+'). '
    temp = temp.replace("\'","")
    print("SPECIAL")
    print(temp)
    prolog_axioms.append(temp)    
#    prolog_axioms.append("starting_openlistF("+ str(openlistF) + '). ')
#    prolog_axioms.append("starting_openlistB("+ str(openlistB) + '). ')
#    print("parentlist")
#    print(parentlistF)
    for idx2,real_idx2 in state_by_idx.items():
        values2 = real_idx2['values']
#        print("idx2")
#        print(idx2)
        if real_idx2['direction'] == 0:
            for member in parentlistF:
                if member[0] == idx2:
                    for member5 in glistF:
#                        print("member5")
#                        print(member5)
#                        print(member5[0])
#                        print("member")
#                        print(member)
#                        print(member[1])
                        if str(member5[0]) == str(member[1]):
#                            print("idx3")
#                            print(real_idx2)
#                            print(values2)
#                            print(idx2)
#                            print(values2['g'])
#                            print("parent")
#                            print(member5[0])
#                            print(member5[1])
                            prolog_axioms.append('parent(\"f'+str(member[1])+'\",\"f'+str(idx2)+'\",'+str(values2['g']-member5[1])+'). ')
            for member2 in statelistB:
                if member2[1] == real_idx2['state']:
                    prolog_axioms.append('opposite(\"f'+str(idx2)+'\",\"b'+str(member2[0])+'\"). ')
            for member3 in glistB:
                for member4 in hlistB:
                    for memberd in dlistB:
                        if member3[0] == member4[0] and memberd[0] == member4[0]:
                            #                        print("lb")
                            lb = max(member3[1] + member4[1], member3[1] + epsilon + values2['g'],values2['g'] + values2['h'])
                            lb_vidal = max(member3[1] + member4[1] + values2['d'], member3[1] + epsilon + values2['g'],values2['g'] + values2['h'] + memberd[1])
                            #                        print(idx2)
                            #                        print(member3[0])
                            if lb <= problem_data["solution_cost"]:
                                prolog_f2e_axioms.append('lower_bound_pair(\"f'+str(idx2)+'\",\"b'+str(member3[0])+'\",'+str(lb)+'). ')   ### 1 and 2 add in here that y
                                if not '\"f'+str(idx2)+'\"' in openlistF_f2e:
                                    openlistF_f2e.append('\"f'+str(idx2)+'\"')
                                if not '\"b'+str(member3[0])+'\"' in openlistB_f2e:
                                    openlistB_f2e.append('\"b'+str(member3[0])+'\"')
                            if lb_vidal <= problem_data["solution_cost"]:
                                prolog_vidal_axioms.append('lower_bound_pair(\"f'+str(idx2)+'\",\"b'+str(member3[0])+'\",'+str(lb)+'). ')   ### 1 and 2 add in here that you add it it is not already there.....
                                if not '\"f'+str(idx2)+'\"' in openlistF_vidal:
                                    openlistF_vidal.append('\"f'+str(idx2)+'\"')
                                if not '\"b'+str(member3[0])+'\"' in openlistB_vidal:
                                    openlistB_vidal.append('\"b'+str(member3[0])+'\"')
                #moved left 2 indents             
                print(problem_data)
                print("test")
                print( problem_data["front_to_front_h"])
                for f2f_cost,f2f_list in f2f_by_idx.items():
                    print("cost")
                    print(f2f_cost)
                    print("list")
                    print(f2f_list)
                    for list_member in  f2f_list:
                        print("list_member")
                        print(list_member)
                        print("idx2")
                        print(idx2)
                        print("member3")
                        print(member3[0])
                        if str(list_member[0]) == str(idx2) and str(list_member[1]) == str(member3[0]):
                            print("got here")
                            print(list_member)
                            print("idx2")
                            print(idx2)
                            first_term = member3[1] + epsilon + values2['g']
                            print("first_term")
                            print(first_term)
                            second_term = member3[1] + int(f2f_cost[0]) + values2['g']
                            print("second_term")
                            print(second_term)
                            lb_f2f = max(first_term,second_term)
                            print("lb_f2f")
                            print(lb_f2f)
                            if lb_f2f <= problem_data["solution_cost"]:
                                prolog_f2f_axioms.append('lower_bound_pair(\"f'+str(idx2)+'\",\"b'+str(member3[0])+'\",'+str(lb_f2f)+'). ')
                                if not '\"f'+str(idx2)+'\"' in openlistF_f2f:
                                    openlistF_f2f.append('\"f'+str(idx2)+'\"')
                                if not '\"b'+str(member3[0])+'\"' in openlistB_f2f:
                                    openlistB_f2f.append('\"b'+str(member3[0])+'\"')
#                        print(lb)
      ##          if member3[0] ==

        if  real_idx2['direction'] == 1:
            for member6 in parentlistB:
                if member6[0] == idx2:
                    for member7 in glistB:
                        if str(member7[0]) == str(member6[1]):
#                            print("idx3")
#                            print(real_idx2)
#                            print(values2)
#                            print(idx2)
#                            print(values2['g'])
#                            print("parent")
#                            print(member7[0])
#                            print(member7[1])
                            prolog_axioms.append('parent(\"b'+str(member6[1])+'\",\"b'+str(idx2)+'\",'+str(values2['g']-member7[1])+'). ')
#                    prolog_axioms.append('parent(b'+str(member[1])+',b'+str(idx2)+').')

        
    ###go through list again and make 1) parent 2) opposite 3) lower bounds - use forward list to find direction
#    print("prolog1")
#    print(prolog_axioms)
#    print(glist)
#    print(hlist)
#    print(parentlist)
#    print(statelist)
    real_name= data_folder + name + str(num) + "f2e.pl" ### 1& 2 right before this add both this to axions
    temp2 = "starting_openlistF("+ str(openlistF_f2f) + '). '
    temp2 = temp2.replace("\'","")
    prolog_f2f_axioms.append(temp2)
    temp4 = "starting_openlistB("+ str(openlistB_f2f) + '). '
    temp4 = temp4.replace("\'","")
    prolog_f2f_axioms.append(temp4)
    temp3 = "starting_openlistF("+ str(openlistF_f2e) + '). '
    temp3 = temp3.replace("\'","")
    prolog_f2e_axioms.append(temp3)
    temp5 = "starting_openlistB("+ str(openlistB_f2e) + '). '
    temp5 = temp5.replace("\'","")
    prolog_f2e_axioms.append(temp5)
    temp6 = "starting_openlistF("+ str(openlistF_vidal) + '). '
    temp6 = temp6.replace("\'","")
    prolog_vidal_axioms.append(temp6)
    temp7 = "starting_openlistB("+ str(openlistB_vidal) + '). '
    temp7 = temp7.replace("\'","")   
    prolog_vidal_axioms.append(temp7)    
    with open(real_name, 'w') as prolog_file:
        prolog_file.write('\n'.join(prolog_axioms))
        prolog_file.write('\n')
        prolog_file.write('\n'.join(prolog_f2e_axioms))
    real_name2= data_folder2 + name + str(num) + "f2f.pl"
    with open(real_name2, 'w') as prolog_file:
        prolog_file.write('\n'.join(prolog_axioms))
        prolog_file.write('\n')
        prolog_file.write('\n'.join(prolog_f2f_axioms))
        real_name3= data_folder3 + name + str(num) + "vidal.pl"
    with open(real_name3, 'w') as prolog_file:
        prolog_file.write('\n'.join(prolog_axioms))
        prolog_file.write('\n')
        prolog_file.write('\n'.join(prolog_vidal_axioms))
    num = num +1
#    if num == 2:
#        exit()
"""        parents = real_idx['parent_indexes']
            for idx2 in state_by_idx.items():
              if idx2 in parents:
                    if idx_parent[0] == parent:
                        real_parent = idx_parent[1]
                        parent_values = real_idx['values']
                        parent_g_value = parent_values['g']
                parent_list.append('parent('+'f'+str(real_idx['idx'])+',f'+str(parent)+','+parent_g_value+'). '
            ###add parent axions
            ###add opposite axioms
            ###store g and h for each state
        else:
            openlistB.append('b'+str(real_idx['idx']))
            ####add parent axioms
            ###store g and h for each state

            ###do lower bound by going through f list
                  ###do lower bound by going through b list

    #apprend parent and opposite axioms
    #prolog_axioms.append(f'starting_openlistF([{",".join([f"f{idx}" for idx in state_by_idx.keys()])}]).')
    #prolog_axioms.append(f'starting_openlistB([{",".join([f"b{idx}" for idx in state_by_idx.keys()])}]).')
    #    prolog_axioms.append(f'c_star({json_data["solution_cost"]}).')
    #    prolog_axioms.append(f'special_nodes([{",".join(special_nodes)}]).')

    print("prolog2")
    print(prolog_axioms)   
    real_name= data_folder + name + str(num) + ".pl"
    with open(real_name, 'w') as prolog_file:
        prolog_file.write('\n'.join(prolog_axioms))
    exit()


    for idx in state_by_idx.items():
        for parent in real_idx: 
            prolog_axioms.append('parent(f"str(idx}),f{parent_idx},1).' for parent_idx in state['parent_indexes']) 
    
    # Generate opposite axioms for nodes with the same state information
    opposite_axioms = []
    state_values = {}  # Keep track of state values to find opposites

    for idx, state in state_by_idx.items():
        state_str = state['state']
        if state_str not in state_values:
            state_values[state_str] = f'f{idx}'
        else:
            opposite_axioms.append(f'opposite({state_values[state_str]}, f{idx}).')

    prolog_axioms.extend(opposite_axioms)

    for idx1, state1 in state_by_idx.items():
        if state1['direction'] == 0:
            for idx2, state2 in state_by_idx.items():
                if state2['direction'] == 1:
                    max_value = max(state1['values']['g'] + state1['values']['h'],
                                    state2['values']['g'] + state2['values']['h'],
                                    state1['values']['g'] + state2['values']['g'] + 1)
                    prolog_axioms.append(f'lower_bound_pair(f{idx1},f{idx2},{max_value}).')
"""

