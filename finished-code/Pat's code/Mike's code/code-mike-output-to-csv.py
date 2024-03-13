import json
import csv
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Read JSON data from file
#real_name= "mike-output.csv" ### 1& 2 right before this add
with open('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/jsonStats', 'r') as json_file:
    json_data = json.load(json_file)
    temp = json_data['jobInfo']
    name = str(temp['problem'])
    real_name= 'outputs/'+str(temp['problem'])+"mike-output.csv" ### 1& 2 right before this add
    print("here")
    print(json_data)
   
    with open(real_name, 'w') as output_file:
        output_file.write('Domain;  Problem; DomainSize; Mode; Heuristic; Algorithm; Cstar; forExpandLess;forEqual;backExpandLess;bacEqual;totalForward;totalBackward;totalLess;total;below ')
        output_file.write('\n')
        for episode in json_data['episodes']:
#        for job in json_data['jobInfo']:
            print('here2')
            output_file.write(str(temp['domain']))
            output_file.write(';')
            output_file.write(str(temp['problem']))
            output_file.write(';')
            output_file.write(str(temp['domainSize']))
            output_file.write(';')
            output_file.write(str(temp['mode']))
            output_file.write(';')
            output_file.write(str(temp['heuristic']))
            output_file.write(';')
            output_file.write(str(temp['lbCalculate']))
            output_file.write(';')
            output_file.write(str(temp['cStar']))
            cStar = temp['cStar']
            output_file.write(';')
            sumForLess = 0
            sumForEq = 0
            sumBacLess = 0
            sumBacEq = 0
            for stat in episode['stats']:
                if stat['action'] == "expand" and stat['direction'] == "forward":
                    if stat['cActionAt'] < cStar: #was lb
                        sumForLess += 1
                    else:
                        sumForEq += 1
                elif stat['action'] == "expand" and stat['direction'] == "backward":
                    if stat['cActionAt'] < cStar: # this must be changed as above
                        sumBacLess += 1
                    else:
                        sumBacEq += 1
            output_file.write(str(sumForLess))
            output_file.write(';')
            output_file.write(str(sumForEq))
            output_file.write(';')
            output_file.write(str(sumBacLess))
            output_file.write(';')
            output_file.write(str(sumBacEq))
            output_file.write(';')
            totalForward = sumForLess + sumForEq
            output_file.write(str(totalForward))
            output_file.write(';')
            totalBackward = sumBacLess + sumBacEq
            output_file.write(str(totalBackward))
            output_file.write(';')
            totalLess = sumBacLess + sumForLess
            output_file.write(str(totalLess))
            output_file.write(';')
            total = totalLess + sumBacEq + sumForEq
            output_file.write(str(total))
            output_file.write(';')
            if total == totalLess:
                output_file.write(str("true"))
            else:
                output_file.write(str("false"))
            output_file.write('\n')

    mydata=pd.read_csv(real_name, delimiter=';')
    sns.set_theme()
    fig1 = sns.relplot(data=mydata,x="totalLess", y="total")
    fig1.savefig(name+"figure1.png") #part_fig.savefig("part_%d.png" % i)
    fig2 = sns.catplot(data=mydata, x='below', y="total",kind="boxen")
    fig2.savefig(name+"figure2.png")
                                 




                                 #            output_file.write(str(job['domain']))
#            output_file.write(';')
#        output_file.write(str(problem['c_star']))
#        output_file.write(';')
#           output_file.write(str(problem['start']))
#           output_file.write(';')
'''
           if problem['size'] <= 1:
                output_file.write(str(0))
           else:
               output_file.write(str(problem['size']))
           output_by_length = problem['output']
           global_min_closed = int(problem['size'])
           global_max_closed = 0
           running_sum_closed = 0
           running_count_closed = 0
           running_min_nodes = int(problem['size'])
           running_max_nodes = 0
           running_sum_nodes = 0
           running_count_nodes = 0
           print("all")
           print(output_by_length)
           for output,output_real in output_by_length.items():
               print("one")
               print(output)
               print("two")
               print(output_real)
               output_file.write(';')
               output_file.write(str(output_real['length']))
               output_file.write(';')
               print("here1")
               print(problem['file'])
               print("output_real['length']")
               print(output_real['length'])
               print("output")
               print(output)
               print("global_min_closed")
               print(global_min_closed)
               print("global_max_closed")
               print(global_max_closed)
               print("running_min_nodes")
               print("running_max_nodes")
               print(running_min_nodes)
               print(running_max_nodes)
               if int(output_real['length']) > 0 and int(output) < global_min_closed:
                   print('here4')
                   global_min_closed = int(output) #output_real['length']
               if int(output_real['length']) > 0 and int(output) > global_max_closed:
                   global_max_closed = int(output) #output_real['length']
               if output_real['length'] > 0:
                   running_sum_closed = running_sum_closed + (float(output_real['length'])*float(output))
                   running_count_closed = running_count_closed + output_real['length']
               print("here2")
               print(global_min_closed)
               print(global_max_closed)
           #    temp = str(output_real['set'])
           #    temp = temp.replace("u","")
           #    output_file.write(temp)
           #    output_file.write(';')
           #return list(set(seq))
               output_file.write(str(output))
               output_file.write(';')
               min_num = int(problem['size'])
               max_num = 0
               sum = 0
               count = 0
               for set in output_real['set']:
                   print("set")
                   print(set)
                   count = 0
                   for item in set:
                       print("item")
                       print(item)
                       if type(item) == int:
                           if item < problem['c_star']:
                               count = count +1
                               print("count")
                               print(count)
                   if count < min_num:
                       min_num = int(count)
                   if count > max_num:
                       max_num = int(count)
                   sum = sum + count
                   print("sum")
                   print(sum)
               if min_num < 1 or sum == 0 :
                   output_file.write(str(0))
               else:
                   output_file.write(str(min_num))
               output_file.write(';')
               output_file.write(str(max_num))
               if min_num < int(problem['size']):
                   running_min_nodes = min(int(running_min_nodes), int(min_num))
               print("max")
               print(max_num)
               print("running_max_nodes")
               print(running_max_nodes)
               running_max_nodes = max(running_max_nodes, max_num)
               print("running_min_nodes2")
               print("running_max_nodes2")
               print(running_min_nodes)
               print(running_max_nodes)
               output_file.write(';')
               print("problem")
               print(output_real['length'])
               print("count")
               print(count)
               print("sum")
               print(sum)
               output_file.write(str(sum)) # new
               running_sum_nodes = running_sum_nodes + sum
#               if output_real['length'] == 0:
#                   output_file.write(str(0))
#               else:
#                   output_file.write(str(sum//output_real['length']))
           print("here3")
           print(problem['file'])
           print(global_min_closed)
           output_file.write(';')
           print("running_min_nodes3")
           print("running_max_nodes3")
           print(running_min_nodes)
           print(running_max_nodes)
           output_file.write(str(running_min_nodes))
#           output_file.write(';')
           output_file.write(';')
           output_file.write(str(running_max_nodes))
           output_file.write(';')
           output_file.write(str(running_sum_nodes))
           output_file.write(';')
           output_file.write(str(running_sum_closed))
           output_file.write(';')
           output_file.write(str(running_count_closed))
           output_file.write(';')
           if running_sum_closed <> 0:
               output_file.write(str(float(running_sum_nodes)/float(running_sum_closed)))
           else:
                 output_file.write("0")             
           output_file.write(';')
           if running_count_closed <> 0:
               output_file.write(str(float(running_sum_closed)/float(running_count_closed)))
           else:
               output_file.write("0")   
           output_file.write(';')
           output_file.write(str(global_min_closed))
           output_file.write(';')
           output_file.write(str(global_max_closed))
           output_file.write(';')           
           output_file.write('\n')
'''
           
