import json
import csv

# Read JSON data from file
real_name= "pancake4_f2f_nbs_excel_July16.csv" ### 1& 2 right before this add
with open('bdhs-bounds-2023/final_pancake4_f2f_nbs_test_July13_catch_fixed.txt', 'r') as json_file:
    json_data = json.load(json_file)

    
    with open(real_name, 'w') as output_file:
        for problem in json_data['problems_data']:
           output_file.write(problem['file'])
           output_file.write(';')
           output_file.write(str(problem['c_star']))
           output_file.write(';')
           output_file.write(str(problem['start']))
           output_file.write(';')
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
           
