import subprocess
#import sys

command ='Rscript'
path2script = 'C:/Users/DK/summarization/lsa_final.R'

cmd = [command, path2script]

#x = subprocess.check_output(cmd,shell=True)
x = subprocess.Popen(cmd,shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

print(x)
