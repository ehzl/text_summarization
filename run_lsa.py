import subprocess
import os

command = 'Rscript'
args = os.getcwd()
path2script = args+'/lsa.R'

cmd = [command, path2script] #+ args

subprocess.check_output(cmd, universal_newlines=True)

print('Complete')
