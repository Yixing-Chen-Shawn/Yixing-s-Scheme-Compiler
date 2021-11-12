#!/usr/bin/python3
#####################################################
#############  LEAVE CODE BELOW  ALONE  #############
# Include base directory into path
import os, sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname( __file__ ), '..', '..')))

# Import tester
from tester import failtest, passtest, assertequals, runcmd, preparefile, runcmdsafe
#############    END UNTOUCHABLE CODE   #############
#####################################################

###################################
# Write your testing script below #
###################################
python_bin = sys.executable
import pickle

# prepare necessary files
preparefile('./test.rkt')
preparefile('./testcase.cps')
preparefile('./header.ll')
preparefile('./combined.ll')
preparefile('./bin')


# run test file
runcmdsafe('rm ./output')
b_stdout, b_stderr, b_exitcode = runcmdsafe(f'racket ./test.rkt')


# convert stdout bytes to utf-8
stdout = ""
stderr = ""
try:
	stdout = b_stdout.decode('utf-8')
	stderr = b_stderr.decode('utf-8')
except:
	pass


# stdout comparison with expected.txt here
try:
	with open('answer', 'rb') as file1, open('output', 'rb') as file2:
		answer = file1.read()
		output = file2.read()
		if answer == output:
			passtest('')
		else:
			failtest(stdout+"\n\n"+stderr)
except FileNotFoundError:
	failtest(stdout+"\n\n"+stderr)
