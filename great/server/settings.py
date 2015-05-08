#coding=utf-8

HOST = ''
PORT = 50008

# maximum sleep time while there is no connect for a smv process
MAX_SLEEP_TIME = 5

# time out in seconds
TIME_OUT = 5

# path to NuSMV
SMV_PATH = '/home/duan/Downloads/NuSMV/bin/NuSMV'

# path for storing smv files
SMV_FILE_DIR = '/tmp/NuSMV/'






dirs = [SMV_FILE_DIR]

import os

for d in dirs:
    if not os.path.isdir(d):
        os.mkdir(d)
