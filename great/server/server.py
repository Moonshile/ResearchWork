#coding=utf-8

import time, os, socket, hashlib, sys

from simpserv import start_server
from smvserv import SMV
from z3serv import SMT2

from settings import MAX_SLEEP_TIME, TIME_OUT, SMV_PATH, SMV_FILE_DIR, HOST, PORT

ERROR = '-2'
WAITING = '-1'
OK = '0'
COMPUTE_REACHABLE = '1'
QUERY_REACHABLE = '2'
CHECK_INV = '3'
SMV_QUIT = '7'
SET_SMT2_CONTEXT = '4'
QUERY_SMT2 = '5'
QUERY_STAND_SMT2 = '6'


smt2_pool = {}
smv_pool = {}

__verbose = False

def add_smv_process(name, content):
    smv_file = SMV_FILE_DIR + hashlib.md5(name).hexdigest() + '.smv'
    new_smv_file = True
    if os.path.isfile(smv_file):
        with open(smv_file, 'r') as f:
            c = f.read()
            if content == c:
                new_smv_file = False
    if new_smv_file:
        with open(smv_file, 'w') as f:
            f.write(content)
    smv = SMV(SMV_PATH, smv_file)
    smv_pool[name] = smv


def serv(conn, addr):
    data = ''
    size = 1024
    len_to_recv = None
    while len(data) < len_to_recv or len_to_recv is None:
        try:
            d = conn.recv(size)
            if len_to_recv is None:
                d = d.split(',')
                len_to_recv = int(d[0])
                data += ','.join(d[1:])
            else:
                data += d
        except socket.timeout, e:
            pass
    if __verbose:
        print data
    cmd = data.split(',')
    if cmd[0] == COMPUTE_REACHABLE:
        """
        In this case, cmd should be [length, command, command_id, name, smv file content]
        """
        # There are many ',' in smv file, so should concat the parts splited
        name = cmd[2]
        content = ','.join(cmd[3:])
        add_smv_process(name, content)
        data = smv_pool[cmd[2]].go_and_compute_reachable()
        conn.sendall(OK)
    elif cmd[0] == QUERY_REACHABLE:
        """
        In this case, cmd should be [length, command, command_id, name]
        """
        data = smv_pool[cmd[2]].query_reachable()
        conn.sendall(','.join([OK, data]) if data else WAITING)
    elif cmd[0] == CHECK_INV:
        """
        In this case, cmd should be [length, command, command_id, name, inv]
        """
        res = smv_pool[cmd[2]].check(cmd[3])
        conn.sendall(','.join([OK, res]))
    elif cmd[0] == SMV_QUIT:
        """
        In this case, cmd should be [length, command, command_id, name]
        """
        smv_pool.exit(cmd[2])
        conn.sendall(OK)
    elif cmd[0] == SET_SMT2_CONTEXT:
        """
        In this case, cmd should be [length, command, command_id, name, context]
        """
        smt2 = SMT2(cmd[3])
        smt2_pool[cmd[2]] = smt2
        conn.sendall(OK)
    elif cmd[0] == QUERY_SMT2:
        """
        In this case, cmd should be [length, command, command_id, name, formula]
        """
        if cmd[2] in smt2_pool:
            res = smt2_pool[cmd[2]].check(cmd[3])
            conn.sendall(','.join([OK, res]))
        else:
            conn.sendall(ERROR)
    elif cmd[0] == QUERY_STAND_SMT2:
        """
        In this case, cmd should be [length, command, command_id, context, formula]
        """
        smt2 = SMT2(cmd[2])
        res = smt2.check(cmd[3])
        conn.sendall(','.join([OK, res]))
    conn.close()

if '-v' in sys.argv or '--verbose' in sys.argv:
    __verbose = True
if '-h' in sys.argv or '--help' in sys.argv:
    print """Usage: [-v|-h] to [--verbose|--help]"""

if __name__ == '__main__':
    start_server(HOST, PORT, serv=serv, timeout=TIME_OUT)

