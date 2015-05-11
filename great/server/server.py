#coding=utf-8

import time, os, socket, hashlib

from simpserv import start_server, Pool
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


smv_pool = Pool()
smt2_pool = {}

def smv_handler(to_parent, from_parent, smv):
    sleep_time = 0.1
    while True:
        while not from_parent.poll():
            sleep_time = sleep_time*2 if sleep_time < MAX_SLEEP_TIME else sleep_time
            time.sleep(sleep_time)
            pass
        sleep_time = 0.1
        data = from_parent.recv()
        if data[0] == COMPUTE_REACHABLE:
            diameter = smv.go_and_compute_reachable()
            to_parent.send([data[1], diameter])
        elif data[0] == CHECK_INV:
            is_inv = smv.check(args[2])
            to_parent.send([data[1], is_inv])
        elif data[0] == SMV_QUIT:
            res = smv.exit()
            to_parent.send([data[1], res])


def start_smv(name, content, smv_path, smv_file_dir):
    smv_file = smv_file_dir + hashlib.md5(name).hexdigest() + '.smv'
    new_smv_file = True
    if os.path.isfile(smv_file):
        with open(smv_file, 'r') as f:
            c = f.read()
            if content == c:
                new_smv_file = False
    if new_smv_file:
        with open(smv_file, 'w') as f:
            f.write(content)
    smv = SMV(smv_path, smv_file)
    smv_pool.add(name, smv_handler, [smv])

smvs = {}

def serv(conn, addr):
    data = ''
    recv_len = 1024
    while recv_len == 1024:
        try:
            d = conn.recv(1024)
            data += d
            recv_len = len(d)
        except socket.timeout, e:
            pass
    print data
    cmd = data.split(',')
    if cmd[0] == COMPUTE_REACHABLE:
        """
        In this case, cmd should be [command, command_id, name, smv file content]
        """
        # There are many ',' in smv file, so should concat the parts splited
        name = cmd[2]
        content = ','.join(cmd[3:])
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
        smvs[name] = smv
        """
        start_smv(cmd[2], ','.join(cmd[3:]), SMV_PATH, SMV_FILE_DIR)
        smv_pool.send(cmd[2], [COMPUTE_REACHABLE, cmd[1]])
        """
        conn.sendall(OK)
    elif cmd[0] == QUERY_REACHABLE:
        """
        In this case, cmd should be [command, command_id, name]
        """
        data = smvs[cmd[2]].go_and_compute_reachable()
        conn.sendall(','.join([OK, data]) if data else WAITING)
    elif cmd[0] == CHECK_INV:
        """
        In this case, cmd should be [command, command_id, name, inv]
        """
        res = smvs[cmd[2]].check(cmd[3])
        """
        smv_pool.send(cmd[2], [CHECK_INV, cmd[1], cmd[3]])
        res = smv_pool.recv(cmd[2])
        while not res:
            res = smv_pool.recv(cmd[2])
        """
        conn.sendall(','.join([OK, res]))
    elif cmd[0] == SMV_QUIT:
        """
        In this case, cmd should be [command, command_id, name]
        """
        smv_pool.exit(cmd[2])
        conn.sendall(OK)
    elif cmd[0] == SET_SMT2_CONTEXT:
        """
        In this case, cmd should be [command, command_id, name, context]
        """
        smt2 = SMT2(cmd[3])
        smt2_pool[cmd[2]] = smt2
        conn.sendall(OK)
    elif cmd[0] == QUERY_SMT2:
        """
        In this case, cmd should be [command, command_id, name, formula]
        """
        if cmd[2] in smt2_pool:
            res = smt2_pool[cmd[2]].check(cmd[3])
            conn.sendall(','.join([OK, res]))
        else:
            conn.sendall(ERROR)
    elif cmd[0] == QUERY_STAND_SMT2:
        """
        In this case, cmd should be [command, command_id, context, formula]
        """
        smt2 = SMT2(cmd[2])
        res = smt2.check(cmd[3])
        conn.sendall(','.join([OK, res]))
    conn.close()

if __name__ == '__main__':
    start_server(HOST, PORT, serv=serv, timeout=TIME_OUT)

