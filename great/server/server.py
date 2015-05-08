#coding=utf-8

import time, os

from simpserv import start_server, Pool
from smv import SMV

from settings import MAX_SLEEP_TIME, TIME_OUT, SMV_PATH, SMV_FILE_DIR, HOST, PORT

ERROR = '-2'
WAITING = '-1'
OK = '0'
COMPUTE_REACHABLE = '1'
CHECK_INV = '2'


pool = Pool()

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


def start_smv(name, content, smv_path, smv_file_dir):
    smv_file = smv_file_dir + hashlib.md5(name).hexdigest()
    new_smv_file = True
    if os.path.isfile(smv_file):
        with open(smv_file, 'r') as f:
            c = f.read()
            if hashlib.md5(content).hexdigest() == hashlib.md5(c).hexdigest():
                new_smv_file = False
    if new_smv_file:
        with open(smv_file, 'w') as f:
            f.write(content)
    smv = SMV(smv_path, smv_file)
    pool.add(name, smv_handler, smv)

def serv(conn, addr):
    data = ''
    try:
        data += conn.recv(1024)
    except socket.timeout, e:
        pass
    cmd = data.split(',')
    if cmd[0] == COMPUTE_REACHABLE:
        """
        In this case, cmd should be [command, command_id, name, smv file content]
        """
        start_smv(cmd[2], cmd[3], SMV_PATH, SMV_FILE_DIR)
        pool.send(cmd[2], [COMPUTE_REACHABLE, cmd[1]])
        conn.sendall(OK)
    elif cmd[0] == QUERY_REACHABLE:
        """
        In this case, cmd should be [command, command_id, name]
        """
        data = pool.recv(cmd[2])
        conn.sendall(','.join([OK] + data) if data else WAITING)
    elif cmd[0] == CHECK_INV:
        """
        In this case, cmd should be [command, command_id, name, inv]
        """
        pool.send(cmd[2], [CHECK_INV, cmd[1], cmd[3]])
        res = pool.recv(cmd[2])
        while not res:
            res = pool.recv(cmd[2])
        conn.sendall(','.join([OK] + res))
    conn.close()

if __name__ == '__main__':
    start_server(HOST, PORT, serv=serv, timeout=TIME_OUT)

