#coding=utf-8

import time
import socket

HOST = 'localhost'
PORT = 50008

while True:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    s.sendall('hello 1')
    data = s.recv(1024)
    print data
    s.close()

    time.sleep(.0000001)
