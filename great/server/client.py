#coding=utf-8

import time
import socket

HOST = 'localhost'
PORT = 50008

while True:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))
    s.sendall('6,,(declare-fun x () Int)(declare-fun y () Int)(declare-fun z () Int),(assert (>= (* 2 x) (+ y z)))(declare-fun f (Int) Int)(declare-fun g (Int Int) Int)(assert (< (f x) (g x x)))(assert (> (f y) (g x x)))(assert (= x y))(check-sat)')
    data = s.recv(1024)
    print data
    s.close()

    time.sleep(.1)
