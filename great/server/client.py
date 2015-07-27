#coding=utf-8

import time
import socket

HOST = '192.168.1.30'
PORT = 50008

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))
to_check = '9,1,n_flash_nodata,(!(Sta.Dir.HeadPtr = 4 & Sta.UniMsg[1].Proc = 2 & Sta.UniMsg[3].Cmd = UNI_GetX))'
quit = '7,1,n_g2k'
s.sendall('%d,%s'%(len(to_check), to_check))
data = s.recv(1024)
print data
s.close()
