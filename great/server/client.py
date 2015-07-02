#coding=utf-8

import time
import socket

HOST = '192.168.1.33'
PORT = 50008

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))
to_check = '3,1,n_flash_nodata,(!((Sta.Proc.CacheState[0] = cache_s) & (!Sta.Dir.HeadVld = TRUE)))'
quit = '7,1,n_flash_nodata'
s.sendall('%d,%s'%(len(to_check), to_check))
data = s.recv(1024)
print data
s.close()
