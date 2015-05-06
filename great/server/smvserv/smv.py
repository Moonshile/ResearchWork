#coding=utf-8

"""
Functions for checking invariants with NuSMV

@author Yongjian Li <lyj238@gmail.com>
@author Kaiqiang Duan <duankq@ios.ac.cn>
"""

from expect import spawn, EOF

class SMV(object):
    def __init__(self, smv_path, smv_file, timeout=-1):
        super(SMV, self).__init__()
        self.smv_path = smv_path
        self.process = spawn(smv_path + ' -dcx -int -old ' + smv_file, timeout=timeout)
        res = self.expect(['NuSMV > '], EOF, TIMEOUT)

    def go_and_compute_reachable(self):
        self.process.send('go\ncompute_reachable\n')
        res = self.process.expect(['The diameter of the FSM is ', EOF, TIMEOUT])
        res = self.process.expect(['\.\s+NuSMV > ', EOF, TIMEOUT])
        if res == 0:
            return self.process.before

    def check(self, invariant):
        pass
