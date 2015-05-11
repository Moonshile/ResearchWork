#coding=utf-8

"""
Functions for checking invariants with NuSMV

@author Yongjian Li <lyj238@gmail.com>
@author Kaiqiang Duan <duankq@ios.ac.cn>
"""

from pexpect import spawn, EOF, TIMEOUT

class SMV(object):
    def __init__(self, smv_path, smv_file):
        super(SMV, self).__init__()
        self.smv_path = smv_path
        self.process = spawn(smv_path + ' -dcx -int -old ' + smv_file)
        self.process.expect(['NuSMV > ', EOF, TIMEOUT])

    def go_and_compute_reachable(self):
        self.process.send('go\ncompute_reachable\n')
        self.process.expect(['The diameter of the FSM is ', EOF, TIMEOUT], timeout=-1)
        res = self.process.expect(['\.\s+NuSMV > ', EOF, TIMEOUT])
        if res == 0:
            return self.process.before
        return '0'

    def check(self, invariant):
        self.process.send('check_invar -p \"' + invariant + '\"\n')
        self.process.expect(['-- invariant .* is ', 'ERROR: ', EOF, TIMEOUT], timeout=1)
        self.process.before
        res = self.process.expect(['\s*NuSMV > ', EOF, TIMEOUT], timeout=1)
        if res == 0:
            return self.process.before
        return '0'

    def exit(self):
        self.process.send('quit\n')
        res = self.process.expect([EOF, TIMEOUT], timeout=1)
        return res == 0

"""
smv = SMV('/home/duan/Downloads/NuSMV/bin/NuSMV', '/tmp/NuSMV/20ca85cec23c961fd6aa03f2a85db182.smv')
print smv.process.before
print smv.go_and_compute_reachable()
print smv.check('af')
print smv.check('n[1] = i')
print smv.check('n[1] = c -> n[2] != c')
print smv.exit()
"""
