#coding=utf-8

"""
Functions for checking invariants with NuSMV

@author Yongjian Li <lyj238@gmail.com>
@author Kaiqiang Duan <duankq@ios.ac.cn>
"""

from pexpect import spawn, EOF, TIMEOUT

class SMV(object):
    def __init__(self, smv_path, smv_file, timeout=1):
        super(SMV, self).__init__()
        self.smv_path = smv_path
        self.process = spawn(smv_path + ' -dcx -int -old ' + smv_file)
        self.timeout = timeout
        self.diameter = None
        self.isComputing = False
        self.clear()

    def clear(self):
        self.process.expect(['NuSMV\s+>\s+', EOF, TIMEOUT], timeout=.001)

    def go_and_compute_reachable(self):
        self.clear()
        if not self.diameter and not self.isComputing:
            self.isComputing = True
            self.process.send('go\ncompute_reachable\n')

    def query_reachable(self):
        if self.diameter:
            return self.diameter
        computed = self.process.expect(
            ['The\s+diameter\s+of\s+the\s+FSM\s+is ', EOF, TIMEOUT], 
            timeout=0
        )
        if computed == 2:
            return None
        elif computed == 0:
            res = self.process.expect(['\.\s+NuSMV\s+>\s+', EOF, TIMEOUT])
            if res == 0:
                self.diameter = self.process.before
                return self.diameter
            return '-1'

    def check(self, invariant):
        self.clear()
        self.process.send('check_invar -p \"' + invariant + '\"\n')
        self.process.expect(['--\s+invariant\s+.*\s+is\s+', 'ERROR:\s+', EOF, TIMEOUT],
            timeout=self.timeout)
        self.process.before
        res = self.process.expect(['\s*NuSMV\s+>\s+', EOF, TIMEOUT], timeout=self.timeout)
        if res == 0:
            return self.process.before.strip()
        return '0'

    def exit(self):
        self.process.send('quit\n')
        res = self.process.expect([EOF, TIMEOUT], timeout=self.timeout)
        self.process.terminate(force=True)
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
