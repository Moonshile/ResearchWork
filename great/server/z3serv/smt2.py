#coding=utf-8

"""
Functions for checking smt2 formulae

@author Yongjian Li <lyj238@gmail.com>
@author Kaiqiang Duan <duankq@ios.ac.cn>
"""

from z3 import Solver, parse_smt2_string

def check(smt2_str):
    s = Solver()
    s.add(parse_smt2_string(smt2_str))
    return str(s.check())
