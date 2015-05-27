#coding=utf-8

import re
import logging

from copy import deepcopy


typeData = [
    ('CACHE_STATE', ['CACHE_I', 'CACHE_S', 'CACHE_E']),
    ('NODE_CMD', ['NODE_None', 'NODE_Get', 'NODE_GetX']),
    ('UNI_CMD', ['UNI_None', 'UNI_Get', 'UNI_GetX', 'UNI_Put', 'UNI_PutX', 'UNI_Nak']),
    ('INV_CMD', ['INV_None', 'INV_Inv', 'INV_InvAck']),
    ('RP_CMD', ['RP_None', 'RP_Replace']),
    ('WB_CMD', ['WB_None', 'WB_Wb']),
    ('SHWB_CMD', ['SHWB_None', 'SHWB_ShWb', 'SHWB_FAck']),
    ('NAKC_CMD', ['NAKC_None', 'NAKC_Nakc']),
]

def genTypes(typeData):
    typedefs = []
    var = []
    for name, values in typeData:
        t = map(lambda x: 'let _%s = strc \"%s\"'%(x, x), values)
        typedefs.append('\n'.join(t))
        v = '  enum \"%s\" [%s];'%(name, '; '.join(map(lambda x : '_%s'%x, values)))
        var.append(v)
    return '\n\n'.join(typedefs), '\n'.join(var)

#print '\n\n'.join(genTypes(typeData))














types = map(lambda x: x[0], typeData) + ['boolean', 'NODE', 'DATA']

varData = """
  NODE_STATE : record
    ProcCmd : NODE_CMD;
    InvMarked : boolean;
    CacheState : CACHE_STATE;
    CacheData : DATA;
  end;

  DIR_STATE : record
    Pending : boolean;
    Local : boolean;
    Dirty : boolean;
    HeadVld : boolean;
    HeadPtr : NODE;
    ShrVld : boolean;
    ShrSet : array [NODE] of boolean;
    InvSet : array [NODE] of boolean;
  end;

  UNI_MSG : record
    Cmd : UNI_CMD;
    Proc : NODE;
    Data : DATA;
  end;

  INV_MSG : record
    Cmd : INV_CMD;
  end;

  RP_MSG : record
    Cmd : RP_CMD;
  end;

  WB_MSG : record
    Cmd : WB_CMD;
    Proc : NODE;
    Data : DATA;
  end;

  SHWB_MSG : record
    Cmd : SHWB_CMD;
    Proc : NODE;
    Data : DATA;
  end;

  NAKC_MSG : record
    Cmd : NAKC_CMD;
  end;

  STATE : record
    Proc : array [NODE] of NODE_STATE;
    Dir : DIR_STATE;
    MemData : DATA;
    UniMsg : array [NODE] of UNI_MSG;
    InvMsg : array [NODE] of INV_MSG;
    RpMsg : array [NODE] of RP_MSG;
    WbMsg : WB_MSG;
    ShWbMsg : SHWB_MSG;
    NakcMsg : NAKC_MSG;
    CurrData : DATA;
    PrevData : DATA;
    LastWrVld : boolean;
    LastWrPtr : NODE;
    Requester : NODE;
    Collecting : boolean;
    FwdCmd : UNI_CMD;
    FwdSrc : NODE;
    LastInvAck : NODE;
    LastOtherInvAck : NODE;
  end;
"""

index = -1

def judgeRecord(n, p, v):
    if v in types:
        return '  [arrdef \"%s\" %s \"%s\"]'%(n, p, v)
    else:
        return '  record_def \"%s\" %s _%s'%(n, p, v)

def handleArr(n, v):
    if v[:5] == 'array':
        global index
        pattern = re.compile(r'array \[(.+)\] of (.+)')
        param, t = pattern.findall(v)[0]
        index += 1
        return judgeRecord(n, '[paramdef \"i%d\" \"%s\"]'%(index, param), t)
    else:
        return judgeRecord(n, '[]', v)

def genVars(varData):
    record_strs = filter(lambda x: x != '', map(lambda x: x.strip(), varData.split('end;')))
    record_pattern = re.compile(r'(.+)\s*:\s*record\s*(.+)', re.S)
    records = []
    for r in record_strs:
        name, values = record_pattern.findall(r)[0]
        values = map(
            lambda x: tuple(map(lambda y: y.strip(), x.split(':'))), 
            filter(lambda x: x.strip() != '', values.split(';'))
        )
        values = map(
            lambda (name, t): handleArr(name, t),
            values
        )
        values = 'let _%s = List.concat [\n%s\n]'%(name, ';\n'.join(values))
        records.append(values)
    return records

#print '\n\n'.join(genVars(varData))














def analyzeParams(params):
    param_names = map(lambda x: x.split(':')[0].strip(), params.split(';'))
    param_name_dict = {}
    for p in param_names: param_name_dict[p] = 0
    param_defs = map(
        lambda x: 'paramdef ' + ' '.join(map(
            lambda y: '\"%s\"'%y.strip(), 
            x.strip().split(':'))
        ),
        params.split(';')
    )
    return param_name_dict, param_defs










class Formula(object):
    __PRIORITY = {
        '(': 100, '=': 50, '!=': 50, '!': 40, '&': 30, '|': 20, '->': 10
    }
    def __init__(self, text, param_names, consts):
        super(Formula, self).__init__()
        self.param_names = param_names
        self.consts = consts
        blanks = re.compile(r'\s')
        self.form_pattern = re.compile(r'(forall.*?end|exists.*?end|\(|\)|=|!=|!|&|\||->)')
        self.text = filter(lambda y: y, map(lambda x: x.strip(), self.form_pattern.split(text)))
        self.suffix = self.process(self.text)
        self.value = self.evaluate(self.suffix, self.param_names)

    def process(self, text):
        ops = []
        suffix = []
        for t in text:
            if t in self.__PRIORITY:
                while ops != [] and ops[-1] != '(' and self.__PRIORITY[t] <= self.__PRIORITY[ops[-1]]:
                    suffix.append(ops.pop())
                ops.append(t)
            elif t == ')':
                while ops[-1] != '(':
                    suffix.append(ops.pop())
                ops.pop()
            else:
                suffix.append(t)
        while ops != []:
            suffix.append(ops.pop())
        return suffix

    def evalVar(self, var):
        """
        'a[b][c].d.e[f]' -> 
        ['a[b][c]', 'd', 'e[f]'] -> 
        [['a', 'b', 'c'], ['d'], ['e', 'f']]
        """
        name_parts = map(
            lambda n: map(lambda x: x.strip(']'), n.split('[')), 
            var.split('.')
        )
        variables = map(
            lambda parts: 'global \"%s\"'%parts[0] if len(parts) == 1 else\
                'arr \"%s\" [%s]' %(
                    parts[0], 
                    '; '.join(map(lambda p: 'paramref \"%s\"'%p, parts[1:]))
                ),
            name_parts
        )
        return '(%s)'%variables[0] if len(variables) == 1 else '(record [%s])'%('; '.join(variables))

    def evalAtom(self, atom, param_names):
        if atom in self.consts:
            return '(const _%s)'%atom
        elif atom in param_names:
            return '(param (paramref \"%s\""))'%atom
        elif re.match(r'^\d+$', atom):
            return '(const (intc %s))'%atom
        elif atom.lower() in ['true', 'false']:
            return '(const (boolc %s))'%atom.lower()
        elif re.match(r'^forall.*?end$', atom) or re.match(r'^exists.*?end$', atom):
            if re.match(r'^forall.*?end$', atom):
                params, text = re.findall(r'forall(.*?)do(.*?)end', atom)[0]
            else:
                params, text = re.findall(r'exists(.*?)do(.*?)end', atom)[0]
            param_name_dict, param_defs = analyzeParams(params)
            for p in param_names:
                if p not in param_name_dict: param_name_dict[p] = 0
            text = filter(lambda y: y, map(lambda x: x.strip(), self.form_pattern.split(text)))
            sub_form = self.evaluate(self.process(text), param_name_dict)
            if re.match(r'^forall.*?end$', atom):
                return '(forallFormula ~types %s %s)'%(param_defs, sub_form)
            else:
                return '(existFormula ~types %s %s)'%(param_defs, sub_form)
        else:
            return '(var %s)'%self.evalVar(atom)

    __RELATION_OP = {
        '&': '(andList [%s; %s])',
        '|': '(orList [%s; %s])',
        '->': '(imply %s %s)',
        '=': '(eqn %s %s)',
        '!=': '(neg (eqn %s %s))',
    }
    def evaluate(self, suffix, param_names):
        values = []
        for s in suffix:
            if s not in self.__PRIORITY:
                values.append((False, s))
            elif s in ['=', '!=']:
                right = self.evalAtom(values.pop()[1], param_names)
                left = self.evalAtom(values.pop()[1], param_names)
                values.append((True, self.__RELATION_OP[s]%(left, right)))
            elif s == '!':
                evaled, atom = values.pop()
                if evaled: val = '(neg %s)'%atom
                elif atom.strip()[:6] in ['forall', 'exists']:
                    val = '(neg %s)'%self.evalAtom(atom, param_names)
                else: val = '(eqn %s (const _False))'%self.evalAtom(atom, param_names)
                values.append((True, val))
            elif s in ['&', '|', '->']:
                def do_eval(evaled, atom):
                    if evaled: return atom
                    elif atom.strip()[:6] in ['forall', 'exists']:
                        return self.evalAtom(atom, param_names)
                    else: return '(eqn %s (const _True))'%self.evalAtom(atom, param_names)
                rval = do_eval(*values.pop())
                lval = do_eval(*values.pop())
                values.append((True, self.__RELATION_OP[s]%(lval, rval)))
            else:
                logging.error('unknown operator %s'%s)
                pass
        return values[0][1]









class Statement(object):
    def __init__(self, text, param_names, consts):
        super(Statement, self).__init__()
        self.param_names = param_names
        self.consts = consts
        self.statements = self.splitText(text)
        self.value = self.evaluate(self.statements, self.param_names)
        print self.value

    def splitText(self, text):
        parts = filter(lambda p: p, map(lambda x: x.strip(), re.split(r'(;|do|then)', text)))
        big_parts = []
        to_add = []
        exp_ends = 0
        for p in parts:
            if p.startswith(('if', 'for', 'exists')):
                exp_ends += 1
                to_add.append(p)
            elif p.startswith('end'):
                exp_ends -= 1
                to_add.append(p)
                if exp_ends == 0:
                    big_parts.append(' '.join(to_add))
                    to_add = []
            elif exp_ends > 0:
                to_add.append(p)
            elif p != ';':
                big_parts.append(p)
        return big_parts

    def evalVar(self, var):
        """
        'a[b][c].d.e[f]' -> 
        ['a[b][c]', 'd', 'e[f]'] -> 
        [['a', 'b', 'c'], ['d'], ['e', 'f']]
        """
        name_parts = map(
            lambda n: map(lambda x: x.strip(']'), n.split('[')), 
            var.split('.')
        )
        variables = map(
            lambda parts: 'global \"%s\"'%parts[0] if len(parts) == 1 else\
                'arr \"%s\" [%s]' %(
                    parts[0], 
                    '; '.join(map(lambda p: 'paramref \"%s\"'%p, parts[1:]))
                ),
            name_parts
        )
        return '(%s)'%variables[0] if len(variables) == 1 else '(record [%s])'%('; '.join(variables))

    def evalAtom(self, atom, param_names):
        if atom in self.consts:
            return '(const _%s)'%atom
        elif atom in param_names:
            return '(param (paramref \"%s\""))'%atom
        elif re.match(r'^\d+$', atom):
            return '(const (intc %s))'%atom
        elif atom.lower() in ['true', 'false']:
            return '(const (boolc %s))'%atom.lower()
        else:
            return '(var %s)'%self.evalVar(atom)

    def evalIf(self, statement, param_names):
        return 'None'

    def evalFor(self, statement, param_names):
        params, statement_str = re.findall(r'for(.*?)do(.*?)end(?:for)*', statement)[0]
        param_name_dict, param_defs = analyzeParams(params)
        for p in param_names:
            if p not in param_name_dict: param_name_dict[p] = 0
        inner_ss = self.evaluate(self.splitText(statement_str), param_names)
        return '(forStatement %s %s)'%(inner_ss, param_defs)

    def evaluate(self, statements, param_names):
        def inner(statement):
            if statement.startswith('if'):
                return self.evalIf(statement, param_names)
            elif statement.startswith('for'):
                return self.evalFor(statement, param_names)
            else:
                try:
                    vstr, estr = statement.split(':=')
                    v = self.evalVar(vstr.strip())
                    e = self.evalAtom(estr.strip(), param_names)
                    return '(assign %s %s)'%(v, e)
                except:
                    logging.error('unable to handle statement: %s'%statement)
        if len(statements) > 1:
            return '(parallel [%s])'%('; '.join(map(lambda s: inner(s), statements)))
        elif len(statements) == 1:
            return inner(statements[0])
        else:
            logging.error('no statement to be evaluated')








class Rule(object):
    def __init__(self, text, params, param_names, consts):
        super(Rule, self).__init__()
        pattern = re.compile(r'rule\s*\"(.*?)\"\s*(.*?)==>.*?begin(.*?)endrule;', re.S)
        self.name, guard, statements = pattern.findall(text)[0]
        self.params = '[%s]'%'; '.join(params)
        self.param_names = param_names
        self.formula = Formula(guard, self.param_names, consts)
        self.statement = Statement(statements, self.param_names, consts)









class RuleSet(object):
    def __init__(self, text, consts):
        super(RuleSet, self).__init__()
        pattern = re.compile(r'ruleset(.*?)do(.*?)endruleset;', re.S)
        params, rules_str = pattern.findall(text)[0]
        param_name_dict, param_defs = analyzeParams(params)
        rule_texts = re.findall(r'(rule.*?endrule;)', rules_str, re.S)
        rules = map(lambda r: Rule(r, param_defs, param_name_dict, consts), rule_texts)



const_values = reduce(lambda res, x: res + x[1], typeData, [])
consts = {}
for c in const_values: consts[c] = 0

rule_f = open('./rules.m', 'r')
ruletext = rule_f.read()
rule_f.close()
pattern = re.compile(r'(ruleset.*?endruleset;)', re.S)
ruleset_texts = pattern.findall(ruletext)
rulesets = map(lambda x: RuleSet(x, consts), ruleset_texts)
