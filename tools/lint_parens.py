#!/usr/bin/env python3
"""Проверка баланса скобок в AutoLISP-файле (с учётом строк и комментариев)."""
import sys


def scan(path):
    try:
        src = open(path, encoding='utf-8').read()
    except UnicodeDecodeError:
        src = open(path, encoding='cp1251').read()
    stack = []
    line = 1
    i = 0
    n = len(src)
    instr = False
    incomment = False
    problems = []
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1
            incomment = False
            i += 1
            continue
        if incomment:
            i += 1
            continue
        if instr:
            if c == '\\':
                i += 2
                continue
            if c == '"':
                instr = False
            i += 1
            continue
        if c == ';':
            incomment = True
            i += 1
            continue
        if c == '"':
            instr = True
            i += 1
            continue
        if c == '(':
            stack.append(line)
        elif c == ')':
            if not stack:
                problems.append(("extra )", line))
            else:
                stack.pop()
        i += 1
    for ln in stack:
        problems.append(("unclosed (", ln))
    if instr:
        problems.append(("unclosed string", line))
    return problems


if __name__ == "__main__":
    for p in sys.argv[1:]:
        pr = scan(p)
        if pr:
            print("%s: %d проблем" % (p, len(pr)))
            for kind, ln in pr[:20]:
                print("   строка %d: %s" % (ln, kind))
            sys.exit(1)
        print("%s: скобки сбалансированы" % p)
