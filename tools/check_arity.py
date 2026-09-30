#!/usr/bin/env python3
"""
check_arity.py -- проверка числа аргументов в вызовах собственных функций.

В AutoLISP вызов с меньшим числом аргументов, чем параметров у defun,
падает в момент выполнения с "слишком мало аргументов". Так упала сборка
12 (KG-FallbackMasterFromDrawing вызвана с одним аргументом при трёх) и
так же едва не прошла сборка 14 (KG-AsNum требует два аргумента, был
вызван с одним). Синтаксис при этом корректен, поэтому ни lint, ни
интерпретатор на частичном прогоне это не ловят.

Скрипт разбирает файл на s-выражения, собирает арность каждого defun и
сверяет её с числом аргументов во всех вызовах.

Запуск:  python3 tools/check_arity.py Integration.lsp
"""

import io
import re
import sys


class Str:
    """Строковый литерал: не атом-имя и не форма."""

    def __init__(self, val):
        self.val = val


class Reader:
    """Минимальный читатель s-выражений: атомы, строки, скобки, комментарии."""

    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.line = 1

    def skip_ws(self):
        while self.pos < len(self.text):
            ch = self.text[self.pos]
            if ch == "\n":
                self.line += 1
                self.pos += 1
            elif ch in " \t\r":
                self.pos += 1
            elif ch == ";":
                while self.pos < len(self.text) and self.text[self.pos] != "\n":
                    self.pos += 1
            else:
                break

    def read_string(self):
        start = self.line
        self.pos += 1
        out = ['"']
        while self.pos < len(self.text):
            ch = self.text[self.pos]
            if ch == "\\" and self.pos + 1 < len(self.text):
                out.append(self.text[self.pos:self.pos + 2])
                if self.text[self.pos + 1] == "\n":
                    self.line += 1
                self.pos += 2
                continue
            if ch == '"':
                self.pos += 1
                out.append('"')
                return "".join(out), start
            if ch == "\n":
                self.line += 1
            out.append(ch)
            self.pos += 1
        raise SyntaxError("незакрытая строка, начата на строке %d" % start)

    def read_atom(self):
        start = self.pos
        while self.pos < len(self.text):
            ch = self.text[self.pos]
            if ch in "()\"; \t\r\n":
                break
            self.pos += 1
        return self.text[start:self.pos]

    def read(self):
        self.skip_ws()
        if self.pos >= len(self.text):
            return None
        ch = self.text[self.pos]
        if ch == "'":
            self.pos += 1
            inner = self.read()
            return ["quote", inner] if inner is not None else ["quote"]
        if ch == '"':
            val, _line = self.read_string()
            return Str(val)
        if ch == "(":
            line = self.line
            self.pos += 1
            out = []
            while True:
                self.skip_ws()
                if self.pos >= len(self.text):
                    raise SyntaxError("незакрытая скобка на строке %d" % line)
                if self.text[self.pos] == ")":
                    self.pos += 1
                    return (out, line)
                if self.text[self.pos] == ".":
                    # dotted pair: точка как отдельный атом
                    pass
                item = self.read()
                if item is None:
                    raise SyntaxError("незакрытая скобка на строке %d" % line)
                out.append(item)
        if ch == ")":
            raise SyntaxError("лишняя ) на строке %d" % self.line)
        return self.read_atom()

    def read_all(self):
        out = []
        while True:
            item = self.read()
            if item is None:
                return out
            out.append(item)


def atom_name(x):
    """Имя символа; None для строк, чисел и форм."""
    if isinstance(x, str):
        return x
    return None


def collect_defs(forms, defs):
    """(defun NAME (a b / c) ...) -> defs[NAME] = число обязательных аргументов.

    Обходится всё дерево: часть defun лежит внутри (if (not KG-TESTING) ...)
    и внутри тел других функций.
    """
    for f in forms:
        if not isinstance(f, tuple):
            continue
        body, _line = f
        if body and atom_name(body[0]) == "defun" and len(body) >= 3:
            name = atom_name(body[1])
            params = body[2]
            if name and isinstance(params, tuple):
                n = 0
                for prm in params[0]:
                    if atom_name(prm) == "/":
                        break
                    n += 1
                defs[name] = n
        collect_defs([x for x in body if isinstance(x, tuple)], defs)


def walk(forms, defs, bad):
    for f in forms:
        if not isinstance(f, tuple):
            continue
        body, line = f
        if body:
            head = atom_name(body[0])
            if head in defs and len(body) - 1 != defs[head]:
                bad.append((head, defs[head], len(body) - 1, line))
        walk([x for x in body if isinstance(x, tuple)], defs, bad)


def main():
    if len(sys.argv) < 2:
        print("Использование: check_arity.py ФАЙЛ.lsp [...]")
        return 2
    total_bad = 0
    for path in sys.argv[1:]:
        text = io.open(path, encoding="utf-8").read()
        forms = Reader(text).read_all()
        defs = {}
        collect_defs(forms, defs)
        bad = []
        walk(forms, defs, bad)
        if bad:
            print("%s: вызовов с неверным числом аргументов: %d" % (path, len(bad)))
            for name, want, got, line in bad:
                print("  строка %d: %s -- defun ждёт %d, передано %d"
                      % (line, name, want, got))
            total_bad += len(bad)
        else:
            print("%s: определений %d, все вызовы с верным числом аргументов"
                  % (path, len(defs)))
    print("Итог: %s" % ("ALL PASS" if total_bad == 0 else "НАЙДЕНО %d" % total_bad))
    return 0 if total_bad == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
