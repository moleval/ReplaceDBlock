#!/usr/bin/env python3
"""
check_load.py -- проверка, что RepDblock.lsp загружается ЦЕЛИКОМ,
включая раздел 8 (исполняющий слой AutoCAD), и что в разделе 8 нет
обращений к неопределённым LISP-функциям.

AutoCAD в песочнице нет, поэтому примитивы COM/ActiveX подменяются
заглушками. Смысл проверки: файл синтаксически корректен и в нём нет
«висячих» имён после всех правок.
"""

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from autolisp import Interpreter, LispError, Builtin, Function  # noqa: E402

# Примитивы AutoCAD / Visual LISP, которые встречаются в разделе 8.
ACAD_PRIMITIVES = [
    "vl-load-com", "vlax-get-acad-object", "vla-get-ActiveDocument",
    "vla-get-Blocks", "vla-get-ModelSpace", "vla-get-Layouts", "vla-Item",
    "vla-get-Name", "vla-get-IsXRef", "vla-get-IsDynamicBlock",
    "vla-get-ObjectName", "vla-get-EffectiveName", "vla-get-Layer",
    "vla-get-InsertionPoint", "vla-get-Rotation", "vla-get-XScaleFactor",
    "vla-get-YScaleFactor", "vla-get-ZScaleFactor", "vla-get-OwnerID",
    "vla-get-ObjectID", "vla-get-Block", "vla-get-PropertyName",
    "vla-put-Name", "vla-put-Layer", "vla-Delete", "vla-Add", "vla-InsertBlock",
    "vla-get-TagString", "vla-get-TextString", "vla-put-TextString",
    "vla-get-Annotative", "vla-put-Annotative", "vla-get-HasAttributes",
    "vla-CopyObjects", "vlax-invoke", "vlax-get-property", "vlax-put-property",
    "vlax-property-available-p", "vlax-ename->vla-object",
    "vlax-vla-object->ename", "vlax-3d-point", "vlax-make-safearray",
    "vlax-safearray-put-element", "vlax-safearray-get-element",
    "vlax-safearray-get-u-bound", "vlax-variant-value", "vlax-for",
    "vlax-vbobject", "vlax-get-object", "vl-catch-all-apply",
    "vlax-invoke-method", "vla-get-Handle", "vla-get-Layout",
    "vl-catch-all-error-p", "vlax-release-object", "vla-GetInterfaceObject",
    "entget", "entmake", "entnext", "entsel", "ssget", "ssname", "sslength",
    "handent", "tblobjname", "tblsearch", "tblnext", "getvar", "setvar",
    "dictsearch", "dictadd", "dictremove",
    "command", "getstring", "getkword", "initget", "wcmatch", "vl-remove-if",
    "vl-remove-if-not", "vl-some", "vl-every", "vl-princ-to-string",
    "vl-string-subst", "make-string", "chr",
]


def main():
    interp = Interpreter()

    def stub(*args):
        return None

    for name in ACAD_PRIMITIVES:
        interp.define(name, stub)
    # vlax-for -- макрос обхода; достаточно, чтобы не падать на загрузке
    interp.define("vlax-for", lambda *a: None)

    path = os.path.join(ROOT, "RepDblock.lsp")
    try:
        interp.load_file(path)
    except LispError as exc:
        print("ОШИБКА ЗАГРУЗКИ:", exc)
        return 1
    except RecursionError:
        print("ОШИБКА: переполнение стека при загрузке")
        return 1

    print("Файл загружен целиком (KG-TESTING = nil), раздел 8 определён.")

    defined = set(k for k, v in interp.globals.items()
                  if isinstance(v, (Function, Builtin)))

    src = open(path, encoding="utf-8").read()
    # имена, которые вызываются в разделе 8
    sec8 = src.split(";;; РАЗДЕЛ 8")[1]
    # строковые литералы не являются вызовами
    sec8 = re.sub(r'"(?:\\.|[^"\\])*"', '""', sec8)
    # комментарии тоже
    sec8 = re.sub(r";[^\n]*", "", sec8)
    called = set()
    for m in re.finditer(r"\(\s*([A-Za-z*][A-Za-z0-9_*:<>+=\-]*)", sec8):
        called.add(m.group(1).upper())

    specials = {"DEFUN", "SETQ", "IF", "COND", "PROGN", "WHILE", "FOREACH",
                "REPEAT", "AND", "OR", "QUOTE", "LET", "LET*", "LAMBDA",
                "T", "NIL", "VL-CATCH-ALL-APPLY", "VLAX-FOR"}

    # параметры и локальные переменные всех defun -- они связаны лексически
    bound = set()
    for m in re.finditer(r"\(defun\s+[^\s(]+\s*\(([^)]*)\)", src):
        for tok in m.group(1).split():
            if tok == "/":
                continue
            bound.add(tok.upper())

    missing = sorted(n for n in called
                     if n not in defined and n not in specials
                     and n not in bound)
    if missing:
        print("\nВ разделе 8 вызываются неопределённые имена:")
        for n in missing:
            print("   ", n)
        return 1

    print("Неопределённых имён в разделе 8 нет (%d вызываемых имён проверено)."
          % len(called))

    # ключевые команды должны существовать
    cmds = [
        "C:RDB", "C:REPDBLOCK", "C:ПОДМЕНАБЛОКА", "C:ПДБ", "C:INTEGRATE",
        "C:RDBPICK", "C:REPDBLOCKPICK", "C:ПДБВЫБОР",
        "C:RDBCHECK", "C:INTEGRATECHECK", "C:REPDBLOCKCHECK", "C:ПДБЧЕК",
        "C:RDBDIAG", "C:INTDIAG", "C:ПДБДИАГ",
        "C:RDBDUMP", "C:INTDUMP", "C:ПДБДАМП",
        "C:RDBDUMPDEF", "C:INTDUMPDEF", "C:ПДБДАМПОПР",
        "C:RDBPASTETEST", "C:INTPASTETEST", "C:ПДБТЕСТВСТАВКИ",
        "C:RDBTESTBED", "C:INTTESTBED", "C:ПДБСТЕНД",
        "C:RDBDBXTEST", "C:INTDBXTEST", "C:ПДБТЕСТDBX",
        "C:RDBRENAMETEST", "C:INTRENAMETEST", "C:ПДБТЕСТПЕРЕИМ",
        "C:RDBCLEANUP", "C:INTCLEANUP", "C:ПДБОЧИСТКА",
        "C:RDBCOUNT", "C:INTCOUNT", "C:ПДБСЧЁТ",
        "C:RDBBRIEF", "C:INTBRIEF", "C:ПДБКРАТКО",
        "C:RDBERR", "C:INTERR", "C:ПДБОШИБКА"
    ]
    absent = [c for c in cmds if c not in defined]
    if absent:
        print("\nНе определены команды:", absent)
        return 1
    print("Все команды и алиасы определены (%d шт.): %s"
          % (len(cmds), ", ".join(c[2:] for c in cmds)))

    # баннер загрузки обязан перечислять основные команды:
    # смотрим ТОЛЬКО в текст баннера: имя команды встречается и в defun,
    # поэтому проверка по всему файлу была бы тривиально зелёной
    bstart = src.find("Команды: ")
    bend = src.find("))", bstart) if bstart >= 0 else -1
    banner = src[bstart:bend] if bstart >= 0 and bend > bstart else ""
    main_cmds = [
        "RDB", "RDBCHECK", "RDBDIAG", "RDBDUMP", "RDBDUMPDEF",
        "RDBPASTETEST", "RDBTESTBED", "RDBDBXTEST", "RDBRENAMETEST",
        "RDBCLEANUP", "RDBCOUNT", "RDBBRIEF", "RDBERR"
    ]
    banner_missing = [c for c in main_cmds if c not in banner]
    if not banner:
        print("\nБаннер загрузки не найден")
        return 1
    if banner_missing:
        print("\nБаннер загрузки не упоминает команды:", banner_missing)
        return 1
    print("Баннер загрузки перечисляет все основные команды.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
