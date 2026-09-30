#!/usr/bin/env python3
"""
check_acad_funcs.py -- проверка, что в RepDblock.lsp нет вызовов функций,
которых в AutoCAD не существует.

Повод: на реальном чертеже INTEGRATE упал с "no function definition: SORT".
Функции sort в AutoCAD нет -- есть vl-sort, vl-sort-i и acad_strlsort.
Собственный интерпретатор (tests/autolisp.py) sort определял, поэтому все
тесты проходили, а на реальном AutoCAD команда падала. Класс ошибки: имя
есть в стенде, но отсутствует в AutoCAD.

Проверяются две позиции:
  * голова списка -- прямой вызов: (foo 1 2);
  * любой символ KG-* -- имена, передаваемые как данные:
    (vl-catch-all-apply '(lambda () (KG-Foo x)) nil),
    (vl-sort its 'KG-IterOlder).

Локальные переменные (часть списка аргументов после "/", параметры lambda)
вызовами не считаются.

Запуск:  python3 tools/check_acad_funcs.py [файл ...]
По умолчанию проверяется RepDblock.lsp.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(ROOT, "tests"))

from autolisp import parse_all, Dotted  # noqa: E402

# Специальные формы AutoLISP: не функции, но существуют.
SPECIAL_FORMS = {
    "DEFUN", "DEFUN-Q", "SETQ", "IF", "COND", "PROGN", "WHILE", "REPEAT",
    "FOREACH", "LAMBDA", "QUOTE", "FUNCTION", "AND", "OR",
}

# Функции, которые действительно есть в AutoCAD (AutoLISP + Visual LISP).
# Список намеренно строгий: лучше ложная тревога, чем пропущенный SORT.
ACAD_BUILTINS = {
    # AutoLISP: арифметика и сравнение
    "+", "-", "*", "/", "=", "/=", "<", ">", "<=", ">=", "1+", "1-",
    "ABS", "MIN", "MAX", "EXP", "EXPT", "LOG", "SQRT", "REM", "SIN", "COS",
    "ATAN", "ANGLE", "DISTANCE", "POLAR", "OSNAP", "TRANS", "CVUNIT",
    "FIX", "FLOAT", "GCD", "LSH", "LOGAND", "LOGIOR", "BOOLE",
    # списки
    "LIST", "CONS", "APPEND", "REVERSE", "CAR", "CDR", "CADR", "CADDR",
    "CADDDR", "CAAR", "CDAR", "LAST", "NTH", "LENGTH", "MEMBER", "ASSOC",
    "SUBST", "MAPCAR", "APPLY", "ATOM", "NULL", "LISTP", "NUMBERP",
    "ZEROP", "MINUSP", "PLUSP", "EVENP", "ODDP",
    # строки
    "STRCAT", "STRLEN", "SUBSTR", "STRCASE", "CHR", "ASCII", "READ",
    "RTOS", "ITOA", "DISTOF", "ANGTOF", "ANGTOS", "MENUCMD", "WCMATCH",
    # ввод-вывод
    "PRINC", "PRIN1", "PRINT", "PROMPT", "TERPRI", "ALERT", "TEXTSCR",
    "GRAPHSCR", "GETSTRING", "GETINT", "GETREAL", "GETFILED", "GETPOINT",
    "GETKWORD", "GETCORNER", "GETDIST", "GETANGLE", "GETORIENT", "INITGET",
    "FINDFILE", "OPEN", "CLOSE", "READ-LINE", "READ-CHAR", "WRITE-LINE",
    "WRITE-CHAR", "LOAD", "VL-FILENAME-BASE", "VL-FILENAME-DIRECTORY",
    "VL-FILENAME-EXTENSION", "VL-FILENAME-MKTEMP",
    # типы и преобразования
    "TYPE", "SET", "BOUNDP", "EVAL", "EXIT", "QUIT", "GC", "TRACE",
    "UNTRACE", "HELP", "COMMAND", "EQUAL", "VL-SYMBOL-VALUE", "VL-SYMBOLP",
    "VL-SYMBOL-NAME", "ATOI", "READ",
    # работа с примитивами
    "ENTGET", "ENTMAKE", "ENTMAKEX", "ENTMOD", "ENTDEL", "ENTLAST", "ENTNEXT",
    "ENTSEL", "ENTUPD", "ENTREG", "TBLOBJNAME", "TBLNEXT", "TBLSEARCH",
    # dictsearch -- поиск в словаре; возвращает подсписок с группами 3, 350,
    # 360. Используется для чтения ACAD_ENHANCEDBLOCK: там лежит имя параметра
    # видимости динамического блока.
    "DICTSEARCH",
    "HANDENT", "SSGET", "SSADD", "SSDEL", "SSLENGTH", "SSNAME", "SSMEMB",
    "SETVAR", "GETVAR", "NENTSEL", "NENTSELP", "ACAD_STRLSORT",
    # Visual LISP: списки и строки
    "VL-SORT", "VL-SORT-I", "VL-REMOVE", "VL-REMOVE-IF", "VL-REMOVE-IF-NOT",
    "VL-EVERY", "VL-SOME", "VL-POSITION", "VL-LIST-LENGTH", "VL-LIST->STRING",
    "VL-STRING->LIST", "VL-STRING-SEARCH", "VL-SUBST", "VL-STRING-SUBST",
    "VL-STRING-LEFT-TRIM", "VL-STRING-RIGHT-TRIM", "VL-STRING-TRIM",
    "VL-STRING-ELT", "VL-STRING-LENGTH", "VL-CONSP",
    "VL-PRINC-TO-STRING", "VL-PRIN1-TO-STRING", "VL-FILE-COPY",
    # Visual LISP: ошибки и вычисление
    "VL-CATCH-ALL-APPLY", "VL-CATCH-ALL-ERROR-P", "VL-CATCH-ALL-ERROR-MESSAGE",
    "VL-LOAD-COM", "VL-LOAD-ALL", "VL-REGISTRY-READ", "NOT", "T", "NIL",
    "EQ", "SETQ", "PROGN",
    # Visual LISP: variant / safearray
    "VLAX-MAKE-VARIANT", "VLAX-VARIANT-VALUE", "VLAX-VARIANT-TYPE",
    "VLAX-MAKE-SAFEARRAY", "VLAX-SAFEARRAY-FILL",
    "VLAX-SAFEARRAY-GET-ELEMENT", "VLAX-SAFEARRAY-PUT-ELEMENT",
    "VLAX-SAFEARRAY-GET-U-BOUND", "VLAX-SAFEARRAY-GET-L-BOUND",
    "VLAX-SAFEARRAY->LIST", "LIST->VLAX-SAFEARRAY",
    "VLAX-VBDOUBLE", "VLAX-VBSTRING", "VLAX-VBEMPTY", "VLAX-VBINTEGER",
    "VLAX-VBLONG", "VLAX-VBNULL", "VLAX-VBOBJECT", "VLAX-VBSINGLE",
    "VLAX-VBVARIANT", "VLAX-TRUE", "VLAX-FALSE", "VLAX-ERASED-P",
    # Visual LISP: объекты
    "VLAX-GET-ACAD-OBJECT", "VLAX-GET-OBJECT", "VLAX-CREATE-OBJECT",
    "VLAX-GET-PROPERTY", "VLAX-PUT-PROPERTY", "VLAX-PROPERTY-AVAILABLE-P",
    "VLAX-METHOD-APPLICABLE-P", "VLAX-INVOKE", "VLAX-INVOKE-METHOD",
    "VLAX-FOR", "VLAX-DATA->VARIANT", "VLAX-VARIANT->DATA",
    "VLAX-ENAME->VLA-OBJECT", "VLAX-VLA-OBJECT->ENAME", "VLAX-3D-POINT",
    "VLAX-RELEASE-OBJECT", "VLAX-ADD-CMD", "VLAX-REMOVE-CMD",
}

# Префиксы, которые точно принадлежат AutoCAD: свойства и методы ActiveX.
ACAD_PREFIXES = ("VLA-", "VLAX-GET-", "VLAX-PUT-")


def is_str(x):
    return isinstance(x, str)


def walk(node, calls, syms):
    """Собирает головы списков (кандидаты в вызовы) и все символы."""
    if isinstance(node, Dotted):
        walk(node.car, calls, syms)
        walk(node.cdr, calls, syms)
        return
    if isinstance(node, list):
        if node:
            head = node[0]
            if is_str(head):
                calls.add(head)
            for item in node:
                walk(item, calls, syms)
        return
    if is_str(node):
        syms.add(node)


def collect_locals(forms):
    """Локальные переменные: аргументы defun после "/" и параметры lambda."""
    locals_ = set()

    def signature(params):
        """И формальные аргументы, и локальные после "/" -- всё это
        переменные, а не вызовы функций."""
        if not isinstance(params, list):
            return
        for p in params:
            if is_str(p) and p != "/":
                locals_.add(p)

    def go(node):
        if isinstance(node, Dotted):
            go(node.car)
            go(node.cdr)
            return
        if not isinstance(node, list) or not node:
            return
        head = node[0]
        if is_str(head) and head == "DEFUN" and len(node) >= 3:
            signature(node[2])
        if is_str(head) and head == "LAMBDA" and len(node) >= 2:
            params = node[1]
            if isinstance(params, list):
                for p in params:
                    if is_str(p):
                        locals_.add(p)
        for item in node:
            go(item)

    for form in forms:
        go(form)
    return locals_


def collect_assigned(forms):
    """Имена, которым присваивает setq, -- это переменные, не функции."""
    assigned = set()

    def go(node):
        if isinstance(node, Dotted):
            go(node.car)
            go(node.cdr)
            return
        if not isinstance(node, list) or not node:
            return
        if is_str(node[0]) and node[0] == "SETQ":
            for item in node[1::2]:
                if is_str(item):
                    assigned.add(item)
        for item in node:
            go(item)

    for form in forms:
        go(form)
    return assigned


def collect_defined(forms):
    """Все defun в файле, включая вложенные: раздел 8 целиком лежит
    внутри одной формы (if (not KG-TESTING) (progn ...)), поэтому
    искать defun только на верхнем уровне нельзя."""
    defined = set()

    def go(node):
        if isinstance(node, Dotted):
            go(node.car)
            go(node.cdr)
            return
        if not isinstance(node, list) or not node:
            return
        if is_str(node[0]) and node[0] == "DEFUN" and len(node) >= 2:
            if is_str(node[1]):
                defined.add(node[1])
            # (defun (C:NAME) ...) -- не встречается, но на всякий случай
            elif isinstance(node[1], list) and node[1] and is_str(node[1][0]):
                defined.add(node[1][0])
        for item in node:
            go(item)

    for form in forms:
        go(form)
    return defined


def known_name(name):
    up = name.upper()
    if up in ACAD_BUILTINS or up in SPECIAL_FORMS:
        return True
    return up.startswith(ACAD_PREFIXES)


GUARDED = ("VL-CATCH-ALL-APPLY", "KG-SAFE")


def collect_bad_lambdas(forms):
    """Вычисленная лямбда в vl-catch-all-apply / KG-Safe.

    В AutoCAD vl-catch-all-apply принимает имя функции или КВОТИРОВАННУЮ
    лямбду. Вычисленная `(lambda () ...)` превращается в SUBR, и вызов
    падает с «неверная функция: #<SUBR ... -lambda->» -- именно так
    упала сборка 18 на шаге замены экземпляров.
    """
    bad = []

    def go(node):
        if not isinstance(node, list) or not node:
            return
        head = node[0] if is_str(node[0]) else ""
        if head.upper() in GUARDED and len(node) > 1:
            arg = node[1]
            if isinstance(arg, list) and arg and is_str(arg[0]) \
                    and arg[0].upper() == "LAMBDA":
                bad.append(head)
        for item in node:
            go(item)

    for form in forms:
        go(form)
    return bad


def scan(path):
    src = open(path, encoding="utf-8").read()
    forms = parse_all(src)
    calls, syms = set(), set()
    for form in forms:
        walk(form, calls, syms)

    defined = collect_defined(forms)
    locals_ = collect_locals(forms)
    known = defined | locals_

    bad_calls = sorted(n for n in calls
                       if n.upper() not in {k.upper() for k in known}
                       and not known_name(n))
    # среди символов смотрим только KG-*: прочие строки -- это данные
    # Переменные KG-* (KG-VERSION, KG-VERBOSE, ...) задаются через setq
    # и вызовами не являются -- их покрывает проверка вызовов выше.
    assigned = collect_assigned(forms)
    bad_syms = sorted(n for n in syms
                      if n.upper().startswith("KG-")
                      and n.upper() not in {k.upper() for k in known}
                      and n.upper() not in {a.upper() for a in assigned})
    return bad_calls, bad_syms, collect_bad_lambdas(forms), len(calls), len(defined)


def main():
    paths = sys.argv[1:] or [os.path.join(ROOT, "RepDblock.lsp")]
    failed = False
    for path in paths:
        bad_calls, bad_syms, bad_lambdas, ncalls, ndefs = scan(path)
        print("%s: вызовов %d, определений %d"
              % (os.path.basename(path), ncalls, ndefs))
        if bad_calls:
            failed = True
            print("  НЕСУЩЕСТВУЮЩИЕ В AutoCAD ВЫЗОВЫ:")
            for n in bad_calls:
                print("    ", n)
        if bad_syms:
            failed = True
            print("  ИМЕНА KG-* БЕЗ ОПРЕДЕЛЕНИЯ:")
            for n in bad_syms:
                print("    ", n)
        if bad_lambdas:
            failed = True
            print("  ВЫЧИСЛЕННАЯ ЛЯМБДА В ПЕРЕХВАТЕ (в AutoCAD падает "
                  "с «неверная функция: #<SUBR ... -lambda->»):")
            for n in sorted(set(bad_lambdas)):
                print("    ", n)
        if not bad_calls and not bad_syms and not bad_lambdas:
            print("  все имена известны AutoCAD или определены в файле")
    if failed:
        print("Итог: FAILURES")
        return 1
    print("Итог: ALL PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
