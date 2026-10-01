"""
autolisp.py -- минимальный интерпретатор подмножества AutoLISP.

Нужен только для того, чтобы исполнять РЕАЛЬНЫЙ файл RepDblock.lsp на
тестовом стенде (в песочнице нет AutoCAD). Поддерживается ровно то
подмножество, которое используется в RepDblock.lsp:

  специальные формы : defun lambda setq let let* if cond progn while foreach
                      repeat and or quote
  списки            : cons car cdr list append reverse length nth assoc member
                      mapcar apply eval vl-remove-if vl-remove-if-not vl-some
                      vl-every vl-remove vl-position vl-consp vl-list-length
                      vl-sort sort last
  числа/сравнение    : + - * / 1+ 1- = /= < > <= >= equal max min abs fix float
                      itoa atoi rtos
  строки            : strcat strcase substr strlen chr wcmatch
                      vl-string-subst vl-princ-to-string
  прочее            : princ print prompt null not type boundp
                      vl-catch-all-apply vl-catch-all-error-p

Символы нечувствительны к регистру (как в AutoLISP). nil -- единственный
ложный значение; пустой список и nil -- одно и то же.
"""

import os
import re
import sys

sys.setrecursionlimit(20000)


# --------------------------------------------------------------------------- 
# Представление данных
# ---------------------------------------------------------------------------

class Dotted:
    """Точечная пара (a . b), где b не является списком."""
    __slots__ = ("car", "cdr")

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return "(%r . %r)" % (self.car, self.cdr)


import math

SELF_EVALUATING = {
    "PI": math.pi,
    ":VLAX-TRUE": 1,
    ":VLAX-FALSE": None,
    "VLAX-VBSTRING": 8,
    "VLAX-VBDOUBLE": 5,
    "VLAX-VBLONG": 3,
    "VLAX-VBINTEGER": 2,
    "VLAX-TRUE": 1,
    "VLAX-FALSE": None,
}


class LispError(Exception):
    pass


class CatchAllError:
    """Результат vl-catch-all-apply при ошибке."""

    def __init__(self, message):
        self.message = message


NIL = None


def is_list(v):
    return isinstance(v, list)


def sym(name):
    """Символы хранятся в верхнем регистре (регистронезависимость AutoLISP)."""
    return name.upper()


# --------------------------------------------------------------------------- 
# Чтение
# ---------------------------------------------------------------------------

TOKEN_RE = re.compile(r"""\s*(?:
      (?P<comment>;[^\n]*)
    | (?P<string>"(?:\\.|[^"\\])*")
    | (?P<lp>\()
    | (?P<rp>\))
    | (?P<quote>')
    | (?P<dot>\s\.\s)
    | (?P<atom>[^\s()\;]+)
)""", re.VERBOSE)


def tokenize(src):
    toks = []
    pos = 0
    n = len(src)
    while pos < n:
        m = TOKEN_RE.match(src, pos)
        if not m:
            if src[pos].isspace():
                pos += 1
                continue
            raise LispError("Не могу разобрать исходник у позиции %d: %r" % (pos, src[pos:pos + 30]))
        pos = m.end()
        if m.group("comment"):
            continue
        if m.group("string"):
            toks.append(("str", unescape(m.group("string")[1:-1])))
        elif m.group("lp"):
            toks.append(("(", "("))
        elif m.group("rp"):
            toks.append((")", ")"))
        elif m.group("quote"):
            toks.append(("'", "'"))
        elif m.group("dot"):
            toks.append(("dot", "."))
        else:
            toks.append(("atom", m.group("atom")))
    return toks


def unescape(s):
    out = []
    i = 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            nxt = s[i + 1]
            mapping = {"n": "\n", "t": "\t", "e": "\x1b", '"': '"', "\\": "\\",
                       "r": "\r"}
            if nxt in mapping:
                out.append(mapping[nxt])
                i += 2
                continue
            out.append(nxt)
            i += 2
            continue
        out.append(c)
        i += 1
    return "".join(out)


def parse_all(src):
    toks = tokenize(src)
    forms = []
    idx = 0
    while idx < len(toks):
        form, idx = parse_one(toks, idx)
        forms.append(form)
    return forms


def parse_one(toks, idx):
    """Всегда возвращает пару (значение, новый_индекс)."""
    if idx >= len(toks):
        raise LispError("Неожиданный конец файла")
    kind, val = toks[idx]
    if kind == "(":
        idx += 1
        items = []
        while True:
            if idx >= len(toks):
                raise LispError("Незакрытая скобка")
            if toks[idx][0] == ")":
                idx += 1
                break
            if toks[idx][0] == "dot":
                cdr, idx = parse_one(toks, idx + 1)
                if idx >= len(toks) or toks[idx][0] != ")":
                    raise LispError("Ожидалась ')' после точечной пары")
                idx += 1
                return build_list(items, cdr), idx
            item, idx = parse_one(toks, idx)
            items.append(item)
        return build_list(items, None), idx
    if kind == ")":
        raise LispError("Лишняя ')'")
    if kind == "'":
        item, idx = parse_one(toks, idx + 1)
        return [sym("quote"), item], idx
    if kind == "str":
        return ("STR", val), idx + 1
    return atom(val), idx + 1


def build_list(items, cdr):
    """Собрать (возможно точечный) список из элементов и хвоста."""
    if cdr is None:
        return list(items)
    if is_list(cdr):
        return list(items) + cdr
    node = cdr
    for it in reversed(items):
        node = Dotted(it, node)
    return node


def atom(text):
    low = text.lower()
    if low == "nil":
        return NIL
    if low == "t":
        return sym("T")
    # число
    if re.fullmatch(r"[+-]?\d+", text):
        return int(text)
    if re.fullmatch(r"[+-]?(\d+\.\d*|\.\d+|\d+)([eE][+-]?\d+)?", text):
        return float(text)
    return sym(text)


# --------------------------------------------------------------------------- 
# Среда
# ---------------------------------------------------------------------------

class Env(dict):
    def __init__(self, parent=None):
        super().__init__()
        self.parent = parent

    def find(self, name):
        e = self
        while e is not None:
            if name in e:
                return e
            e = e.parent
        return None


class Function:
    def __init__(self, params, locals_, body, env, name=None):
        self.params = params
        self.locals = locals_
        self.body = body
        self.env = env
        self.name = name


class Builtin:
    def __init__(self, fn, name="builtin"):
        self.fn = fn
        self.name = name


# --------------------------------------------------------------------------- 
# Печать в стиле AutoLISP
# ---------------------------------------------------------------------------

def lisp_to_string(v):
    if v is None:
        return "nil"
    if isinstance(v, Keyword):
        return ":" + v.name
    if v is True or v == sym("T"):
        return "T"
    if isinstance(v, str):
        return v
    if isinstance(v, float):
        s = repr(v)
        if "." not in s and "e" not in s and "E" not in s:
            s += ".0"
        return s
    if isinstance(v, int):
        return str(v)
    if isinstance(v, Dotted):
        if v.cdr is None:
            # (a . nil) == (a)
            return "(%s)" % lisp_to_string(v.car)
        return "(%s . %s)" % (lisp_to_string(v.car), lisp_to_string(v.cdr))
    if is_list(v):
        return "(" + " ".join(lisp_to_string(x) for x in v) + ")"
    if isinstance(v, Function):
        return "#<SUBR %s>" % (v.name or "lambda")
    if isinstance(v, Builtin):
        return "#<SUBR %s>" % v.name
    if isinstance(v, CatchAllError):
        return "#<error>"
    return str(v)


# --------------------------------------------------------------------------- 
# Сравнение и равенство
# ---------------------------------------------------------------------------

def num_eq(a, b):
    return float(a) == float(b)


def lisp_equal(a, b):
    if a is None and b is None:
        return True
    if a is None or b is None:
        return False
    if isinstance(a, str) and isinstance(b, str):
        return a.upper() == b.upper()
    if isinstance(a, (int, float)) and isinstance(b, (int, float)) \
            and not isinstance(a, bool) and not isinstance(b, bool):
        return num_eq(a, b)
    if isinstance(a, Dotted) and isinstance(b, Dotted):
        return lisp_equal(a.car, b.car) and lisp_equal(a.cdr, b.cdr)
    if is_list(a) and is_list(b):
        if len(a) != len(b):
            return False
        return all(lisp_equal(x, y) for x, y in zip(a, b))
    return a == b


class Keyword:
    """Ключевое слово AutoCAD: :vlax-true, :vlax-false.

    В AutoLISP это символ, а символ в условии ИСТИНЕН -- в том числе
    :vlax-false. Именно на этом ломались логические флаги COM в
    RepDblock.lsp: (if (vla-get-IsXRef blk) t nil) давал t для любого
    определения. Интерпретатор обязан вести себя так же, иначе тесты
    такую ошибку не ловят.
    """

    def __init__(self, name):
        self.name = name.upper()

    def __repr__(self):
        return ":" + self.name


def truthy(v):
    return v is not None


# --------------------------------------------------------------------------- 
# Преобразования списка
# ---------------------------------------------------------------------------

def to_pylist(v):
    """Точечный список -> (элементы, хвост)."""
    items = []
    cur = v
    while isinstance(cur, Dotted):
        items.append(cur.car)
        cur = cur.cdr
    if is_list(cur):
        items.extend(cur)
        return items, None
    return items, cur


def from_pairs(items, tail):
    if tail is None:
        return list(items)
    node = tail
    for it in reversed(items):
        node = Dotted(it, node)
    return node


# --------------------------------------------------------------------------- 
# Интерпретатор
# ---------------------------------------------------------------------------

class Interpreter:
    def __init__(self):
        self.globals = Env()
        self.output = []
        self._install_builtins()

    # -- вывод -------------------------------------------------------------
    def emit(self, s):
        self.output.append(str(s))

    def get_output(self):
        return "".join(self.output)

    # -- загрузка ----------------------------------------------------------
    def load_string(self, src, filename="<string>"):
        for form in parse_all(src):
            self.evaluate(form, self.globals)

    def load_file(self, path):
        with open(path, encoding="utf-8") as f:
            self.load_string(f.read(), path)

    # -- вычисление --------------------------------------------------------
    def evaluate(self, expr, env):
        if expr is None:
            return NIL
        if isinstance(expr, tuple) and expr and expr[0] == "STR":
            return expr[1]
        if isinstance(expr, str):          # символ
            if expr == "T":
                return sym("T")
            if expr.startswith(":"):
                # ключевое слово само по себе значение, и оно истинно
                return Keyword(expr[1:])
            if expr in SELF_EVALUATING:
                return SELF_EVALUATING[expr]
            e = env.find(expr)
            if e is None:
                raise LispError("не определена переменная: %s" % expr)
            return e[expr]
        if isinstance(expr, Dotted):
            # вызов с точечным списком аргументов не используется
            raise LispError("неожиданная точечная пара в позиции выражения")
        if not is_list(expr):
            return expr
        if not expr:
            return NIL
        head = expr[0]
        if isinstance(head, str):
            if head == "QUOTE":
                # (quote X) -- значение X без вычисления
                val = resolve_quoted(expr[1] if len(expr) > 1 else NIL, env)
                # лямбда-форма становится замыканием в текущем окружении,
                # как в AutoLISP
                if is_list(val) and val and val[0] == "LAMBDA":
                    params, locals_ = split_lambda_list(val[1])
                    return Function(params, locals_, val[2:], env, "lambda")
                return val
            return self.eval_special(head, expr, env)
        fn = self.evaluate(head, env)
        return self.call(fn, [self.evaluate(a, env) for a in expr[1:]], env)

    def eval_special(self, name, expr, env):
        # ---- defun ----
        if name == "DEFUN":
            fname = expr[1]
            args = expr[2]
            params, locals_ = split_lambda_list(args)
            body = expr[3:]
            fn = Function(params, locals_, body, env, fname)
            self.globals[fname] = fn
            # вызов ищет имя в верхнем регистре (sym), поэтому defun
            # обязан класть функцию под тем же ключом: иначе
            # переопределение встроенной функции молча не действует
            self.globals[sym(fname)] = fn
            return fname
        # ---- lambda ----
        if name == "LAMBDA":
            params, locals_ = split_lambda_list(expr[1])
            return Function(params, locals_, expr[2:], env, "lambda")
        # ---- quote ----
        if name == "QUOTE":
            return expr[1]
        # ---- setq ----
        if name == "SETQ":
            val = NIL
            i = 1
            while i < len(expr):
                var = expr[i]
                val = self.evaluate(expr[i + 1], env)
                if not isinstance(var, str):
                    raise LispError("setq: не символ %r" % (var,))
                e = env.find(var)
                if e is None:
                    self.globals[var] = val
                else:
                    e[var] = val
                i += 2
            return val
        # ---- progn ----
        if name == "PROGN":
            val = NIL
            for e in expr[1:]:
                val = self.evaluate(e, env)
            return val
        # ---- if ----
        if name == "IF":
            if truthy(self.evaluate(expr[1], env)):
                return self.evaluate(expr[2], env)
            if len(expr) > 3:
                return self.evaluate(expr[3], env)
            return NIL
        # ---- cond ----
        if name == "COND":
            for clause in expr[1:]:
                test = self.evaluate(clause[0], env)
                if truthy(test):
                    if len(clause) == 1:
                        return test
                    val = NIL
                    for e in clause[1:]:
                        val = self.evaluate(e, env)
                    return val
            return NIL
        # ---- and / or ----
        if name == "AND":
            val = sym("T")
            for e in expr[1:]:
                val = self.evaluate(e, env)
                if not truthy(val):
                    return NIL
            return val
        if name == "OR":
            for e in expr[1:]:
                val = self.evaluate(e, env)
                if truthy(val):
                    return val
            return NIL
        # ---- while ----
        if name == "WHILE":
            val = NIL
            while truthy(self.evaluate(expr[1], env)):
                for e in expr[2:]:
                    val = self.evaluate(e, env)
            return NIL
        # ---- repeat ----
        if name == "REPEAT":
            n = int(self.evaluate(expr[1], env))
            val = NIL
            for _ in range(n):
                for e in expr[2:]:
                    val = self.evaluate(e, env)
            return val
        # ---- foreach ----
        if name == "FOREACH":
            var = expr[1]
            seq = self.evaluate(expr[2], env)
            items, _ = to_pylist(seq)
            val = NIL
            for it in items:
                env[var] = it
                for e in expr[3:]:
                    val = self.evaluate(e, env)
            return val
        # ---- vlax-for ----
        # Обход коллекции ActiveX в Visual LISP:
        #   (vlax-for x coll тело ...) -- тело вычисляется для каждого x.
        # Как и foreach, это специальная форма: без неё аргумент-переменная
        # вычислялся бы до вызова и падал как неопределённое имя.
        if name == "VLAX-FOR":
            var = expr[1]
            coll = self.evaluate(expr[2], env)
            # коллекция ActiveX в стенде -- любой итерируемый объект
            try:
                items = list(coll)
            except TypeError:
                items = []
            val = NIL
            for it in items:
                env[var] = it
                for e in expr[3:]:
                    val = self.evaluate(e, env)
            return val
        # ---- let / let* ----
        if name in ("LET", "LET*"):
            bindings = expr[1]
            newenv = Env(env) if name == "LET" else env
            for b in bindings:
                if is_list(b):
                    v = b[0]
                    val = self.evaluate(b[1], newenv) if len(b) > 1 else NIL
                else:
                    v, val = b, NIL
                newenv[v] = val
            val = NIL
            for e in expr[2:]:
                val = self.evaluate(e, newenv)
            return val
        # ---- vl-catch-all-apply ----
        if name == "VL-CATCH-ALL-APPLY":
            fn = self.evaluate(expr[1], env)
            args = expr[2] if len(expr) > 2 else []
            argvals, _ = to_pylist(self.evaluate(args, env))
            try:
                return self.call(fn, argvals, env)
            except Exception as ex:                       # noqa: BLE001
                return CatchAllError(str(ex))
        # обычный вызов
        fn = self.evaluate(sym(name), env) if isinstance(name, str) else name
        return self.call(fn, [self.evaluate(a, env) for a in expr[1:]], env)

    # -- вызов функции -----------------------------------------------------
    def call(self, fn, args, env):
        # 1. приводим вызываемое значение к функции
        #    'имя-функции  -- как в (mapcar 'KG-StrKey ...)
        if isinstance(fn, str):
            e = env.find(fn)
            fn = e[fn] if e is not None else self.globals.get(fn)
        #    лямбда-форма, полученная через '(lambda ...) -- как в AutoLISP,
        #    такая форма сама по себе является вызываемым значением
        if is_list(fn) and fn and fn[0] == "LAMBDA":
            params, locals_ = split_lambda_list(fn[1])
            fn = Function(params, locals_, fn[2:], env, "lambda")
        # 2. вызов
        if isinstance(fn, Builtin):
            return fn.fn(*args)
        if isinstance(fn, Function):
            newenv = Env(fn.env)
            for p, a in zip(fn.params, args):
                newenv[p] = a
            for l in fn.locals:
                newenv[l] = NIL
            val = NIL
            for e in fn.body:
                val = self.evaluate(e, newenv)
            return val
        raise LispError("не функция: %r" % (fn,))

    # -- встроенные --------------------------------------------------------
    def define(self, name, fn):
        self.globals[sym(name)] = Builtin(fn, name)

    def _install_builtins(self):
        d = self.define

        # --- базовые ---
        d("princ", lambda *a: (self.emit(lisp_to_string(a[0]) if a else ""), NIL)[1])
        d("print", lambda *a: (self.emit("\n" + (lisp_to_string(a[0]) if a else "")), NIL)[1])
        d("prompt", lambda *a: (self.emit(lisp_to_string(a[0]) if a else ""), NIL)[1])
        d("vl-princ-to-string", lambda v="": lisp_to_string(v))
        d("null", lambda v=NIL: sym("T") if v is None else NIL)
        d("not", lambda v=NIL: sym("T") if v is None else NIL)
        d("boundp", lambda s: sym("T") if self.globals.find(s) is not None else NIL)
        d("type", lambda v: ("STR" if isinstance(v, str) else
                             "INT" if isinstance(v, int) else
                             "REAL" if isinstance(v, float) else
                             "LIST" if (is_list(v) or v is None or isinstance(v, Dotted)) else
                             "SYM"))
        d("eval", lambda x: self.evaluate(x, self.globals))
        d("vl-load-com", lambda: sym("T"))

        # --- арифметика ---
        d("+", lambda *a: _add(a))
        d("-", lambda *a: _sub(a))
        d("*", lambda *a: _mul(a))
        d("/", lambda *a: _div(a))
        d("1+", lambda x: x + 1)
        d("1-", lambda x: x - 1)
        d("abs", abs)
        d("max", lambda *a: max(a))
        d("min", lambda *a: min(a))
        d("fix", lambda x: int(x))
        d("float", lambda x: float(x))
        def _itoa(x):
            # AutoCAD: (itoa "1.5") -> "неверный тип аргумента: fixnump"
            if isinstance(x, bool) or not isinstance(x, (int, float)):
                raise LispError('неверный тип аргумента: fixnump %r' % (x,))
            return str(int(x))

        d("itoa", _itoa)
        d("atoi", _atoi)
        d("rtos", lambda x, *rest: ("%.6f" % float(x)).rstrip("0").rstrip(".")
          if rest and rest[0] == 2 and len(rest) > 1 and rest[1] == 0
          else ("%%.%df" % int(rest[1]) % float(x)) if rest and len(rest) > 1
          else ("%g" % float(x)))

        # --- сравнение ---
        d("=", lambda a, b: sym("T") if lisp_equal(a, b) else NIL)
        d("/=", lambda a, b: NIL if lisp_equal(a, b) else sym("T"))
        d("equal", lambda a, b, *r: sym("T") if lisp_equal(a, b) else NIL)
        # Сравнение чисел. В AutoCAD сравнение со строкой не «молча
        # сравнивает как Python», а падает: (< "1.5" "1.1") ->
        # "неверный тип аргумента: fixnump: 1.5". Именно так на реальном
        # чертеже упал (sort its '<) в KG-ScanFamily, а интерпретатор
        # этого не замечал, потому что Python строки сравнивать умеет.
        import operator as _op

        def _cmp(op, a, b):
            for v in (a, b):
                if isinstance(v, bool) or not isinstance(v, (int, float)):
                    raise LispError('неверный тип аргумента: fixnump %r' % (v,))
            return sym("T") if op(a, b) else NIL

        d("<", lambda a, b: _cmp(_op.lt, a, b))
        d(">", lambda a, b: _cmp(_op.gt, a, b))
        d("<=", lambda a, b: _cmp(_op.le, a, b))
        d(">=", lambda a, b: _cmp(_op.ge, a, b))

        # --- списки ---
        d("cons", _cons)
        d("car", _car)
        d("cdr", _cdr)
        d("list", lambda *a: list(a))
        d("append", _append)
        d("reverse", _reverse)
        d("length", _length)
        d("nth", lambda n, lst: (to_pylist(lst)[0][n] if 0 <= n < len(to_pylist(lst)[0]) else NIL))
        d("last", lambda lst: (to_pylist(lst)[0][-1] if to_pylist(lst)[0] else NIL))
        d("assoc", _assoc)
        d("member", _member)
        d("vl-consp", lambda v: sym("T") if (isinstance(v, Dotted) or (is_list(v) and v)) else NIL)
        d("vl-list-length", lambda v: len(to_pylist(v)[0]))
        d("vl-position", _vl_position)
        d("vl-remove", lambda x, lst: [e for e in to_pylist(lst)[0] if not lisp_equal(e, x)])
        d("vl-remove-if", _vl_remove_if(self))
        d("vl-remove-if-not", _vl_remove_if_not(self))
        d("vl-some", _vl_some(self))
        d("vl-every", _vl_every(self))
        d("mapcar", _mapcar(self))
        d("apply", _apply(self))
        # sort намеренно НЕ определён: в AutoCAD такой функции нет,
        # есть vl-sort / vl-sort-i / acad_strlsort
        d("vl-sort", _vl_sort(self))
        d("subst", lambda new, old, lst: [new if lisp_equal(e, old) else e for e in to_pylist(lst)[0]])

        # --- строки ---
        d("strcat", lambda *a: "".join("" if x is None else _as_str(x) for x in a))
        d("strcase", lambda s, *r: (_as_str(s).lower() if r and r[0] == 1
                                    else _as_str(s).upper()))
        d("substr", _substr)
        d("strlen", lambda *a: sum(len(x or "") for x in a))
        d("chr", lambda n: chr(int(n)))
        # make-string намеренно НЕ определён: в AutoCAD такой функции нет
        d("wcmatch", _wcmatch)
        d("vl-string-subst", lambda new, old, s: (s or "").replace(old, new))
        d("vl-string-search", lambda pat, s, start=0: ((s or "")[int(start or 0):].find(pat) + int(start or 0)) if (pat in (s or "")[int(start or 0):]) else NIL)

        # --- ошибки ---
        d("vl-catch-all-error-p", lambda v=NIL: sym("T") if isinstance(v, CatchAllError) else NIL)
        d("vl-catch-all-error-message", lambda v=NIL: v.message if isinstance(v, CatchAllError) else "")

        # --- COM / variants ---
        d("vlax-make-variant", lambda val=None, vtype=None: val)
        d("vlax-variant-value", lambda val=None: val)
        d("vlax-put-property", lambda obj, prop, val: val)
        d("vlax-get-property", lambda obj, prop: getattr(obj, str(prop).lower(), NIL))
        d("vlax-put", lambda obj, prop, val: val)
        d("vlax-get", lambda obj, prop: getattr(obj, str(prop).lower(), NIL))


# --------------------------------------------------------------------------- 
# Реализации встроенных
# ---------------------------------------------------------------------------

def _add(a):
    vals = [x for x in a if x is not None]
    if not vals:
        return 0
    if all(isinstance(x, int) for x in vals):
        return sum(vals)
    return float(sum(vals))


def _sub(a):
    vals = [x for x in a if x is not None]
    if not vals:
        return 0
    if len(vals) == 1:
        return -vals[0]
    r = vals[0]
    for x in vals[1:]:
        r -= x
    return r


def _mul(a):
    vals = [x for x in a if x is not None]
    if not vals:
        return 1
    r = 1
    for x in vals:
        r *= x
    return r


def _div(a):
    r = a[0]
    for x in a[1:]:
        r = r / x
    if isinstance(r, float) and r.is_integer() and all(isinstance(x, int) for x in a):
        return int(r)
    return r


def _car(v):
    if isinstance(v, Dotted):
        return v.car
    if is_list(v) and v:
        return v[0]
    return NIL


def _cdr(v):
    if isinstance(v, Dotted):
        return v.cdr
    if is_list(v) and v:
        return v[1:]
    return NIL


def _as_str(v):
    """Строковое представление значения (не-строки приводим, как AutoLISP)."""
    if v is None:
        return ""
    if isinstance(v, str):
        return v
    return lisp_to_string(v)


def _atoi(s):
    m = re.match(r"\s*([+-]?\d+)", s or "")
    return int(m.group(1)) if m else 0


def _cons(a, b):
    # (cons x nil) -> (x) ; (cons x список) -> добавление в голову ;
    # иначе -- точечная пара (x . b). Пустой список и nil в AutoLISP --
    # одно и то же, поэтому отдельно его проверять нельзя.
    if is_list(b):
        return [a] + b
    return Dotted(a, b)


def _append(*args):
    out = []
    tail = None
    for i, a in enumerate(args):
        if a is None:
            continue
        if is_list(a):
            out.extend(a)
        elif isinstance(a, Dotted):
            items, t = to_pylist(a)
            out.extend(items)
            if i == len(args) - 1:
                tail = t
        else:
            out.append(a)
    return _nil_or(from_pairs(out, tail) if tail is not None else out)


def _nil_or(v):
    """В AutoLISP пустой список и nil -- одно и то же.

    Без этого (reverse nil) возвращал «пустой список», который в Python
    отличается от nil по истинности, и (not (reverse nil)) давало nil
    вместо T. На реальном AutoCAD такое расхождение молча меняет ветку.
    """
    if isinstance(v, list) and not v:
        return NIL
    return v


def _reverse(v):
    items, tail = to_pylist(v)
    items = list(reversed(items))
    return _nil_or(from_pairs(items, tail))


def _length(v):
    if v is None:
        return 0
    if isinstance(v, str):
        return len(v)
    return len(to_pylist(v)[0])


def _assoc(key, alst):
    items, _ = to_pylist(alst)
    for e in items:
        if isinstance(e, Dotted):
            if lisp_equal(e.car, key):
                return e
        elif is_list(e) and e:
            if lisp_equal(e[0], key):
                return e
    return NIL


def _member(x, lst):
    items, _ = to_pylist(lst)
    for i, e in enumerate(items):
        if lisp_equal(e, x):
            return items[i:]
    return NIL


def _vl_position(x, lst):
    items, _ = to_pylist(lst)
    for i, e in enumerate(items):
        if lisp_equal(e, x):
            return i
    return NIL


def _vl_remove_if(interp):
    def f(pred, lst):
        items, _ = to_pylist(lst)
        return _nil_or([e for e in items
                        if not truthy(interp.call(pred, [e], interp.globals))])
    return f


def _vl_remove_if_not(interp):
    def f(pred, lst):
        items, _ = to_pylist(lst)
        return _nil_or([e for e in items
                        if truthy(interp.call(pred, [e], interp.globals))])
    return f


def _vl_some(interp):
    def f(pred, *lists):
        pylists = [to_pylist(l)[0] for l in lists]
        for combo in zip(*pylists):
            r = interp.call(pred, list(combo), interp.globals)
            if truthy(r):
                return r
        return NIL
    return f


def _vl_every(interp):
    def f(pred, *lists):
        pylists = [to_pylist(l)[0] for l in lists]
        for combo in zip(*pylists):
            if not truthy(interp.call(pred, list(combo), interp.globals)):
                return NIL
        return sym("T")
    return f


def _mapcar(interp):
    def f(fn, *lists):
        pylists = [to_pylist(l)[0] for l in lists]
        return _nil_or([interp.call(fn, list(combo), interp.globals)
                        for combo in zip(*pylists)])
    return f


def _apply(interp):
    def f(fn, args):
        argvals, _ = to_pylist(args)
        return interp.call(fn, argvals, interp.globals)
    return f


def _sort(interp):
    import functools

    def f(lst, cmpfn):
        items, _ = to_pylist(lst)

        # Компаратор AutoLISP возвращает T/nil, а не -1/0/1:
        # (f a b) = T -> a раньше b, иначе проверяем обратный порядок.
        # Без этого sort с предикатом «строго меньше» давал мусор.
        def key(a, b):
            if truthy(interp.call(cmpfn, [a, b], interp.globals)):
                return -1
            if truthy(interp.call(cmpfn, [b, a], interp.globals)):
                return 1
            return 0

        return sorted(items, key=functools.cmp_to_key(key))
    return f


def _vl_sort(interp):
    f = _sort(interp)
    return f


def _substr(s, start, *rest):
    s = s or ""
    start = int(start)
    if start < 1:
        start = 1
    if start > len(s):
        return ""
    if rest:
        n = int(rest[0])
        return s[start - 1:start - 1 + n]
    return s[start - 1:]


# --------------------------------------------------------------------------- 
# wcmatch -- подмножество: * ? , [] ~ ` (экранирование)
# ---------------------------------------------------------------------------

def _wcmatch(s, pattern):
    if s is None:
        s = ""
    s = s.upper()
    for alt in split_wcmatch_alternatives(pattern.upper()):
        if wcmatch_single(s, alt):
            return sym("T")
    return NIL


def split_wcmatch_alternatives(pattern):
    """Разбор по ',' вне квадратных скобок."""
    out = []
    cur = []
    depth = 0
    i = 0
    while i < len(pattern):
        c = pattern[i]
        if c == "`" and i + 1 < len(pattern):
            cur.append(c)
            cur.append(pattern[i + 1])
            i += 2
            continue
        if c == "[":
            depth += 1
        elif c == "]":
            depth = max(0, depth - 1)
        if c == "," and depth == 0:
            out.append("".join(cur))
            cur = []
            i += 1
            continue
        cur.append(c)
        i += 1
    out.append("".join(cur))
    return out


def wcmatch_single(s, pattern):
    """Рекурсивное сопоставление с '*', '?', '[...]', '`'."""
    return _match(s, 0, pattern, 0)


def _match(s, si, p, pi):
    while pi < len(p):
        c = p[pi]
        if c == "`" and pi + 1 < len(p):
            if si >= len(s) or s[si] != p[pi + 1]:
                return False
            si += 1
            pi += 2
            continue
        if c == "*":
            # жадный перебор
            pi += 1
            while pi < len(p) and p[pi] == "*":
                pi += 1
            if pi >= len(p):
                return True
            for k in range(si, len(s) + 1):
                if _match(s, k, p, pi):
                    return True
            return False
        if c == "?":
            if si >= len(s):
                return False
            si += 1
            pi += 1
            continue
        if c == "[":
            end = p.find("]", pi + 1)
            if end < 0:
                if si >= len(s) or s[si] != "[":
                    return False
                si += 1
                pi += 1
                continue
            charset = p[pi + 1:end]
            negate = charset.startswith("~")
            if negate:
                charset = charset[1:]
            members = set()
            i = 0
            while i < len(charset):
                if charset[i] == "`" and i + 1 < len(charset):
                    members.add(charset[i + 1])
                    i += 2
                    continue
                if i + 2 < len(charset) and charset[i + 1] == "-":
                    for code in range(ord(charset[i]), ord(charset[i + 2]) + 1):
                        members.add(chr(code))
                    i += 3
                    continue
                members.add(charset[i])
                i += 1
            if si >= len(s):
                return False
            hit = s[si] in members
            if hit == negate:
                return False
            si += 1
            pi = end + 1
            continue
        if si >= len(s) or s[si] != c:
            return False
        si += 1
        pi += 1
    return si >= len(s)


def resolve_quoted(v, env=None):
    """Превращает разобранное литеральное значение в данные.

    Строковые литералы в разборе хранятся как ("STR", текст); внутри
    (quote ...) их надо развернуть в обычные строки. Лямбда-формы
    возвращаются как есть -- их отдельно превращают в замыкание.
    """
    if isinstance(v, tuple) and len(v) == 2 and v[0] == "STR":
        return v[1]
    if isinstance(v, Dotted):
        return Dotted(resolve_quoted(v.car, env), resolve_quoted(v.cdr, env))
    if is_list(v):
        if v and v[0] == "LAMBDA":
            return v
        return [resolve_quoted(x, env) for x in v]
    return v


def split_lambda_list(args):
    """Разбор (a b / c d) -> ([a b], [c d])."""
    params = []
    locals_ = []
    seen_slash = False
    for a in args:
        if isinstance(a, str) and a == sym("/"):
            seen_slash = True
            continue
        (locals_ if seen_slash else params).append(a)
    return params, locals_


# --------------------------------------------------------------------------- 
# Удобная обёртка
# ---------------------------------------------------------------------------

def run_file(path, prelude=None, setup=None):
    interp = Interpreter()
    if prelude:
        interp.load_string(prelude)
    if setup:
        setup(interp)
    interp.load_file(path)
    return interp


def call(interp, expr_src):
    for form in parse_all(expr_src):
        result = interp.evaluate(form, interp.globals)
    return result


def s(interp, expr_src):
    """Вычислить и вернуть результат в виде строки AutoLISP."""
    return lisp_to_string(call(interp, expr_src))
