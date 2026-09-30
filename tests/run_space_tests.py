#!/usr/bin/env python3
"""
run_space_tests.py -- прогон РЕАЛЬНЫХ функций раздела 8, которые отвечают за
определение пространства вхождения (модель / лист).

Зачем отдельный прогон: tests/run_tests.py грузит Integration.lsp с
KG-TESTING = T, то есть раздел 8 (исполняющий слой AutoCAD) не загружается
вообще, и правки в нём тестами не исполнялись. Именно в разделе 8 жила
ошибка с группой 330, из-за которой на реальном чертеже не читалось ни одно
вхождение.

Здесь KG-TESTING не устанавливается, примитивы COM подменяются заглушками
с фальшивым чертежом (модель, два листа, одно определение с вложенным
вхождением), и исполняются настоящие KG-BuildSpaceMap, KG-SpaceOfHandle,
KG-SpaceOf, KG-HandleOf, KG-OwnerBlockName, KG-RecordNameOf.

Запуск:  python3 tests/run_space_tests.py
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from autolisp import (Interpreter, LispError, call, lisp_to_string,  # noqa: E402
                      Dotted, Keyword)


# --- фальшивый чертёж -------------------------------------------------------

class VTrue(object):
    """Истина из заглушки COM.

    Python-True интерпретатор видит как 1, а Python-None -- как nil,
    поэтому возвращать их из заглушек нельзя: (KG-IsErr nil) дал бы T.
    """


VT = VTrue()


class Ename(object):
    """Аналог ename: не строка и не список, чтобы guard
    «это строка, а не ename» в Integration.lsp не срабатывал."""

    def __init__(self, key):
        self.key = key

    def __str__(self):
        return self.key


class FakeEnt(object):
    """Объект пространства: только handle, как у AcDbEntity."""

    def __init__(self, handle):
        self._handle = handle

    def get_Handle(self):
        return self._handle


class FullRef(object):
    """Вхождение, у которого читаются все свойства."""

    def __init__(self, handle, eff):
        self._handle = handle
        self._eff = eff

    def get_Handle(self):
        return self._handle

    def get_EffectiveName(self):
        return self._eff

    def get_Name(self):
        return "*U63"

    def get_InsertionPoint(self):
        # variant, а не список -- как в настоящем AutoCAD
        return FakeVariant([1250.0, -300.0, 15.0])

    def get_Layer(self):
        return "КОНСТРУКЦИИ"

    def get_Rotation(self):
        return 1.5708

    def get_XScaleFactor(self):
        return 2.0

    def get_YScaleFactor(self):
        return 2.0

    def get_ZScaleFactor(self):
        return 2.0

    def __iter__(self):
        return iter([])


class BrokenRef(object):
    """Вхождение, у которого не читается ничего, кроме handle.

    Так ведёт себя часть объектов на реальном чертеже: раньше любое
    нечитаемое свойство обрывало чтение всех вхождений.
    """

    def __init__(self, handle):
        self._handle = handle

    def get_Handle(self):
        return self._handle

    def __iter__(self):
        return iter([])


class FakeVariant(object):
    """Variant из COM: разворачивается только через vlax-variant-value.

    Именно так приходит InsertionPoint; (nth …) над ним в AutoCAD даёт
    «неверный тип аргумента: consp #<variant …>».
    """

    def __init__(self, items):
        self.items = items


class FakeLayout(object):
    def __init__(self, name):
        self._name = name

    def get_Name(self):
        return self._name


class FakeBlock(object):
    """Запись блока (пространство или определение)."""

    def __init__(self, name, ents=None, layout=None, isxref=False):
        self._name = name
        self._ents = ents or []
        self._layout = layout
        self._isxref = isxref

    def get_Name(self):
        return self._name

    def get_IsXRef(self):
        # AutoCAD возвращает :vlax-true / :vlax-false, и ОБА символа в
        # условии истинны. None вместо :vlax-false делал стенд добрее
        # настоящего AutoCAD -- так и прошла ошибка, из-за которой все
        # определения считались внешними ссылками.
        return Keyword("VLAX-TRUE") if self._isxref else Keyword("VLAX-FALSE")

    def get_Layout(self):
        # у определения листа без объекта Layout -- как при ошибке чтения
        if self._layout is None:
            raise LispError("нет объекта Layout")
        return self._layout

    def put_Name(self, v):
        self._name = v

    def Item(self, handle):
        for e in self._ents:
            if e.get_Handle().upper() == str(handle).upper():
                return e
        raise LispError("нет объекта с handle %s" % handle)

    def __iter__(self):
        return iter(self._ents)


class FakeBlockTable(object):
    """Таблица блоков: Item по имени и пополнение при копировании."""

    def __init__(self, blocks):
        self._blocks = list(blocks)

    def Item(self, name):
        for b in self._blocks:
            if b.get_Name().upper() == str(name).upper():
                return b
        raise LispError("нет определения %s" % name)

    def add(self, blk):
        self._blocks.append(blk)

    def names(self):
        return [b.get_Name() for b in self._blocks]

    def __iter__(self):
        return iter(self._blocks)


class FakeDoc(object):
    """Документ (обычный или ObjectDBX).

    CopyObjects намеренно БЕЗ return: в AutoCAD это метод без
    возвращаемого значения, и vlax-invoke от него даёт nil. Стенд,
    который возвращал бы значение, не поймал бы ошибку «код проверял
    результат void-метода» -- именно так вариант создавался статическим.
    """

    def __init__(self, table):
        self._table = table

    def get_Blocks(self):
        return self._table

    def CopyObjects(self, objs, target):
        for o in objs:
            target.add(FakeBlock(o.get_Name(), list(o)))


# ename экземпляров (для KG-SpaceOf / KG-HandleOf)
EN_MODEL = Ename("E18A1")
EN_L1 = Ename("E6070")
EN_L2 = Ename("E625B")
EN_NESTED = Ename("E7A01")
EN_UNKNOWN = Ename("EFFFF")

# «трудные» вхождения: полное и почти нечитаемое
ENT_FULL = FullRef("78BC", "Комплект КП50 v1.4 стойка КП45303-2")
ENT_BROKEN = BrokenRef("7B33")

# экземпляр в модели, на Лист1, на Лист2 и вложенный в определение
ENT_MODEL = FakeEnt("18A1")
ENT_L1 = FakeEnt("6070")
ENT_L2 = FakeEnt("625B")
ENT_NESTED = FakeEnt("7A01")

DRAW_TABLE = None   # заполняется после BLOCKS
DOC = None
DBX_DOC = None

BLOCKS = [
    FakeBlock("*Model_Space", [ENT_MODEL, ENT_FULL, ENT_BROKEN]),
    FakeBlock("*Paper_Space", [ENT_L1], FakeLayout("Лист1")),
    FakeBlock("*Paper_Space0", [ENT_L2], FakeLayout("Лист2")),
    # лист без объекта Layout -- имя должно остаться именем блока
    FakeBlock("*Paper_Space1", [FakeEnt("7B02")]),
    FakeBlock("Комплект КП50 v1.5", [ENT_NESTED]),
    FakeBlock("ВнешняяСсылка", [FakeEnt("7C03")], isxref=True),
]

DRAW_TABLE = FakeBlockTable(BLOCKS)
DOC = FakeDoc(DRAW_TABLE)
DBX_DOC = FakeDoc(FakeBlockTable([]))

# entget: группа 5 есть у всех, группа 330 -- НИ У КОГО (как в реальном файле)
def dx(*pairs):
    """Список групп DXF из точечных пар -- как возвращает entget."""
    return [Dotted(k, v) for k, v in pairs]


ENTGET = {
    "E18A1": dx((5, "18A1"), (0, "INSERT"), (2, "*U63")),
    "E78BC": dx((5, "78BC"), (0, "INSERT"), (2, "*U63")),
    "E7B33": dx((5, "7B33"), (0, "INSERT"), (2, "Слои и стили для шаблона")),
    "E6070": dx((5, "6070"), (0, "INSERT"), (2, "*U63")),
    "E625B": dx((5, "625B"), (0, "INSERT"),
                (2, "Комплект КП50 v1.4 стойка КП45303-2")),
    "E7A01": dx((5, "7A01"), (0, "INSERT"), (2, "Стойка КП50")),
    # запись блока: группа 2 -- её имя
    # запись блока *U63, на которую указывает handent
    "RECBLOCK": dx((2, "*U63")),
}

PASSED = [0]
FAILED = [0]
FAILNAMES = []


def check(name, got, want):
    if lisp_to_string(got) == lisp_to_string(want):
        PASSED[0] += 1
        print("  ok   %s" % name)
    else:
        FAILED[0] += 1
        FAILNAMES.append(name)
        print("  FAIL %s: получено %s, ожидалось %s"
              % (name, lisp_to_string(got), lisp_to_string(want)))


def build_interp():
    interp = Interpreter()

    interp.define("vlax-get-acad-object", lambda: "ACAD")
    interp.define("vla-get-ActiveDocument", lambda a: DOC)
    interp.define("vla-get-Blocks", lambda d: d.get_Blocks())
    interp.define("vla-Item", lambda col, key: col.Item(key))
    interp.define("vla-put-Name", lambda o, v: o.put_Name(v))
    interp.define("vlax-release-object", lambda o: None)
    interp.define("vla-GetInterfaceObject", lambda a, p: DBX_DOC)
    interp.define("getvar", lambda n: "24.2s (LMS Tech)")

    # символ-аргумент интерпретатор приводит к верхнему регистру,
    # поэтому имя метода ищем без учёта регистра
    def attr(obj, name):
        want = str(name).upper()
        for a in dir(obj):
            if a.upper() == want:
                return getattr(obj, a)
        raise LispError("нет свойства/метода %s" % name)

    def get_property(obj, prop, *rest):
        return attr(obj, "get_" + str(prop))()

    def invoke_method(obj, meth, *args):
        return attr(obj, str(meth))(*args)

    interp.define("vlax-get-property", get_property)
    # форма vla-get-XXX -- отдельная функция, а не свойство
    for pname in ("Handle", "Name", "IsXRef", "IsDynamicBlock", "Layout",
                  "ObjectName", "EffectiveName", "Layer"):
        interp.define("vla-get-" + pname,
                      (lambda nm: (lambda o: attr(o, "get_" + nm)()))(pname))
    interp.define("vlax-invoke-method", invoke_method)
    interp.define("vlax-invoke", invoke_method)
    # ename в стенде -- символ (в AutoLISP ename не строка, и guard
    # «это строка, а не ename» в Integration.lsp на строке сработал бы)
    interp.define("entget", lambda e: ENTGET.get(str(e).upper()))
    interp.define("vlax-vla-object->ename", lambda o: Ename("E" + o.get_Handle()))
    interp.define("vlax-variant-value",
                  lambda v: v.items if isinstance(v, FakeVariant) else v)
    interp.define("vlax-safearray-get-u-bound", lambda a, d: len(a) - 1)
    interp.define("vlax-safearray-get-element", lambda a, i: a[i])
    interp.define("vlax-property-available-p", lambda o, p: True)
    interp.define("GetDynamicBlockProperties", lambda o: [])
    interp.define("handent", lambda h: Ename("RECBLOCK")
                  if str(h).upper() == "63" else None)
    interp.define("ssget", lambda *a: None)
    return interp


def fresh_world():
    """Пересоздать фейковый чертёж: FakeBlock изменяемы, а M-серия их
    переименовывает, поэтому перед новым прогоном мир собирается заново."""
    global BLOCKS, DRAW_TABLE, DOC, DBX_DOC
    BLOCKS = [
        FakeBlock("*Model_Space", [ENT_MODEL, ENT_FULL, ENT_BROKEN]),
        FakeBlock("*Paper_Space", [ENT_L1], FakeLayout("Лист1")),
        FakeBlock("*Paper_Space0", [ENT_L2], FakeLayout("Лист2")),
        FakeBlock("*Paper_Space1", [FakeEnt("7B02")]),
        FakeBlock("Комплект КП50 v1.5", [ENT_NESTED]),
        FakeBlock("ВнешняяСсылка", [FakeEnt("7C03")], isxref=True),
    ]
    DRAW_TABLE = FakeBlockTable(BLOCKS)
    DOC = FakeDoc(DRAW_TABLE)
    DBX_DOC = FakeDoc(FakeBlockTable([]))


def run_deepcopy_checks():
    """Глубокое копирование определения через ObjectDBX.

    Прогон ОТДЕЛЬНЫМ интерпретатором: M-серия переопределяет
    KG_EXDefExists заглушкой, и вместе с ней проверялся бы не настоящий
    код раздела 8.
    """
    fresh_world()
    interp = build_interp()
    interp.load_file(os.path.join(ROOT, "Integration.lsp"))
    print()
    print("Глубокое копирование определения (свежий интерпретатор)")

    # Document.CopyObjects и Database.CopyObjects -- методы БЕЗ
    # возвращаемого значения, vlax-invoke от них даёт nil. Код проверял
    # результат вызова и поэтому никогда не шёл дальше первого
    # копирования: вариант создавался статическим, а KG-DBX-ERR оставался
    # пустым, и отчёт печатал «причина не сообщена».
    call(interp, "(setq KG-DBX-ERR nil)")
    check("M25 глубокое копирование вернуло успех",
          lisp_to_string(call(
              interp,
              '(KG_EXCopyDefDeep "Комплект КП50 v1.5" '
              '"Комплект КП50 v1.51 КП45387")')),
          "T")
    check("M26 причин отказа нет",
          lisp_to_string(call(interp, "KG-DBX-ERR")), "nil")
    check("M27 новое определение появилось в чертеже",
          lisp_to_string(call(
              interp, '(KG_EXDefExists "Комплект КП50 v1.51 КП45387")')),
          "T")
    check("M28 исходное определение осталось на месте",
          lisp_to_string(call(interp, '(KG_EXDefExists "Комплект КП50 v1.5")')),
          "T")


def main():
    interp = build_interp()
    path = os.path.join(ROOT, "Integration.lsp")
    try:
        interp.load_file(path)
    except LispError as exc:
        print("ОШИБКА ЗАГРУЗКИ Integration.lsp:", exc)
        return 2

    print("Integration.lsp загружен БЕЗ KG-TESTING: раздел 8 определён.")
    print("Прогон реальных функций определения пространства\n")

    # карта строится настоящей KG-BuildSpaceMap
    call(interp, "(setq KG-SPACE-MAP (KG-BuildSpaceMap))")
    print("Построенная карта handle -> пространство:")
    print("   " + lisp_to_string(call(interp, "KG-SPACE-MAP")))
    print()

    check("S1 экземпляр модели", call(interp, '(KG-SpaceOfHandle "18A1")'), "Model")
    check("S2 экземпляр Лист1", call(interp, '(KG-SpaceOfHandle "6070")'), "Лист1")
    check("S3 экземпляр Лист2", call(interp, '(KG-SpaceOfHandle "625B")'), "Лист2")
    check("S3.1 handle в нижнем регистре", call(interp, '(KG-SpaceOfHandle "625b")'),
          "Лист2")
    check("S4 вложенное вхождение не получает пространство",
          call(interp, '(KG-SpaceOfHandle "7A01")'), None)
    check("S5 handle из внешней ссылки не попадает в карту",
          call(interp, '(KG-SpaceOfHandle "7C03")'), None)
    check("S6 регистр handle не важен",
          call(interp, '(KG-SpaceOfHandle "6070")'), "Лист1")
    check("S7 нечисловой handle не роняет поиск",
          call(interp, '(KG-SpaceOfHandle 15)'), None)
    interp.globals["EN_MODEL"] = EN_MODEL
    interp.globals["EN_L1"] = EN_L1
    interp.globals["EN_L2"] = EN_L2
    interp.globals["EN_NESTED"] = EN_NESTED
    interp.globals["EN_UNKNOWN"] = EN_UNKNOWN
    interp.globals["EN_FULL"] = Ename("E78BC")
    interp.globals["EN_BROKEN"] = Ename("E7B33")
    interp.globals["OBJ_FULL"] = ENT_FULL
    interp.globals["OBJ_BROKEN"] = ENT_BROKEN
    interp.globals["VAR3"] = FakeVariant([1.5, -2.5, 3.5])

    check("S8 KG-SpaceOf по ename модели",
          call(interp, "(KG-SpaceOf EN_MODEL)"), "Model")
    check("S9 KG-SpaceOf по ename листа",
          call(interp, "(KG-SpaceOf EN_L1)"), "Лист1")
    check("S10 KG-SpaceOf для вложенного -- nil, а не Model",
          call(interp, "(KG-SpaceOf EN_NESTED)"), None)
    check("S11 KG-SpaceOf для неизвестного ename -- nil",
          call(interp, "(KG-SpaceOf EN_UNKNOWN)"), None)
    check("S12 KG-HandleOf читает группу 5",
          call(interp, "(KG-HandleOf EN_L2)"), "625B")
    check("S13 KG-HandleOf на не-ename не падает",
          call(interp, '(KG-HandleOf nil)'), None)
    check("S14 группа 330 отсутствует -- KG-OwnerBlockName nil",
          call(interp, "(KG-OwnerBlockName EN_MODEL)"), None)
    check("S15 KG-RecordNameOf по handle записи",
          call(interp, '(KG-RecordNameOf "63")'), "*U63")
    check("S16 KG-RecordNameOf на неизвестном handle",
          call(interp, '(KG-RecordNameOf "FF")'), None)

    # --- чтение модели экземпляра: то, что падало на каждом вхождении ---
    print("Модель «трудного» вхождения (не читается ничего, кроме handle):")
    print("   " + lisp_to_string(call(interp, "(KG-InstanceModel OBJ_BROKEN EN_BROKEN)")))
    m = call(interp, "(KG-InstanceModel OBJ_BROKEN EN_BROKEN)")
    check("M1 модель построена, а не nil", (m is not None), True)
    check("M2 эффективное имя взято из группы 2",
          call(interp, '(KG-CdrCI "eff" (KG-InstanceModel OBJ_BROKEN EN_BROKEN))'),
          "Слои и стили для шаблона")
    check("M3 слой по умолчанию",
          call(interp, '(KG-CdrCI "layer" (KG-InstanceModel OBJ_BROKEN EN_BROKEN))'), "0")
    check("M4 масштаб по умолчанию",
          call(interp, '(KG-CdrCI "sx" (KG-InstanceModel OBJ_BROKEN EN_BROKEN))'), 1.0)
    check("M5 точка вставки по умолчанию",
          lisp_to_string(call(interp,
              '(KG-CdrCI "pos" (KG-InstanceModel OBJ_BROKEN EN_BROKEN))')), "(0.0 0.0 0.0)")
    check("M6 handle прочитан",
          call(interp, '(KG-CdrCI "handle" (KG-InstanceModel OBJ_BROKEN EN_BROKEN))'),
          "7B33")

    print("Модель вхождения, у которого читаются все свойства:")
    print("   " + lisp_to_string(call(interp, "(KG-InstanceModel OBJ_FULL EN_FULL)")))
    check("M7 эффективное имя",
          call(interp, '(KG-CdrCI "eff" (KG-InstanceModel OBJ_FULL EN_FULL))'),
          "Комплект КП50 v1.4 стойка КП45303-2")
    check("M8 пространство из карты",
          call(interp, '(KG-CdrCI "space" (KG-InstanceModel OBJ_FULL EN_FULL))'), "Model")
    check("M9 слой",
          call(interp, '(KG-CdrCI "layer" (KG-InstanceModel OBJ_FULL EN_FULL))'),
          "КОНСТРУКЦИИ")
    check("M10 масштаб",
          call(interp, '(KG-CdrCI "sx" (KG-InstanceModel OBJ_FULL EN_FULL))'), 2.0)
    check("M11 точка вставки из variant, а не из списка",
          lisp_to_string(call(interp,
              '(KG-CdrCI "pos" (KG-InstanceModel OBJ_FULL EN_FULL))')),
          "(1250.0 -300.0 15.0)")
    check("M15 KG-PointValue из variant",
          lisp_to_string(call(interp, "(KG-PointValue VAR3 nil)")), "(1.5 -2.5 3.5)")
    check("M16 KG-PointValue из nil",
          lisp_to_string(call(interp, "(KG-PointValue nil nil)")), "(0.0 0.0 0.0)")
    check("M17 KG-PointValue из короткого списка берёт запасной",
          lisp_to_string(call(interp, "(KG-PointValue (list 1.0) (list 7.0 8.0 9.0))")),
          "(7.0 8.0 9.0)")
    check("M18 KG-ListValue из variant",
          lisp_to_string(call(interp, "(KG-ListValue VAR3)")), "(1.5 -2.5 3.5)")

    check("M12 KG-AsNum из nil", call(interp, "(KG-AsNum nil 1.0)"), 1.0)
    check("M13 KG-AsNum из строки", call(interp, '(KG-AsNum "abc" 2.0)'), 2.0)
    check("M14 KG-AsNum из числа", call(interp, "(KG-AsNum 3 0.0)"), 3)

    # --- M19-M22: обработчик ошибок возвращает имя мастер-версии ----------
    # На реальном чертеже INTEGRATE упал после того, как имя мастера было
    # освобождено, и в списке блоков осталось "Комплект КП50 v1.5~до1.5".
    interp.define("KG_EXDefHandle", lambda n: "REC" + str(n))
    call(interp, "(setq KG-SAVED-VARS nil)")
    call(interp, "(setq *XDEFS* (list \"Мастер v2\" \"Стойка\"))")
    call(interp, """
(defun KG_EXDefExists (n) (if (member n *XDEFS*) t nil))
(defun KG_EXRenameDef (o n)
  (if (member n *XDEFS*)
    nil
    (progn (setq *XDEFS* (vl-remove o *XDEFS*))
           (setq *XDEFS* (cons n *XDEFS*))
           t))
)
""")

    # имя свободно -> возвращаем
    call(interp, '(setq KG-MASTER-FREED (cons "Мастер v2" "Мастер v2~до2"))')
    call(interp, '(setq *XDEFS* (list "Мастер v2~до2" "Стойка"))')
    call(interp, '(KG-ErrorRestore "no function definition: SORT")')
    check("M19 после падения имя мастер-версии возвращено",
          lisp_to_string(call(interp, "(KG_EXDefExists \"Мастер v2\")")), "T")
    check("M20 временного имени больше нет",
          lisp_to_string(call(interp, "(KG_EXDefExists \"Мастер v2~до2\")")), "nil")
    check("M21 флаг освобождения сброшен",
          lisp_to_string(call(interp, "KG-MASTER-FREED")), "nil")

    # имя занято пришедшим из буфера определением -> не трогаем
    call(interp, '(setq KG-MASTER-FREED (cons "Мастер v2" "Мастер v2~до2"))')
    call(interp, '(setq *XDEFS* (list "Мастер v2" "Мастер v2~до2"))')
    call(interp, '(KG-ErrorRestore "no function definition: SORT")')
    check("M22 занятое имя не перезаписывается, временное остаётся",
          lisp_to_string(call(interp, "(KG_EXDefExists \"Мастер v2~до2\")")), "T")

    # --- M23-M24: логические флаги из COM ----------------------------------
    # AutoCAD возвращает :vlax-true / :vlax-false, и ОБА символа в условии
    # истинны. Пока флаг читался как (if (vla-get-IsXRef blk) t nil), каждое
    # определение чертежа считалось внешней ссылкой, KG-UserDefNames
    # возвращал пустой список, и INTEGRATE не освобождал перед вставкой ни
    # одного имени -- 40 вложенных определений остались старыми.
    call(interp, "(setq *M23* (KG_DBGetModel))")
    check("M23 обычное определение не помечено внешней ссылкой",
          lisp_to_string(call(
              interp,
              '(KG-FlagCI "is-xref" (KG-FindDef *M23* "Комплект КП50 v1.5"))')),
          "nil")
    check("M24 KG-UserDefNames находит определения чертежа",
          lisp_to_string(call(interp, "(KG-UserDefNames *M23*)")),
          "(Комплект КП50 v1.5)")

    run_deepcopy_checks()

    print()
    print("Пройдено %d, провалено %d" % (PASSED[0], FAILED[0]))
    if FAILNAMES:
        for n in FAILNAMES:
            print("   -", n)
        return 1
    print("ALL PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
