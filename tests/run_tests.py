#!/usr/bin/env python3
"""
run_tests.py -- запуск тестов RepDblock.lsp.

Загружает РЕАЛЬНЫЙ файл RepDblock.lsp интерпретатором AutoLISP-подмножества
(tests/autolisp.py), подставляет тестовые адаптеры БД и исполняет тесты.

Запуск:  python3 tests/run_tests.py
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from autolisp import Interpreter, call, lisp_to_string, LispError  # noqa: E402
from autolisp import _car, _cdr  # noqa: E402


def main():
    interp = Interpreter()

    # Строгие car/cdr для tests.lsp: стенд должен падать там же, где
    # AutoCAD, иначе «зелёный прогон» ничего не значит.
    interp.define("_car", _car)
    interp.define("_cdr", _cdr)

    def _strict_fail(msg="неверный тип аргумента: consp"):
        raise LispError(msg)

    # Строгие car/cdr в tests.lsp должны ПОДНИМАТЬ ошибку так же, как
    # AutoCAD. Средствами самого LISP это не делается: vl-catch-all-apply
    # внутри car ловил собственное исключение, и проверка молча не
    # работала.
    interp.define("_strict-fail", _strict_fail)

    # Флаг среды: исполняющий слой AutoCAD не загружается, адаптеры
    # подставляются тестами.
    interp.load_string("(setq KG-TESTING T)")

    lisp_path = os.path.join(ROOT, "RepDblock.lsp")
    tests_path = os.path.join(HERE, "tests.lsp")

    try:
        interp.load_file(lisp_path)
    except LispError as exc:
        print("ОШИБКА ЗАГРУЗКИ RepDblock.lsp:", exc)
        return 2
    except RecursionError:
        print("ОШИБКА: переполнение стека при загрузке RepDblock.lsp")
        return 2

    try:
        interp.load_file(tests_path)
    except LispError as exc:
        print("ОШИБКА ЗАГРУЗКИ tests.lsp:", exc)
        return 2

    try:
        result = call(interp, "(RUN-ALL-TESTS)")
    except LispError as exc:
        print("\nОШИБКА ВЫПОЛНЕНИЯ ТЕСТОВ:", exc)
        return 2

    print(interp.get_output())

    passed = call(interp, "TESTS-PASSED")
    failed = call(interp, "TESTS-FAILED")
    print()
    print("Итог интерпретатора: %s" % lisp_to_string(result))
    print("Пройдено %d, провалено %d" % (passed, failed))
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
