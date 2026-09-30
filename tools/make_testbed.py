#!/usr/bin/env python3
"""
make_testbed.py -- Этап 0 ТЗ: тестовый стенд для проверки Integration.lsp.

Создаёт два DXF-файла:

  testbed/old.dxf  -- «старый» чертёж: несколько итераций семейства
                          ABC1.01, вложенные определения, экземпляры в модели
                          и на двух листах, посторонние семейства.
  testbed/new_master.dxf -- «новая» версия: мастер ABC1.01(N) с вложенными
                          определениями нового состава. Из этого файла
                          копируется блок в буфер, затем вставляется в old.

Сценарий проверки в AutoCAD:
  1. открыть testbed/old.dxf;
  2. открыть testbed/new_master.dxf, выделить блок ABC1.01(N), Ctrl+C;
  3. в old выполнить CTRL+V (вставка в начало координат);
  4. INTEGRATE.

ВАЖНО. ezdxf умеет писать только DXF и не умеет создавать динамические
блоки (параметры видимости описываются в расширении ACAD_ENHANCEDBLOCK,
публичного формата записи нет). Поэтому стенд состоит из статических
блоков. Для проверки переноса состояний видимости блоки нужно один раз
доработать вручную в BEDIT -- см. подсказки, которые печатает команда
INTTESTBED и раздел README «Что стенд не покрывает».
"""

import os
import sys

import ezdxf
from ezdxf.enums import TextEntityAlignment

# Номер новой итерации. Всё остальное в стенде считается от него,
# поэтому сценарий легко пересобрать под другой номер.
NEW_ITER = 5
FAMILY = "ABC1.01"

HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(os.path.dirname(HERE), "testbed")

# Старые итерации: имя варианта -> номер итерации, в которой вариант появился.
OLD_VARIANTS = [("Стойка", 1), ("Ригель", 2), ("Крышка", 3)]
MASTER_ITER = 4          # текущая мастер-версия в старом файле

# Вложенные определения семейства.
NESTED = ["Стойка", "Ригель", "Крышка", "Закладная"]

# Посторонние определения, которые интеграция трогать не должна.
FOREIGN = ["СБОРКА_1", "ABC1.02(4)Стойка", "XYZ1.01(7)Стойка", "ЧУЖОЙ"]


def make_doc():
    """Пустой документ с двумя листами."""
    doc = ezdxf.new("R2013", setup=False)
    doc.units = ezdxf.units.MM
    # листы с предсказуемыми именами; лист по умолчанию удаляем последним,
    # потому что ezdxf не позволяет удалить единственный лист
    for name in ("Лист 1", "Лист 2"):
        if name not in doc.layouts:
            doc.layouts.new(name)
    if "Layout1" in doc.layouts:
        doc.layouts.delete("Layout1")
    return doc


def nested_block(doc, name, marker):
    """Вложенное определение: окружность + подпись, чтобы отличие
    версий было видно глазами."""
    if name in doc.blocks:
        return
    blk = doc.blocks.new(name=name)
    blk.add_circle((0, 0), radius=100)
    blk.add_text(
        f"{name} {marker}",
        height=40,
    ).set_placement((130, -20), align=TextEntityAlignment.LEFT)


def variant_block(doc, name, nested_names, marker):
    """Определение варианта/мастера: набор вложенных вхождений + подпись."""
    if name in doc.blocks:
        return
    blk = doc.blocks.new(name=name)
    x = 0
    for nm in nested_names:
        blk.add_blockref(nm, (x, 0))
        x += 300
    blk.add_text(name, height=60).set_placement(
        (0, 200), align=TextEntityAlignment.LEFT
    )
    blk.add_text(marker, height=30).set_placement(
        (0, -200), align=TextEntityAlignment.LEFT
    )


def place(space, name, x, y):
    space.add_blockref(name, (x, y))


def build_old(path):
    """Старый чертёж: итерации 1..4, экземпляры в модели и на листах."""
    doc = make_doc()

    # вложенные определения «старого» содержимого
    for nm in NESTED:
        nested_block(doc, nm, f"(до {NEW_ITER})")

    # старые варианты и текущая мастер-версия
    for variant, it in OLD_VARIANTS:
        variant_block(doc, f"{FAMILY}({it}){variant}", [variant], "старая")
    variant_block(
        doc, f"{FAMILY}({MASTER_ITER})", NESTED[:3], "старая мастер-версия"
    )

    # посторонние определения
    for nm in FOREIGN:
        if nm not in doc.blocks:
            blk = doc.blocks.new(name=nm)
            blk.add_circle((0, 0), radius=60)
            blk.add_text(nm, height=40).set_placement(
                (80, -15), align=TextEntityAlignment.LEFT
            )

    msp = doc.modelspace()
    l1 = doc.layouts.get("Лист 1")
    l2 = doc.layouts.get("Лист 2")

    # модель: мастер (2), Стойка (5), Ригель (4), Крышка (1)
    place(msp, f"{FAMILY}({MASTER_ITER})", 0, 0)
    place(msp, f"{FAMILY}({MASTER_ITER})", 0, 1200)
    for i in range(5):
        place(msp, f"{FAMILY}(1)Стойка", 3000 + i * 600, 0)
    for i in range(4):
        place(msp, f"{FAMILY}(2)Ригель", 3000 + i * 600, 1200)
    place(msp, f"{FAMILY}(3)Крышка", 3000, 2400)

    # лист 1: Стойка (4), Ригель (2)
    for i in range(4):
        place(l1, f"{FAMILY}(1)Стойка", 1000 + i * 600, 1000)
    for i in range(2):
        place(l1, f"{FAMILY}(2)Ригель", 1000 + i * 600, 2200)

    # лист 2: Стойка (2), Ригель (2)
    for i in range(2):
        place(l2, f"{FAMILY}(1)Стойка", 1000 + i * 600, 1000)
    for i in range(2):
        place(l2, f"{FAMILY}(2)Ригель", 1000 + i * 600, 2200)

    # посторонние экземпляры
    place(msp, "СБОРКА_1", 20000, 0)
    place(msp, "ABC1.02(4)Стойка", 21000, 0)
    place(msp, "XYZ1.01(7)Стойка", 22000, 0)
    place(l1, "ЧУЖОЙ", 20000, 1000)

    doc.saveas(path)
    return doc


def build_new_master(path):
    """Файл-источник: только новая мастер-версия с новыми вложенными."""
    doc = make_doc()

    # состав вложенных новой версии: три прежних (новое содержимое),
    # «Закладной» больше нет (её определение в старом файле должно
    # остаться нетронутым), плюс совершенно новый блок «Адаптер».
    for nm in ("Стойка", "Ригель", "Крышка"):
        nested_block(doc, nm, f"(версия {NEW_ITER})")
    nested_block(doc, "Адаптер", f"(новый, версия {NEW_ITER})")

    variant_block(
        doc,
        f"{FAMILY}({NEW_ITER})",
        ["Стойка", "Ригель", "Крышка", "Адаптер"],
        "новая мастер-версия",
    )

    msp = doc.modelspace()
    place(msp, f"{FAMILY}({NEW_ITER})", 0, 0)

    doc.saveas(path)
    return doc


def describe(path):
    """Проверка сохранённого файла: читаем обратно и считаем."""
    doc = ezdxf.readfile(path)
    user_blocks = [
        b.name for b in doc.blocks
        if not b.name.startswith("*") and not b.is_any_layout
    ]
    spaces = ["Model"] + [n for n in doc.layout_names() if n != "Model"]
    counts = {}
    for sp in spaces:
        layout = doc.layouts.get(sp)
        counts[sp] = {}
        for ins in layout.query("INSERT"):
            counts[sp][ins.dxf.name] = counts[sp].get(ins.dxf.name, 0) + 1
    return user_blocks, spaces, counts


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    old_path = os.path.join(OUT_DIR, "old.dxf")
    new_path = os.path.join(OUT_DIR, "new_master.dxf")

    build_old(old_path)
    build_new_master(new_path)

    for path in (old_path, new_path):
        blocks, spaces, counts = describe(path)
        print(f"\n{path}")
        print("  определений пользователя:", len(blocks))
        total = 0
        for sp in spaces:
            n = sum(counts[sp].values())
            total += n
            print(f"  {sp}: вхождений {n}")
            for nm in sorted(counts[sp]):
                print(f"      {nm} x{counts[sp][nm]}")
        print("  всего вхождений:", total)

    print("\nСценарий проверки в AutoCAD:")
    print(f"  1. открыть {old_path}")
    print(f"  2. открыть {new_path}, выделить {FAMILY}({NEW_ITER}), Ctrl+C")
    print("  3. в старом файле вставить в начало координат (0,0)")
    print("  4. INTEGRATE")
    return 0


if __name__ == "__main__":
    sys.exit(main())
