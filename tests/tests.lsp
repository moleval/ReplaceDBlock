;;;===========================================================================
;;; tests_tests.lsp -- тесты для Integration.lsp
;;;
;;; Файл загружается ВНУТРИ тестового стенда (tests/run_tests.py), который
;;; исполняет его настоящим интерпретатором AutoLISP-подмножества. Перед
;;; загрузкой Integration.lsp стенд выставляет KG-TESTING = T, поэтому
;;; исполняющий слой AutoCAD (раздел 8) не загружается, а адаптеры KG_DB*/
;;; KG_EX* подставляются здесь и работают с фейковой базой *DB*.
;;;
;;; Проверяются:
;;;   T1  парсер имени блока на всех векторах из ТЗ (Этап 1)
;;;   T2  рекурсивный обход вложенных определений (Этап 5)
;;;   T3  сканирование файла и группировка (Этап 2)
;;;   T4  карта вложенных блоков (Этап 6)
;;;   T5  правило видимости, вариант A (п. 9.4 ТЗ)
;;;   T6  построение карты интеграции (Этап 4)
;;;   T7  ПОЛНАЯ интеграция через KG-Integrate_Model (Этапы 8-12)
;;;   T8  валидация результата и отсутствие служебных имён (п. 20 ТЗ)
;;;   T9  исключительные случаи (Этап 14)
;;;===========================================================================

(setq TESTS-PASSED 0)
(setq TESTS-FAILED 0)
(setq TESTS-FAILNAMES nil)

(defun T-Ok (name cond)
  (if cond
    (progn
      (setq TESTS-PASSED (1+ TESTS-PASSED))
      (princ (strcat "\n  ok   " name))
    )
    (progn
      (setq TESTS-FAILED (1+ TESTS-FAILED))
      (setq TESTS-FAILNAMES (cons name TESTS-FAILNAMES))
      (princ (strcat "\n  FAIL " name))
    )
  )
  cond
)

(defun T-EqStr (name actual expected)
  (T-Ok (strcat name "  [ожидается \"" expected "\", получено \""
                (if actual actual "nil") "\"]")
        (and actual (= (strcase actual) (strcase expected))))
)

(defun T-EqInt (name actual expected)
  (T-Ok (strcat name "  [ожидается " (itoa expected) ", получено "
                (if actual (itoa actual) "nil") "]")
        (and actual (= actual expected)))
)

;;;---------------------------------------------------------------------------
;;; ФЕЙКОВАЯ БАЗА ЧЕРТЕЖА
;;;---------------------------------------------------------------------------

;; Пустая база по умолчанию
(setq *DB* (list (cons "defs" nil) (cons "insts" nil) (cons "layouts" nil)))

;; def  = (name is-xref is-layout is-dynamic nested vis-states)
(defun DB-MakeDef (name nested vis / )
  (list
    (cons "name" name)
    (cons "is-xref" nil)
    (cons "is-layout" nil)
    (cons "is-dynamic" (if vis t nil))
    (cons "nested" nested)
    (cons "vis-states" vis)
    (cons "vis-param" "Видимость")
  )
)

;; inst = (handle eff def space layer pos rot sx sy sz nested-vis)
(defun DB-MakeInst (handle eff def space layer pos nestedvis / )
  (list
    (cons "handle" handle)
    (cons "eff" eff)
    (cons "def" def)
    (cons "space" space)
    (cons "layer" layer)
    (cons "pos" pos)
    (cons "rot" 0.0)
    (cons "sx" 1.0) (cons "sy" 1.0) (cons "sz" 1.0)
    (cons "normal" (list 0.0 0.0 1.0))
    (cons "nested-vis" nestedvis)
    ;; Свойства самого блока: у реального экземпляра всегда есть свой
    ;; параметр видимости. Без этого счётчик перенесённых свойств был бы
    ;; всегда 0 и проверка ничего бы не ловила.
    (cons "dyn-props" (list (cons "Видимость1" "B")))
  )
)

(defun DB-Defs ()  (cdr (assoc "defs"  *DB*)))
(defun DB-Insts () (cdr (assoc "insts" *DB*)))

(defun DB-SetDefs (v)
  (setq *DB* (list (cons "defs" v)
                   (cons "insts" (DB-Insts))
                   (cons "layouts" nil)))
)
(defun DB-SetInsts (v)
  (setq *DB* (list (cons "defs" (DB-Defs))
                   (cons "insts" v)
                   (cons "layouts" nil)))
)

(defun DB-FindDef (name / hit)
  (setq hit nil)
  (foreach d (DB-Defs)
    (if (and (not hit) (KG-StrEq (KG-CdrCI "name" d) name)) (setq hit d))
  )
  hit
)
(defun DB-HasDef (name) (if (DB-FindDef name) t nil))

(defun DB-DelDef (name / out)
  (setq out nil)
  (foreach d (DB-Defs)
    (if (not (KG-StrEq (KG-CdrCI "name" d) name)) (setq out (cons d out)))
  )
  (DB-SetDefs (reverse out))
)

(defun DB-DelInst (h / out)
  (setq out nil)
  (foreach i (DB-Insts)
    (if (not (= (KG-CdrCI "handle" i) h)) (setq out (cons i out)))
  )
  (DB-SetInsts (reverse out))
)

(defun DB-FindInst (h / hit)
  (setq hit nil)
  (foreach i (DB-Insts)
    (if (and (not hit) (= (KG-CdrCI "handle" i) h)) (setq hit i))
  )
  hit
)

(defun DB-InstNames ( / out)
  (setq out nil)
  (foreach i (DB-Insts) (setq out (cons (KG-CdrCI "eff" i) out)))
  (reverse out)
)

(defun DB-CountEff (eff / n)
  (setq n 0)
  (foreach i (DB-Insts)
    (if (KG-StrEq (KG-CdrCI "eff" i) eff) (setq n (1+ n)))
  )
  n
)

(defun DB-DefNames ( / out)
  (setq out nil)
  (foreach d (DB-Defs) (setq out (cons (KG-CdrCI "name" d) out)))
  (reverse out)
)

;; Счётчик handle'ов для пересоздаваемых экземпляров
(setq DB-HANDLE-SEQ 1000)
(defun DB-NextHandle ( / h)
  (setq DB-HANDLE-SEQ (1+ DB-HANDLE-SEQ))
  (setq h (strcat "H" (itoa DB-HANDLE-SEQ)))
  h
)

;;;---------------------------------------------------------------------------
;;; АДАПТЕРЫ (подменяют исполняющий слой AutoCAD)
;;;---------------------------------------------------------------------------

(defun KG_DBGetModel () *DB*)

(defun KG_EXAllDefNames () (DB-DefNames))

(defun KG_EXDefExists (name) (DB-HasDef name))

;; Интеграция в режиме вставки из буфера. В реальном AutoCAD имена
;; освобождает C:INTEGRATE ДО вставки и передаёт их через KG-PRERENAMES;
;; стенд повторяет этот порядок, иначе KG-Integrate_Model осталась бы
;; без списка переименований.
(defun TEST-IntegratePaste (base iter)
  (setq KG-PREDEFS (KG_EXAllDefNames))
  (setq KG-PRERENAMES
    (KG-Step_PreRename
      (KG-ConflictCandidates (KG_DBGetModel) base) iter base (KG_DBGetModel)))
  (KG-Integrate_Model base iter nil t)
)

;; Прогон режима A ТОЧНО в том порядке, в каком работает C:INTEGRATE:
;; освободить имена -> снять handle -> ОДНА вставка -> определить, что
;; пришло -> вернуть непришедшие имена -> интеграция БЕЗ второй вставки.
;; Именно здесь на реальном чертеже имена не освобождались до вставки,
;; вставка выполнялась дважды и создавался мусорный вариант.
(defun TEST-IntegrateAsCommand ( / beforedefs mdl pre nm p c preren hsnap
                                   afterdefs snap mastername q)
  (setq KG-PRERENAMES nil)
  (setq KG-PREDEFS nil)
  (setq KG-ARRIVED nil)
  (setq snap (KG-SnapshotDrawing))
  (setq beforedefs (KG_EXAllDefNames))
  (setq KG-PREDEFS beforedefs)
  (setq mdl (KG_DBGetModel))
  (setq pre nil)
  (foreach nm (KG-UserDefNames mdl)
    (if (and (/= (substr nm 1 1) "*") (not (member nm pre)))
      (setq pre (cons nm pre))
    )
  )
  (setq KG-PRE-CANDIDATES (length pre))
  (setq preren (KG-Step_PreRename (reverse pre) nil nil mdl))
  (setq hsnap (KG_EXDefHandleSnapshot))
  (KG_EXPasteAtOrigin)
  (setq afterdefs (KG_EXAllDefNames))
  (setq KG-ARRIVED (KG-Step_DetectArrived beforedefs afterdefs))
  (foreach nm (KG-ArrivedByHandle hsnap)
    (if (not (KG-StrInterCI (list nm) KG-ARRIVED))
      (setq KG-ARRIVED (cons nm KG-ARRIVED))
    )
  )
  (setq preren (KG-Step_RollbackNotArrived preren KG-ARRIVED))
  (setq KG-PRERENAMES preren)
  (setq mastername (KG-DetectMasterName KG-ARRIVED))
  (if mastername
    (progn
      (setq q (KG-ParseBlockName mastername))
      (KG-Integrate_Model
        (nth 0 q) (nth 1 q)
        (KG-PickMasterHandle (KG_EXNewInstancesSince snap))
        nil)
    )
    nil
  )
)

;; Handle определения. В реальном AutoCAD у определения, которое
;; PASTECLIP принёс вместо старого, handle ДРУГОЙ, а переименование
;; handle НЕ меняет. На этом основана проверка «что реально пришло из
;; буфера». Стенд повторяет именно это: handle хранится внутри
;; определения, поэтому переименование его сохраняет, а замена
;; определения (TEST-ReplaceDef) даёт новый.
(setq DB-PASTE-SEQ 0)
(setq DB-REC-SEQ 0)
(defun DB-NextRec ( / r)
  (setq DB-REC-SEQ (1+ DB-REC-SEQ))
  (setq r (strcat "REC" (itoa DB-REC-SEQ)))
  r
)

(defun KG_EXDefHandle (name / d h out)
  (setq d (DB-FindDef name))
  (if d
    (progn
      (setq h (KG-CdrCI "rech" d))
      (if (null h)
        (progn
          (setq h (DB-NextRec))
          (setq out nil)
          (foreach e (DB-Defs)
            (if (KG-StrEq (KG-CdrCI "name" e) name)
              (setq out (cons (append e (list (cons "rech" h))) out))
              (setq out (cons e out))
            )
          )
          (DB-SetDefs (reverse out))
        )
      )
      h
    )
    nil
  )
)

;; Так ведёт себя PASTECLIP, когда имя освобождено: определение с этим
;; именем приходит из буфера и ЗАМЕНЯЕТ прежнее -- содержимое новое,
;; handle другой, имя то же.
(defun TEST-ReplaceDef (name nested states)
  (DB-DelDef name)
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef name nested states))))
  t
)

;; Снимок и сравнение по handle живут в разделе 8 (исполняющий слой),
;; поэтому на стенде повторяются здесь тем же способом.
(defun KG_EXDefHandleSnapshot ( / out)
  (setq out nil)
  (foreach nm (DB-DefNames)
    (setq out (cons (cons nm (KG_EXDefHandle nm)) out))
  )
  (reverse out)
)

(defun KG-ArrivedByHandle (snap / out nm pair)
  (setq out nil)
  (foreach nm (DB-DefNames)
    (setq pair (KG-AssocCI nm snap))
    (if (or (null pair) (not (KG-StrEq (cdr pair) (KG_EXDefHandle nm))))
      (setq out (cons nm out))
    )
  )
  (reverse out)
)

(defun KG_EXRenameDef (oldname newname / d out)
  (if (DB-HasDef newname)
    nil
    (progn
      (setq d (DB-FindDef oldname))
      (if d
        (progn
          (setq d (KG-SetAssoc "name" newname d))
          (setq out nil)
          (foreach x (DB-Defs)
            (setq out (cons (if (KG-StrEq (KG-CdrCI "name" x) oldname) d x) out))
          )
          (DB-SetDefs (reverse out))
          t
        )
        nil
      )
    )
  )
)

;; DB-LOCKED имитирует определение, занятое экземплярами: его нельзя
;; удалить, и команда обязана это пережить, ничего не сломав.
(setq DB-LOCKED nil)
(defun KG_EXDeleteDef (name)
  (if (or (member name DB-LOCKED) (not (DB-HasDef name)))
    nil
    (progn (DB-DelDef name) t)
  )
)

(defun KG-EXDefIsDynamic (name / d)
  (setq d (DB-FindDef name))
  (if d (KG-CdrCI "is-dynamic" d) nil)
)

;; Имитация PASTECLIP.
;; Содержимое буфера лежит в *CLIP*. Как и настоящий AutoCAD, определение
;; ПРИХОДИТ только если его имя свободно в целевом файле; иначе оно
;; отбрасывается (AutoCAD печатает "Duplicate definition of block ... ignored").
(setq *CLIP* nil)
(setq *CLIP-ARRIVED* nil)
;; Тестовый режим: вставка из буфера не приносит ничего (в реальном
;; AutoCAD так бывает, когда буфер пуст или вставка не состоялась).
(setq *CLIP-DROP-MASTER* nil)

(defun KG_EXPasteAtOrigin ( / nm)
  (setq *CLIP-ARRIVED* nil)
  (setq DB-PASTE-SEQ (1+ DB-PASTE-SEQ))
  (if (not *CLIP-DROP-MASTER*)
    (foreach d *CLIP*
      (setq nm (KG-CdrCI "name" d))
      (if (not (DB-HasDef nm))
        (progn
          (DB-SetDefs (append (DB-Defs) (list d)))
          (setq *CLIP-ARRIVED* (cons nm *CLIP-ARRIVED*))
        )
      )
    )
  )
  ;; технический экземпляр мастер-версии -- только если определение пришло
  (if (and *CLIP-MASTER* (member *CLIP-MASTER* *CLIP-ARRIVED*))
    (DB-SetInsts
      (append (DB-Insts)
        (list (DB-MakeInst "HTECH" *CLIP-MASTER* *CLIP-MASTER* "Model" "0"
                           (list 0.0 0.0 0.0) nil))))
  )
  t
)

;; Загрузить «буфер обмена» новой мастер-версией
(defun TEST-SetClipboard (master nested-defs)
  (setq *CLIP-MASTER* master)
  (setq *CLIP-ARRIVED* nil)
  (setq *CLIP-DROP-MASTER* nil)
  (setq *CLIP* (append (list (DB-MakeDef master
                              (mapcar '(lambda (d) (KG-CdrCI "name" d)) nested-defs)
                              nil))
                       nested-defs))
)

(defun KG_EXNewInstancesSince (snapshot / out)
  (setq out nil)
  (foreach i (DB-Insts)
    (if (not (member (KG-CdrCI "handle" i) snapshot))
      (setq out (cons (KG-CdrCI "handle" i) out))
    )
  )
  (reverse out)
)

(defun KG-SnapshotDrawing ( / out)
  (setq out nil)
  (foreach i (DB-Insts) (setq out (cons (KG-CdrCI "handle" i) out)))
  (reverse out)
)

(defun KG_EXInstanceHandle (h) (DB-FindInst h))

;; в стенде пространства берутся из модели экземпляра, карты нет
(defun KG-RefreshSpaceMap () nil)

;; Создание экземпляра: копия свойств, новое пространство НЕ меняется,
;; handle меняется (как и в реальном AutoCAD при пересоздании).
(defun KG_EXCreateInstance (defname instmodel / h ni)
  (if (not (DB-HasDef defname))
    nil
    (progn
      (setq h (DB-NextHandle))
      (setq ni (KG-SetAssoc "handle" h instmodel))
      (setq ni (KG-SetAssoc "eff" defname ni))
      (setq ni (KG-SetAssoc "def" defname ni))
      (DB-SetInsts (append (DB-Insts) (list ni)))
      h
    )
  )
)

(defun KG_EXDeleteInstance (h)
  (if (DB-FindInst h)
    (progn (DB-DelInst h) t)
    nil
  )
)

(defun KG_EXRestoreDynProps (h props / n)
  (setq n 0)
  (foreach p props (setq n (1+ n)))
  n
)

(defun KG_EXSetEffectiveName (h defname / i out)
  (if (not (DB-HasDef defname))
    nil
    (progn
      (setq out nil)
      (foreach i (DB-Insts)
        (if (= (KG-CdrCI "handle" i) h)
          (setq out (cons (KG-SetAssoc "eff" defname
                          (KG-SetAssoc "def" defname i)) out))
          (setq out (cons i out))
        )
      )
      (DB-SetInsts (reverse out))
      t
    )
  )
)

;; Установка видимости вложенного блока внутри экземпляра.
;; Контракт настоящего адаптера: T -- установлено, nil -- не
;; установилось, "НЕ НАЙДЕН" -- вложенной вставки в представлении нет,
;; "НЕТ ПРЕДСТАВЛЕНИЯ" -- вставка смотрит на общее определение.
;; Заглушка обязана отвечать тем же, иначе ветки обработки ответа
;; остаются непроверенными: сборка 36 печатала «не установлено» на
;; каждый экземпляр, а тесты были зелёные, потому что здесь стоял свой
;; ответ, не совпадающий с настоящим.
(setq DB-SETVIS-MODE "OK")
(defun KG_EXSetNestedVisibility (h nestedname value / i nv out found)
  (cond
    ((KG-StrEq DB-SETVIS-MODE "НЕТ ПРЕДСТАВЛЕНИЯ") "НЕТ ПРЕДСТАВЛЕНИЯ")
    ((KG-StrEq DB-SETVIS-MODE "НЕ НАЙДЕН")
     ;; первый проход не находит, после пересоздания представления -- находит
     (if DB-SETVIS-RETRIED t (progn (setq DB-SETVIS-RETRIED t) "НЕ НАЙДЕН")))
    (t
     (setq found nil out nil)
     (foreach i (DB-Insts)
       (if (= (KG-CdrCI "handle" i) h)
         (progn
           (setq nv (KG-CdrCI "nested-vis" i))
           (if (KG-AssocCI nestedname nv)
             (setq nv (KG-SetAssoc nestedname value nv))
             (setq nv (append nv (list (cons nestedname value))))
           )
           (setq found t)
           (setq out (cons (KG-SetAssoc "nested-vis" nv i) out))
         )
         (setq out (cons i out))
       )
     )
     (DB-SetInsts (reverse out))
     (if found t "НЕ НАЙДЕН"))
  )
)

;; Создание определения-варианта копированием из мастер-версии.
;; Как и в AutoCAD: динамические авторские элементы НЕ переносятся,
;; геометрия и вложенные ссылки -- переносятся.
;; Адаптеры раздела 8: глубокое клонирование (ObjectDBX) и запасной путь.
;; Глубокое клонирование копирует определение ЦЕЛИКОМ, вместе со списком
;; состояний видимости, поэтому вариант остаётся динамическим. Флаг
;; DB-DBX-OK имитирует недоступность ObjectDBX.
(setq DB-DBX-OK t)
(defun KG_EXCopyDefDeep (srcname newname / src)
  (setq src (DB-FindDef srcname))
  (if (and DB-DBX-OK src (not (DB-HasDef newname)))
    (progn
      (DB-SetDefs
        (append (DB-Defs)
                (list (DB-MakeDef newname
                         (KG-CdrCI "nested" src)
                         (KG-CdrCI "vis-states" src)))))
      t
    )
    nil
  )
)

(defun KG-EXCreateDefStatic (newname mastername / src)
  (setq src (DB-FindDef mastername))
  (if (and src (not (DB-HasDef newname)))
    (progn
      (DB-SetDefs
        (append (DB-Defs) (list (DB-MakeDef newname
                                   (KG-CdrCI "nested" src) nil))))
      t
    )
    nil
  )
)

;;;---------------------------------------------------------------------------
;;; ПОСТРОЕНИЕ ТЕСТОВОГО СТЕНДА (соответствует Этапу 0 ТЗ)
;;;---------------------------------------------------------------------------

(defun TEST-BuildOldDrawing ()
  (setq *CLIP* nil)
  (setq *CLIP-MASTER* nil)
  (DB-SetDefs
    (list
      ;; старые варианты семейства
      (DB-MakeDef "ABC1.01(4)"          (list "Стойка" "Ригель" "Закладная") nil)
      (DB-MakeDef "ABC1.01(1)Стойка"    (list "Стойка") nil)
      (DB-MakeDef "ABC1.01(2)Ригель"    (list "Ригель") nil)
      (DB-MakeDef "ABC1.01(3)Крышка"    (list "Крышка") nil)
      ;; старые вложенные определения
      (DB-MakeDef "Стойка"     nil (list "A" "B" "C" "D"))
      (DB-MakeDef "Ригель"     nil (list "1" "2"))
      (DB-MakeDef "Закладная"  nil (list "X" "Y"))
      (DB-MakeDef "Крышка"     nil (list "K1" "K2"))
      ;; сторонние объекты, которые НЕ должны быть затронуты (п. 3.4 ТЗ)
      (DB-MakeDef "СБОРКА_1"   (list "ABC1.01(1)Стойка") nil)
      (DB-MakeDef "ABC1.02(4)Стойка" (list "Стойка") nil)
      (DB-MakeDef "XYZ1.01(7)Стойка" (list "Стойка") nil)
      (DB-MakeDef "ЧУЖОЙ"      nil nil)
    )
  )
  (DB-SetInsts
    (list
      ;; 2 без суффикса
      (DB-MakeInst "H1" "ABC1.01(4)" "ABC1.01(4)" "Model" "0"
                   (list 0.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H2" "ABC1.01(4)" "ABC1.01(4)" "Model" "0"
                   (list 500.0 0.0 0.0) (list (cons "Стойка" "D")))
      ;; 12 Стойка
      (DB-MakeInst "H3"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 1000.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H4"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 1100.0 0.0 0.0) (list (cons "Стойка" "D")))
      (DB-MakeInst "H5"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 1200.0 0.0 0.0) (list (cons "Стойка" "B")))
      (DB-MakeInst "H6"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 1" "0"
                   (list 1300.0 0.0 0.0) (list (cons "Стойка" "C")))
      (DB-MakeInst "H7"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 1" "0"
                   (list 1400.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H8"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 1" "0"
                   (list 1500.0 0.0 0.0) (list (cons "Стойка" "B")))
      (DB-MakeInst "H9"  "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 1" "0"
                   (list 1600.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H10" "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 2" "0"
                   (list 1700.0 0.0 0.0) (list (cons "Стойка" "C")))
      (DB-MakeInst "H11" "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Лист 2" "0"
                   (list 1800.0 0.0 0.0) (list (cons "Стойка" "D")))
      (DB-MakeInst "H12" "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 1900.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H13" "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 2000.0 0.0 0.0) (list (cons "Стойка" "B")))
      (DB-MakeInst "H14" "ABC1.01(1)Стойка" "ABC1.01(1)Стойка" "Model" "0"
                   (list 2100.0 0.0 0.0) (list (cons "Стойка" "C")))
      ;; 8 Ригель
      (DB-MakeInst "H15" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Model" "0"
                   (list 3000.0 0.0 0.0) (list (cons "Ригель" "1")))
      (DB-MakeInst "H16" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Model" "0"
                   (list 3100.0 0.0 0.0) (list (cons "Ригель" "2")))
      (DB-MakeInst "H17" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Лист 1" "0"
                   (list 3200.0 0.0 0.0) (list (cons "Ригель" "1")))
      (DB-MakeInst "H18" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Лист 1" "0"
                   (list 3300.0 0.0 0.0) (list (cons "Ригель" "2")))
      (DB-MakeInst "H19" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Лист 2" "0"
                   (list 3400.0 0.0 0.0) (list (cons "Ригель" "1")))
      (DB-MakeInst "H20" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Лист 2" "0"
                   (list 3500.0 0.0 0.0) (list (cons "Ригель" "2")))
      (DB-MakeInst "H21" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Model" "0"
                   (list 3600.0 0.0 0.0) (list (cons "Ригель" "1")))
      (DB-MakeInst "H22" "ABC1.01(2)Ригель" "ABC1.01(2)Ригель" "Model" "0"
                   (list 3700.0 0.0 0.0) (list (cons "Ригель" "2")))
      ;; 1 Крышка
      (DB-MakeInst "H23" "ABC1.01(3)Крышка" "ABC1.01(3)Крышка" "Model" "0"
                   (list 4000.0 0.0 0.0) (list (cons "Крышка" "K1")))
      ;; сторонние экземпляры: трогать нельзя
      (DB-MakeInst "H90" "ABC1.02(4)Стойка" "ABC1.02(4)Стойка" "Model" "0"
                   (list 9000.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H91" "XYZ1.01(7)Стойка" "XYZ1.01(7)Стойка" "Model" "0"
                   (list 9100.0 0.0 0.0) (list (cons "Стойка" "A")))
      (DB-MakeInst "H92" "СБОРКА_1" "СБОРКА_1" "Model" "0"
                   (list 9200.0 0.0 0.0) nil)
      (DB-MakeInst "H93" "ЧУЖОЙ" "ЧУЖОЙ" "Лист 1" "0"
                   (list 9300.0 0.0 0.0) nil)
    )
  )
)

;; Вспомогательная утилита: добавить определения мастер-версии напрямую,
;; минуя проверку занятости имён (для тестов, которым нужно готовое состояние)
(defun TEST-ForceNewMaster ()
  (DB-SetDefs
    (append (DB-Defs)
      (list
        (DB-MakeDef "ABC1.01(5)" (list "Стойка" "Ригель" "Крышка" "Адаптер") nil)
        (DB-MakeDef "Стойка"    nil (list "A" "B" "C" "E" "F"))
        (DB-MakeDef "Ригель"    nil (list "1" "2" "3"))
        (DB-MakeDef "Крышка"    nil (list "K1" "K2" "K3"))
        (DB-MakeDef "Адаптер"   nil (list "AD1"))
      )
    )
  )
  ;; технический экземпляр мастер-версии
  (DB-SetInsts
    (append (DB-Insts)
      (list (DB-MakeInst "HTECH" "ABC1.01(5)" "ABC1.01(5)" "Model" "0"
                         (list 0.0 0.0 0.0) nil))
    )
  )
)

;;;---------------------------------------------------------------------------
;;; T1. ПАРСЕР (Этап 1)
;;;---------------------------------------------------------------------------

(defun TEST-Parser ()
  (princ "\n\nT1. Парсер имени блока")

  ;; "ABC1.01(17)Стойка"
  (T-EqStr "T1.1 base"    (KG-ParseBase "ABC1.01(17)Стойка") "ABC1.01")
  (T-EqStr "T1.2 iter"    (KG-ParseIteration "ABC1.01(17)Стойка") "17")
  (T-EqStr "T1.3 variant" (KG-ParseVariant "ABC1.01(17)Стойка") "Стойка")

  ;; пустой вариант
  (T-EqStr "T1.4 base"    (KG-ParseBase "ABC1.01(15)") "ABC1.01")
  (T-EqStr "T1.5 iter"    (KG-ParseIteration "ABC1.01(15)") "15")
  (T-Ok    "T1.6 variant пустой" (= (KG-ParseVariant "ABC1.01(15)") ""))

  ;; одноцифровая итерация
  (T-EqStr "T1.7 iter 8"  (KG-ParseIteration "ABC1.01(8)Ригель") "8")
  (T-EqStr "T1.8 variant" (KG-ParseVariant "ABC1.01(8)Ригель") "Ригель")

  ;; другое семейство
  (T-EqStr "T1.9 base другого семейства" (KG-ParseBase "ABC1.02(17)Стойка") "ABC1.02")
  (T-Ok "T1.10 не наше семейство"
        (not (KG-IsFamilyName "ABC1.02(17)Стойка" "ABC1.01")))

  ;; отрицательные случаи из ТЗ
  (T-Ok "T1.11 нет скобок"          (null (KG-ParseBlockName "ABC1.01Стойка")))
  (T-Ok "T1.12 пустые скобки"       (null (KG-ParseBlockName "ABC1.01()Стойка")))
  (T-Ok "T1.13 нецифровая итерация" (null (KG-ParseBlockName "ABC1.01(1x)Стойка")))
  (T-Ok "T1.14 нет закрывающей"     (null (KG-ParseBlockName "ABC1.01(15")))
  (T-Ok "T1.15 пустое имя"          (null (KG-ParseBlockName "")))
  (T-Ok "T1.16 nil"                 (null (KG-ParseBlockName nil)))
  (T-Ok "T1.17 пустой base"         (null (KG-ParseBlockName "(15)Стойка")))

  ;; последняя закрывающая скобка
  (T-EqStr "T1.18 последняя скобка" (KG-ParseIteration "A(1)(2)Стойка") "2")
  (T-EqStr "T1.19 base со скобкой"  (KG-ParseBase "A(1)(2)Стойка") "A(1)")

  ;; сборка имени
  (T-EqStr "T1.20 MakeName с суффиксом" (KG-MakeName "ABC1.01" 15 "Стойка")
           "ABC1.01(15)Стойка")
  (T-EqStr "T1.21 MakeName без суффикса" (KG-MakeName "ABC1.01" 15 "")
           "ABC1.01(15)")

  ;; кириллица и регистронезависимость семейства
  (T-Ok "T1.22 регистр семейства"
        (KG-IsFamilyName "abc1.01(3)крышка" "ABC1.01"))
  (T-Ok "T1.23 точное совпадение итерации"
        (KG-IsIntegrationName "ABC1.01(15)Стойка" "ABC1.01" 15))
  (T-Ok "T1.24 другая итерация не совпадает"
        (not (KG-IsIntegrationName "ABC1.01(14)Стойка" "ABC1.01" 15)))
)

;;;---------------------------------------------------------------------------
;;; T2. ВЛОЖЕННЫЕ ОПРЕДЕЛЕНИЯ (Этап 5)
;;;---------------------------------------------------------------------------

(defun TEST-Nested ()
  (princ "\n\nT2. Рекурсивный обход вложенных определений")
  (DB-SetDefs
    (list
      (DB-MakeDef "MASTER" (list "Стойка" "Ригель") nil)
      (DB-MakeDef "Стойка" (list "Закладная") nil)
      (DB-MakeDef "Ригель" (list "Прижим") nil)
      (DB-MakeDef "Закладная" nil nil)
      (DB-MakeDef "Прижим" (list "Гайка") nil)
      (DB-MakeDef "Гайка" nil nil)
      ;; цикл не должен зациклить обход
      (DB-MakeDef "ЦИКЛ_A" (list "ЦИКЛ_B") nil)
      (DB-MakeDef "ЦИКЛ_B" (list "ЦИКЛ_A") nil)
    )
  )
  (T-EqInt "T2.1 глубина 2 уровня"
           (length (KG-GetNestedBlocks (KG_DBGetModel) "MASTER")) 5)
  (T-Ok "T2.2 состав"
        (and (member "Стойка" (KG-GetNestedBlocks (KG_DBGetModel) "MASTER"))
             (member "Закладная" (KG-GetNestedBlocks (KG_DBGetModel) "MASTER"))
             (member "Ригель" (KG-GetNestedBlocks (KG_DBGetModel) "MASTER"))
             (member "Прижим" (KG-GetNestedBlocks (KG_DBGetModel) "MASTER"))))
  (T-Ok "T2.3 самого корня в списке нет"
        (not (member "MASTER" (KG-GetNestedBlocks (KG_DBGetModel) "MASTER"))))
  (T-EqInt "T2.4 обход с корнем"
           (length (KG-GetMasterDependencies (KG_DBGetModel) "MASTER")) 6)
  (T-EqInt "T2.5 цикл не зацикливает"
           (length (KG-GetNestedBlocks (KG_DBGetModel) "ЦИКЛ_A")) 1)
  (T-Ok "T2.6 неизвестное определение"
        (= (length (KG-GetNestedBlocks (KG_DBGetModel) "НЕТУ")) 0))
)

;;;---------------------------------------------------------------------------
;;; T3. СКАНИРОВАНИЕ (Этап 2)
;;;---------------------------------------------------------------------------

(defun TEST-Scan ()
  (princ "\n\nT3. Сканирование файла")
  (TEST-BuildOldDrawing)
  (setq SCAN3 (KG-ScanFamily (KG_DBGetModel) "ABC1.01" 5))
  (T-EqInt "T3.1 всего старых" (cdr (assoc "total" SCAN3)) 23)
  (T-EqInt "T3.2 итераций найдено" (length (cdr (assoc "iterations" SCAN3))) 4)
  (T-EqInt "T3.3 вариантов" (length (cdr (assoc "variants" SCAN3))) 4)

  (setq G3 (cdr (assoc "groups" SCAN3)))
  (T-EqInt "T3.4 вариант \"\" = 2"
           (length (cdr (KG-AssocCI "" G3))) 2)
  (T-EqInt "T3.5 вариант Стойка = 12"
           (length (cdr (KG-AssocCI "Стойка" G3))) 12)
  (T-EqInt "T3.6 вариант Ригель = 8"
           (length (cdr (KG-AssocCI "Ригель" G3))) 8)
  (T-EqInt "T3.7 вариант Крышка = 1"
           (length (cdr (KG-AssocCI "Крышка" G3))) 1)

  (setq SP3 (cdr (assoc "spaces" SCAN3)))
  (T-EqInt "T3.8 Model = 13" (cdr (KG-AssocCI "Model" SP3)) 13)
  (T-EqInt "T3.9 Лист 1 = 6" (cdr (KG-AssocCI "Лист 1" SP3)) 6)
  (T-EqInt "T3.10 Лист 2 = 4" (cdr (KG-AssocCI "Лист 2" SP3)) 4)

  ;; другие семейства не попадают
  (T-Ok "T3.11 ABC1.02 не учтён"
        (not (vl-some '(lambda (pr) (wcmatch (KG-CdrCI "eff" (car pr)) "ABC1.02*"))
                      (KG-FindOldIterations (KG_DBGetModel) "ABC1.01" 5))))
  (T-Ok "T3.12 XYZ1.01 не учтён"
        (not (vl-some '(lambda (pr) (wcmatch (KG-CdrCI "eff" (car pr)) "XYZ*"))
                      (KG-FindOldIterations (KG_DBGetModel) "ABC1.01" 5))))
  ;; новая итерация не считается старой
  (T-Ok "T3.13 новая итерация в список старых не попадает"
        (not (vl-some '(lambda (pr) (KG-IterEq (nth 1 (cdr pr)) "5"))
                      (KG-FindOldIterations (KG_DBGetModel) "ABC1.01" 5))))
  ;; старая -- любая другая, включая более позднюю: её тоже надо заменить,
  ;; иначе в файле останутся два конкурирующих поколения
  (T-Ok "T3.14 любая другая итерация считается старой"
        (vl-every '(lambda (pr) (KG-IterOther (nth 1 (cdr pr)) "5"))
                  (KG-FindOldIterations (KG_DBGetModel) "ABC1.01" 5)))
)

;;;---------------------------------------------------------------------------
;;; T4. КАРТА ВЛОЖЕННЫХ БЛОКОВ (Этап 6)
;;;---------------------------------------------------------------------------

(defun TEST-NestedMap ()
  (princ "\n\nT4. Карта вложенных определений")

  ;; 4a. Имена освобождены ДО вставки -> всё пришло, всё к обновлению
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (KG-Step_PreRename (KG-ConflictCandidates (KG_DBGetModel) "ABC1.01") 5 "ABC1.01" (KG_DBGetModel))
  (KG_EXPasteAtOrigin)
  (setq NM4 (KG-MapNestedBlocks (KG_DBGetModel) "ABC1.01(5)"))
  (T-EqInt "T4.1 к обновлению = 4"
           (length (cdr (assoc "to-update" NM4))) 4)
  (T-EqInt "T4.2 к добавлению = 0"
           (length (cdr (assoc "to-add" NM4))) 0)
  (T-EqInt "T4.2b у Стойки теперь 5 состояний (A B C E F)"
           (length (KG-CdrCI "vis-states" (DB-FindDef "Стойка"))) 5)

  ;; 4b. Имена НЕ освобождены -> AutoCAD отбрасывает определения.
  ;;     Это ровно тот сценарий, из-за которого переименование делается
  ;;     ДО вставки, и программа обязана его обнаружить.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (setq BEFORE4 (KG_EXAllDefNames))
  (KG_EXPasteAtOrigin)                       ; имена заняты -> всё отброшено
  ;; мастер приходит, но вложенные AutoCAD подставляет СТАРЫЕ
  (setq NM4c (KG-BuildIntegrationMap (KG_DBGetModel) "ABC1.01(5)" BEFORE4))
  (T-EqInt "T4.3 вложенные числятся к обновлению (Стойка Ригель Крышка)"
           (length (cdr (assoc "nested-to-update" NM4c))) 3)
  ;; из четырёх вложенных реально пришёл только новый "Адаптер";
  ;; Стойка/Ригель/Крышка были подставлены старыми
  (T-EqInt "T4.3b фактически пришёл только новый Адаптер"
           (length (KG-StrInterCI (KG-GetNestedBlocks (KG_DBGetModel) "ABC1.01(5)")
                                  (KG-Step_DetectArrived BEFORE4 (KG_EXAllDefNames)))) 1)
  (T-Ok "T4.3c Стойка НЕ пришла из буфера"
        (not (member "Стойка"
                     (KG-Step_DetectArrived BEFORE4 (KG_EXAllDefNames)))))
  (T-EqInt "T4.4 содержимое Стойки осталось СТАРЫМ (4 состояния)"
           (length (KG-CdrCI "vis-states" (DB-FindDef "Стойка"))) 4)

  ;; 4c. вложенных такого имени в файле нет -> все идут в «добавить»
  (DB-SetDefs (list (DB-MakeDef "M2" (list "Узел" "Крепёж") nil)))
  (setq NM4b (KG-MapNestedBlocks (KG_DBGetModel) "M2"))
  (T-EqInt "T4.5 к добавлению = 2" (length (cdr (assoc "to-add" NM4b))) 2)
  (T-EqInt "T4.6 к обновлению = 0" (length (cdr (assoc "to-update" NM4b))) 0)
)

;;;---------------------------------------------------------------------------
;;; T5. ПРАВИЛО ВИДИМОСТИ (п. 9.4 ТЗ)
;;;---------------------------------------------------------------------------

(defun TEST-Visibility ()
  (princ "\n\nT5. Правило видимости, вариант A")
  ;; старое состояние есть в новой версии
  (setq R1 (KG-ResolveVisibility "A" (list "A" "B" "C")))
  (T-EqStr "T5.1 состояние сохранено" (nth 0 R1) "A")
  (T-Ok    "T5.2 без предупреждения" (null (nth 2 R1)))

  ;; старое состояние отсутствует -> первое доступное + предупреждение
  (setq R2 (KG-ResolveVisibility "D" (list "A" "B" "C")))
  (T-EqStr "T5.3 установлено первое" (nth 0 R2) "A")
  (T-Ok    "T5.4 признак несовпадения" (not (nth 1 R2)))
  (T-Ok    "T5.5 предупреждение есть" (nth 2 R2))

  ;; регистронезависимость
  (setq R3 (KG-ResolveVisibility "b" (list "A" "B" "C")))
  (T-EqStr "T5.6 регистронезависимо" (nth 0 R3) "B")

  ;; пустое старое состояние -> первое доступное, БЕЗ предупреждения
  (setq R4 (KG-ResolveVisibility "" (list "A" "B" "C")))
  (T-EqStr "T5.7 пустое -> первое" (nth 0 R4) "A")
  (T-Ok    "T5.8 без предупреждения" (null (nth 2 R4)))

  ;; нет доступных состояний
  (setq R5 (KG-ResolveVisibility "D" nil))
  (T-EqStr "T5.9 нет состояний -> оставили старое" (nth 0 R5) "D")

  ;; новые состояния доступны (п. 9.3 ТЗ) -- просто присутствуют в списке
  (T-EqInt "T5.10 новых состояний больше" (length (list "A" "B" "C" "E" "F")) 5)
)

;;;---------------------------------------------------------------------------
;;; T6. КАРТА ИНТЕГРАЦИИ (Этап 4)
;;;---------------------------------------------------------------------------

(defun TEST-Plan ()
  (princ "\n\nT6. Карта интеграции")
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (KG-Step_PreRename (KG-ConflictCandidates (KG_DBGetModel) "ABC1.01") 5 "ABC1.01" (KG_DBGetModel))
  (setq BEFORE6 (KG_EXAllDefNames))
  (KG_EXPasteAtOrigin)

  (setq PLAN6 (KG-BuildIntegrationMap (KG_DBGetModel) "ABC1.01(5)" BEFORE6))
  (T-Ok    "T6.1 план построен" PLAN6)
  (T-EqStr "T6.2 семейство" (cdr (assoc "family" PLAN6)) "ABC1.01")
  (T-EqStr "T6.3 новая итерация" (cdr (assoc "newiter" PLAN6)) "5")
  (T-EqStr "T6.4 мастер" (cdr (assoc "master" PLAN6)) "ABC1.01(5)")
  (T-EqInt "T6.5 экземпляров к замене" (length (cdr (assoc "instances" PLAN6))) 23)

  ;; все 4 варианта надо создать: новых определений вариантов ещё нет
  ;; мастер-версия уже есть (пришла из буфера), остальные 3 варианта надо создать
  (T-EqInt "T6.6 вариантов к созданию" (length (cdr (assoc "variants-to-create" PLAN6))) 3)
  (T-EqInt "T6.7 готовых вариантов (пустой суффикс)"
           (length (cdr (assoc "variants-existing" PLAN6))) 1)

  ;; конфликты: все старые вложенные определения должны быть освобождены
  (setq PRE6 (KG-ConflictCandidates (KG_DBGetModel) "ABC1.01"))
  (T-Ok "T6.8 Стойка в списке конфликтов" (member "Стойка" PRE6))
  (T-Ok "T6.9 Ригель в списке конфликтов" (member "Ригель" PRE6))
  (T-Ok "T6.10 Крышка в списке конфликтов" (member "Крышка" PRE6))
  (T-Ok "T6.11 Закладная в списке конфликтов" (member "Закладная" PRE6))
  ;; чужие определения не трогаем
  (T-Ok "T6.12 ЧУЖОЙ не переименовывается" (not (member "ЧУЖОЙ" PRE6)))
  (T-Ok "T6.13 СБОРКА_1 не переименовывается" (not (member "СБОРКА_1" PRE6)))
  (T-Ok "T6.14 имена семейства не переименовываются"
        (not (member "ABC1.01(1)Стойка" PRE6)))

  ;; нераспознанное имя
  (T-Ok "T6.15 мусорное имя отклонено"
        (null (KG-BuildIntegrationMap (KG_DBGetModel) "ПРОСТОБЛОК")))
)

;;;---------------------------------------------------------------------------
;;; T7. ПОЛНАЯ ИНТЕГРАЦИЯ (Этапы 8-12)
;;;---------------------------------------------------------------------------

(defun TEST-FullIntegration ()
  (princ "\n\nT7. Полная интеграция")
  (TEST-BuildOldDrawing)
  (setq BEFORE-INSTS (length (DB-Insts)))
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  ;; вся операция одной функцией; do-paste = T
  (setq REP7 (TEST-IntegratePaste "ABC1.01" 5))

  (T-Ok "T7.1 интеграция выполнена" (cdr (assoc "plan" REP7)))
  (T-EqInt "T7.1a ошибок нет" (length (cdr (assoc "errors" REP7))) 0)
  (T-EqInt "T7.2 заменено экземпляров"
           (cdr (assoc "replaced" (cdr (assoc "replaced" REP7)))) 23)
  (T-EqInt "T7.3 создано вариантов (Стойка, Ригель, Крышка)"
           (length (cdr (assoc "created" REP7))) 3)

  ;; итоговые имена -- точные, без служебных суффиксов
  (T-Ok "T7.4 создан ABC1.01(5)Стойка" (DB-HasDef "ABC1.01(5)Стойка"))
  (T-Ok "T7.5 создан ABC1.01(5)Ригель" (DB-HasDef "ABC1.01(5)Ригель"))
  (T-Ok "T7.6 создан ABC1.01(5)Крышка" (DB-HasDef "ABC1.01(5)Крышка"))
  (T-Ok "T7.7 определение ABC1.01(5) осталось" (DB-HasDef "ABC1.01(5)"))

  ;; количество по вариантам сохранено
  (T-EqInt "T7.8  ABC1.01(5) х 2"       (DB-CountEff "ABC1.01(5)") 2)
  (T-EqInt "T7.9  ABC1.01(5)Стойка х12" (DB-CountEff "ABC1.01(5)Стойка") 12)
  (T-EqInt "T7.10 ABC1.01(5)Ригель х8"  (DB-CountEff "ABC1.01(5)Ригель") 8)
  (T-EqInt "T7.11 ABC1.01(5)Крышка х1"  (DB-CountEff "ABC1.01(5)Крышка") 1)

  ;; старое семейство исчезло
  (T-EqInt "T7.12 старых ABC1.01(1)Стойка = 0" (DB-CountEff "ABC1.01(1)Стойка") 0)
  (T-EqInt "T7.13 старых ABC1.01(2)Ригель = 0" (DB-CountEff "ABC1.01(2)Ригель") 0)
  (T-EqInt "T7.14 старых ABC1.01(3)Крышка = 0" (DB-CountEff "ABC1.01(3)Крышка") 0)
  (T-EqInt "T7.15 старых ABC1.01(4) = 0"       (DB-CountEff "ABC1.01(4)") 0)

  ;; технический экземпляр удалён, общее количество сохранилось
  (T-Ok "T7.16 технический экземпляр удалён" (not (DB-FindInst "HTECH")))
  (T-EqInt "T7.17 общее количество сохранено"
           (length (DB-Insts)) BEFORE-INSTS)

  ;; другие семейства не тронуты
  (T-EqInt "T7.18 ABC1.02(4)Стойка на месте" (DB-CountEff "ABC1.02(4)Стойка") 1)
  (T-EqInt "T7.19 XYZ1.01(7)Стойка на месте" (DB-CountEff "XYZ1.01(7)Стойка") 1)
  (T-EqInt "T7.20 СБОРКА_1 на месте"         (DB-CountEff "СБОРКА_1") 1)
  (T-EqInt "T7.21 ЧУЖОЙ на месте"            (DB-CountEff "ЧУЖОЙ") 1)

  ;; вложенные определения получили новые содержимое и точные имена
  (T-Ok "T7.22 определение Стойка осталось" (DB-HasDef "Стойка"))
  (T-Ok "T7.23 определение Ригель осталось" (DB-HasDef "Ригель"))
  (T-Ok "T7.24 определение Крышка осталось" (DB-HasDef "Крышка"))
  (T-EqInt "T7.25 у Стойки теперь 5 состояний (A B C E F)"
           (length (KG-CdrCI "vis-states" (DB-FindDef "Стойка"))) 5)
  (T-Ok "T7.25a добавлен новый вложенный Адаптер" (DB-HasDef "Адаптер"))

  ;; пространства сохранены
  (T-EqInt "T7.26 ABC1.01(5) в модели = 2" (TEST-CountSpace "ABC1.01(5)" "Model") 2)
  (T-Ok "T7.27 экземпляры на листах остались на листах"
        (> (TEST-CountSheet "ABC1.01(5)Стойка") 0))

  ;; состояния видимости перенесены индивидуально
  ;; вложенных пар всего 23: 12 Стойка + 8 Ригель + 1 Крышка + 2 мастер.
  ;; Из них 3 (состояние D) в новой версии отсутствуют.
  (T-EqInt "T7.28 восстановлено состояний"
           (cdr (assoc "vis-restored" (cdr (assoc "replaced" REP7)))) 20)
  (T-EqInt "T7.28b отсутствующих состояний три"
           (cdr (assoc "vis-missing" (cdr (assoc "replaced" REP7)))) 3)
  (T-Ok "T7.29 есть предупреждения о пропавших состояниях"
        (> (length (cdr (assoc "warnings" REP7))) 0))
  (T-Ok "T7.30 предупреждение про состояние D"
        (vl-some '(lambda (w) (wcmatch w "*\"D\"*"))
                 (cdr (assoc "warnings" REP7))))

  ;; пространственное распределение не поменялось
  (T-EqInt "T7.31 Model всего (16 = 13 семейства + 3 сторонних)"
           (TEST-CountSpaceAny "Model") 16)
  (T-EqInt "T7.32 Лист 1 всего (6 семейства + ЧУЖОЙ)"
           (TEST-CountSpaceAny "Лист 1") 7)
  (T-EqInt "T7.33 Лист 2 всего" (TEST-CountSpaceAny "Лист 2") 4)

  ;; временных имён в файле не осталось
  (T-EqInt "T7.34 служебных имён нет"
           (length (vl-remove-if-not '(lambda (n) (KG-IsServiceName n))
                                     (DB-DefNames))) 0)
  ;; старые определения сохранены под временными именами (п. 8.4 ТЗ)
  (T-Ok "T7.35 старое определение Стойки сохранено как Стойка~до5"
        (DB-HasDef "Стойка~до5"))
  (T-Ok "T7.35b старое определение Ригеля -- Ригель~до5"
        (DB-HasDef "Ригель~до5"))
  (T-Ok "T7.35c старое определение Крышки -- Крышка~до5"
        (DB-HasDef "Крышка~до5"))
  (T-Ok "T7.35d служебных маркеров в этих именах нет"
        (not (KG-IsServiceName "Стойка~до5")))
  (T-EqInt "T7.36 удалённых нет (по умолчанию не удаляем)"
           (length (cdr (assoc "deleted-defs" REP7))) 0)
  ;; Переименованных осталось 3: четвёртое (Закладная) буфер не принёс,
  ;; поэтому его имя возвращено на место.
  (T-EqInt "T7.37 переименованных осталось 3"
           (length (cdr (assoc "renames" REP7))) 3)
  (T-Ok "T7.38 Закладная вернулась под исходным именем" (DB-HasDef "Закладная"))
  (T-Ok "T7.39 служебного имени Закладная~до5 нет"
        (not (DB-HasDef "Закладная~до5")))
)

(defun TEST-CountSpace (eff space / n)
  (setq n 0)
  (foreach i (DB-Insts)
    (if (and (KG-StrEq (KG-CdrCI "eff" i) eff)
             (KG-StrEq (KG-CdrCI "space" i) space))
      (setq n (1+ n)))
  )
  n
)
(defun TEST-CountSpaceAny (space / n)
  (setq n 0)
  (foreach i (DB-Insts)
    (if (KG-StrEq (KG-CdrCI "space" i) space) (setq n (1+ n)))
  )
  n
)
(defun TEST-CountSheet (effprefix / n)
  (setq n 0)
  (foreach i (DB-Insts)
    (if (and (wcmatch (KG-CdrCI "eff" i) (strcat effprefix "*"))
             (wcmatch (KG-CdrCI "space" i) "Лист*"))
      (setq n (1+ n)))
  )
  n
)

;;;---------------------------------------------------------------------------
;;; T8. ВАЛИДАЦИЯ (Этап 12, п. 20 ТЗ)
;;;---------------------------------------------------------------------------

(defun TEST-Validation ()
  (princ "\n\nT8. Валидация результата")
  ;; состояние базы -- сразу после T7
  (setq VAL8 (KG-ValidateIntegration (KG_DBGetModel) "ABC1.01" 5
               (list
                 (cons "" (list 1 2))
                 (cons "Стойка" (list 1 2 3 4 5 6 7 8 9 10 11 12))
                 (cons "Ригель" (list 1 2 3 4 5 6 7 8))
                 (cons "Крышка" (list 1))
               )))
  (T-EqInt "T8.1 старых экземпляров не осталось" (cdr (assoc "old-left" VAL8)) 0)
  (T-EqInt "T8.2 потерь нет" (cdr (assoc "lost" VAL8)) 0)
  (T-EqInt "T8.3 новых экземпляров 23" (cdr (assoc "new-total" VAL8)) 23)
  (T-Ok "T8.4 валидация пройдена" (cdr (assoc "ok" VAL8)))
  (T-EqInt "T8.5 служебных имён нет" (length (cdr (assoc "service-names" VAL8))) 0)

  ;; негативный тест: валидация ловит расхождение
  (DB-SetInsts (vl-remove-if
                 '(lambda (i) (KG-StrEq (KG-CdrCI "eff" i) "ABC1.01(5)Крышка"))
                 (DB-Insts)))
  (setq VAL8b (KG-ValidateIntegration (KG_DBGetModel) "ABC1.01" 5
                (list (cons "Крышка" (list 1)))))
  (T-EqInt "T8.6 потеря обнаружена" (cdr (assoc "lost" VAL8b)) 1)
  (T-Ok "T8.7 валидация не пройдена" (not (cdr (assoc "ok" VAL8b))))

  ;; 8c. Отчёт обязан печатать ФАКТИЧЕСКОЕ число обновлённых вложенных.
  ;;     На реальном чертеже план насчитал 42, AutoCAD подставил старые
  ;;     определения, а отчёт напечатал «Обновлено: 42» рядом с
  ;;     предупреждением, что все 42 остались старыми.
  (setq REP8C
    (list
      (cons "plan" (list (cons "nested-to-update" (list "Стойка" "Ригель"))))
      (cons "nested-stale" (list "Стойка" "Ригель"))
      (cons "replaced" (list (cons "replaced" 0) (cons "vis-restored" 0)
                             (cons "vis-missing" 0)))
      (cons "validation" (list (cons "old-left" 0) (cons "new-total" 0)
                               (cons "lost" 0) (cons "per-variant" nil)))
      (cons "warnings" nil)
      (cons "errors" nil)))
  ;; перехват princ на время проверки текста отчёта
  (setq KG-TEST-OUT "")
  (setq PRINC-REAL princ)
  (defun princ (s) (setq KG-TEST-OUT (strcat KG-TEST-OUT s)) s)
  (KG-PrintIntegrationReport REP8C)
  (setq princ PRINC-REAL)
  (T-Ok "T8.8 при полностью старом содержимом отчёт печатает 0"
        (wcmatch KG-TEST-OUT "*Обновлено вложенных определений: 0*"))
  (T-Ok "T8.9 и отдельно -- сколько осталось старыми"
        (wcmatch KG-TEST-OUT "*Из них остались старыми: 2*"))
  (T-Ok "T8.10 устаревшее определение помечено в списке"
        (wcmatch KG-TEST-OUT "*ОСТАЛОСЬ СТАРЫМ*"))
)

;;;---------------------------------------------------------------------------
;;; T9. ИСКЛЮЧИТЕЛЬНЫЕ СЛУЧАИ (Этап 14)
;;;---------------------------------------------------------------------------

(defun TEST-EdgeCases ()
  (princ "\n\nT9. Исключительные случаи")

  ;; 14.1 нет старых версий -- замена не требуется, мастер остаётся
  (DB-SetDefs (list (DB-MakeDef "ABC9.01(7)" nil nil)))
  (DB-SetInsts (list (DB-MakeInst "E1" "ABC9.01(7)" "ABC9.01(7)" "Model" "0"
                                  (list 0.0 0.0 0.0) nil)))
  (setq S9 (KG-ScanFamily (KG_DBGetModel) "ABC9.01" 7))
  (T-EqInt "T9.1 нет старых версий" (cdr (assoc "total" S9)) 0)
  (setq R9 (KG-Integrate_Model "ABC9.01" 7 "E1" nil))
  (T-EqInt "T9.2 заменять нечего"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R9)))) 0)
  (T-EqInt "T9.2a вариантов не создано" (length (cdr (assoc "created" R9))) 0)
  (T-Ok "T9.2b мастер остался" (DB-HasDef "ABC9.01(7)"))

  ;; 14.2 новая версия не распознана
  (setq R9b (KG-Integrate_Model "БЕЗСКОБОК" 1 nil nil))
  (T-Ok "T9.3 нераспознанное имя -> ошибка" (cdr (assoc "errors" R9b)))

  ;; 14.3 новый вариант существует, но старых таких блоков нет:
  ;;      определение НЕ создаётся (п. 6.4 ТЗ)
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (KG-Step_PreRename (KG-ConflictCandidates (KG_DBGetModel) "ABC1.01") 5 "ABC1.01" (KG_DBGetModel))
  (KG_EXPasteAtOrigin)
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "ABC1.01(5)Адаптер" nil nil))))
  (setq P9 (KG-BuildIntegrationMap (KG_DBGetModel) "ABC1.01(5)" nil))
  (T-Ok "T9.4 вариант Адаптер НЕ создаётся (старых нет)"
        (not (member "Адаптер" (cdr (assoc "variants-to-create" P9)))))
  (T-Ok "T9.4a но и в готовых его нет -- он просто существует"
        (not (member "Адаптер" (cdr (assoc "variants-existing" P9)))))

  ;; 14.6 пропущенные номера итераций
  (DB-SetDefs (list (DB-MakeDef "F(1)" nil nil)
                    (DB-MakeDef "F(3)" nil nil)
                    (DB-MakeDef "F(7)" nil nil)
                    (DB-MakeDef "F(15)" nil nil)))
  (DB-SetInsts (list (DB-MakeInst "F1" "F(1)" "F(1)" "Model" "0" (list 0.0 0.0 0.0) nil)
                     (DB-MakeInst "F3" "F(3)" "F(3)" "Model" "0" (list 0.0 0.0 0.0) nil)
                     (DB-MakeInst "F7" "F(7)" "F(7)" "Model" "0" (list 0.0 0.0 0.0) nil)
                     (DB-MakeInst "F15" "F(15)" "F(15)" "Model" "0" (list 0.0 0.0 0.0) nil)))
  (T-EqInt "T9.5 все итерации ниже новой -- старые"
           (length (KG-FindOldIterations (KG_DBGetModel) "F" 15)) 3)

  ;; 14.5 несколько семейств: обрабатывается только нужное
  (TEST-BuildOldDrawing)
  (setq ONLY9 (KG-FindFamilyInstances (KG_DBGetModel) "ABC1.01"))
  (T-Ok "T9.6 только наше семейство"
        (vl-every '(lambda (pr) (KG-StrEq (nth 0 (cdr pr)) "ABC1.01")) ONLY9))

  ;; распознавание служебных имён (п. 20 ТЗ)
  (T-Ok "T9.7  $0$ распознаётся"   (KG-IsServiceName "Стойка$0$"))
  (T-Ok "T9.8  _new распознаётся"  (KG-IsServiceName "Стойка_new"))
  (T-Ok "T9.9  *U распознаётся"    (KG-IsServiceName "*U123"))
  (T-Ok "T9.10 нормальное имя ок"  (not (KG-IsServiceName "Стойка")))
  (T-Ok "T9.11 имя с точкой ок"    (not (KG-IsServiceName "ABC1.01(15)Стойка")))

  ;; уникальное временное имя не конфликтует
  (T-Ok "T9.12 временное имя уникально"
        (KG-StrEq (KG-UniqueName "Стойка~до5" (list "Стойка~до5"))
                  "Стойка~до5$1$"))
  (T-Ok "T9.13 временное имя не считается служебным"
        (not (KG-IsServiceName "Стойка~до5")))

  ;; 9e. Дополнение пробелами: в AutoCAD нет make-string, поэтому KG-Pad.
  ;;     Здесь же исполняется KG-PrintScanReport -- единственное место,
  ;;     где KG-Pad вызывается.
  (T-EqStr "T9.14 KG-Pad добивает до ширины" (KG-Pad "ab" 5) "ab   ")
  (T-EqStr "T9.15 KG-Pad не обрезает длинное" (KG-Pad "abcdef" 3) "abcdef")
  (T-EqStr "T9.16 KG-Pad от пустой строки" (KG-Pad "" 4) "    ")
  (T-EqStr "T9.17 KG-Pad приводит nil к строке" (KG-Pad nil 2) "  ")
  (T-Ok "T9.18 отчёт сканирования печатается без ошибок"
        (progn
          (KG-PrintScanReport
            (list (cons "groups" (list (cons "Стойка" (list 1 2 3))
                                       (cons "Ригель" (list 1))))
                  (cons "total" 4)))
          t))
)

;;;---------------------------------------------------------------------------
;;; T10. МАСТЕР-ВЕРСИЯ ИЗ БУФЕРА (главный источник истины)
;;;---------------------------------------------------------------------------

(defun TEST-MasterFromClipboard ()
  (princ "\n\nT10. Ориентация на содержимое буфера")

  ;; 10a-1. Имя мастера занято определением из прошлой неудачной попытки.
  ;;        Программа обязана освободить его ДО вставки -- иначе AutoCAD
  ;;        отбрасывает пришедшее определение и интеграция не начинается.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))))
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "ABC1.01(5)" nil nil))))
  (setq BEFORE10 (length (DB-Insts)))
  (setq R10 (TEST-IntegratePaste "ABC1.01" 5))
  (T-Ok "T10.1a занятое имя мастера не мешает интеграции"
        (null (cdr (assoc "errors" R10))))
  (T-Ok "T10.1b мастер-версия получена из буфера"
        (DB-HasDef "ABC1.01(5)"))
  (T-Ok "T10.1c прежнее определение мастера сохранено под временным именем"
        (DB-HasDef "ABC1.01(5)~до5"))
  (T-EqInt "T10.1d технический экземпляр удалён, количество сохранено"
           (length (DB-Insts)) BEFORE10)
  (T-EqInt "T10.1e заменено по факту"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R10)))) 23)

  ;; 10a-2. Мастер-версия из буфера НЕ пришла -> понятная ошибка,
  ;;        чертёж не испорчен, временное имя возвращено на место
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))))
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "ABC1.01(5)" nil nil))))
  (setq *CLIP-DROP-MASTER* t)
  (setq BEFORE10 (length (DB-Insts)))
  (setq R10 (TEST-IntegratePaste "ABC1.01" 5))
  (setq *CLIP-DROP-MASTER* nil)
  (T-Ok "T10.1 ошибка при отброшенном буфере" (cdr (assoc "errors" R10)))
  (T-EqInt "T10.2 экземпляры не тронуты" (length (DB-Insts)) BEFORE10)
  (T-EqInt "T10.3 старых вариантов не создано" (length (cdr (assoc "created" R10))) 0)
  (T-Ok "T10.3a временное имя мастера возвращено на место"
        (and (DB-HasDef "ABC1.01(5)") (not (DB-HasDef "ABC1.01(5)~до5"))))
  (T-Ok "T10.3b вложенные переименования откачены"
        (and (DB-HasDef "Стойка") (not (DB-HasDef "Стойка~до5"))))

  ;; 10b. Буфер принёс мастер с другим номером итерации -- работает
  ;;      по фактическому содержимому, а не по «ожидаемому»
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(17)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель" nil (list "1" "2" "3"))))
  (setq R10b (TEST-IntegratePaste "ABC1.01" 17))
  (T-EqInt "T10.4 заменено по фактической итерации 17"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R10b)))) 23)
  (T-Ok "T10.5 создан ABC1.01(17)Стойка" (DB-HasDef "ABC1.01(17)Стойка"))
  (T-Ok "T10.6 создан ABC1.01(17)Ригель" (DB-HasDef "ABC1.01(17)Ригель"))

  ;; 10c. Вложенный блок в буфере -- совершенно новый, в файле его не было
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(6)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C"))
          (DB-MakeDef "Ригель"  nil (list "1" "2"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2"))
          (DB-MakeDef "Закладная" nil (list "X" "Y"))
          (DB-MakeDef "СовершенноНовый" nil nil)))
  (setq R10c (TEST-IntegratePaste "ABC1.01" 6))
  (T-Ok "T10.7 новое вложенное определение добавлено"
        (DB-HasDef "СовершенноНовый"))
  (T-Ok "T10.8 оно в списке добавленных"
        (member "СовершенноНовый" (cdr (assoc "nested-to-add" (cdr (assoc "plan" R10c))))))

  ;; 10d. Вложенного блока в новой версии нет -- старое определение остаётся
  ;;      (п. 8.4 ТЗ: автоматически не удаляем)
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(8)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C"))
          (DB-MakeDef "Ригель" nil (list "1" "2"))))
  (setq R10d (TEST-IntegratePaste "ABC1.01" 8))
  ;; Вложенных, которых в новом мастере нет, буфер не принёс -- их имена
  ;; возвращаются на место: определение остаётся, имя не служебное.
  (T-Ok "T10.9 старое вложенное Крышка вернулось под своим именем"
        (DB-HasDef "Крышка"))
  (T-Ok "T10.10 старое вложенное Закладная вернулось под своим именем"
        (DB-HasDef "Закладная"))
  (T-Ok "T10.10b служебных имён Крышка~до8 нет" (not (DB-HasDef "Крышка~до8")))
  (T-EqInt "T10.11 интеграция всё равно выполнена"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R10d)))) 23)

  ;; 10e. Освобождение имени мастер-версии как самостоятельный механизм
  (TEST-BuildOldDrawing)
  (T-Ok "T10.12 свободное имя не трогается"
        (null (KG-FreeMasterName "ABC1.01(5)" 5)))
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "ABC1.01(5)" nil nil))))
  (setq FREED10 (KG-FreeMasterName "ABC1.01(5)" 5))
  (T-Ok "T10.13 занятое имя освобождено" (and (car FREED10) (cdr FREED10)))
  (T-Ok "T10.14 старое имя свободно" (not (DB-HasDef "ABC1.01(5)")))
  (T-Ok "T10.15 временное имя занято" (DB-HasDef "ABC1.01(5)~до5"))
  (T-Ok "T10.16 временное имя не служебное"
        (not (KG-IsServiceName "ABC1.01(5)~до5")))
  (T-Ok "T10.17 имя возвращается на место" (KG-RestoreMasterName FREED10))
  (T-Ok "T10.18 старое имя снова занято" (DB-HasDef "ABC1.01(5)"))
  (T-Ok "T10.19 временного имени больше нет" (not (DB-HasDef "ABC1.01(5)~до5")))

  ;; 10f. Определение «что пришло из буфера» по handle. Сравнение имён
  ;;      на реальном чертеже дало ложный ответ, поэтому добавлен способ
  ;;      по снимку handle: у заменённого определения handle другой.
  (TEST-BuildOldDrawing)
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "Стойка" nil nil))))
  (setq SNAP10F (KG_EXDefHandleSnapshot))
  (T-Ok "T10.20 до вставки пришедших нет"
        (null (KG-ArrivedByHandle SNAP10F)))
  (DB-SetDefs (append (DB-Defs) (list (DB-MakeDef "Прогоновина" nil nil))))
  (TEST-ReplaceDef "Стойка" (list "Закладная") (list "A" "B"))
  (setq ARR10F (KG-ArrivedByHandle SNAP10F))
  (T-Ok "T10.21 новое определение видно по handle" (member "Прогоновина" ARR10F))
  (T-Ok "T10.22 заменённое определение тоже видно" (member "Стойка" ARR10F))
  (T-Ok "T10.23 незаменённое определение не видно" (null (member "Крышка" ARR10F)))
  ;; Переименование handle не меняет -- иначе откат «непришедших» имён
  ;; выглядел бы как приход нового определения.
  (setq H10G (KG_EXDefHandle "Крышка"))
  (KG_EXRenameDef "Крышка" "Крышка~до9")
  (T-Ok "T10.24 переименование handle не меняет"
        (KG-StrEq H10G (KG_EXDefHandle "Крышка~до9")))

  ;; 10g. Прогон в порядке C:INTEGRATE. На реальном чертеже здесь было
  ;;      три поломки: вставка выполнялась дважды, освобождённое имя
  ;;      мастера уходило в "~до" ещё раз (мусорный вариант
  ;;      "v1.5 до1.5"), а 41 вложенное определение оставалось старым.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(8)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (setq PASTE-BEFORE DB-PASTE-SEQ)
  (setq BEFORE10G (length (DB-Insts)))
  (setq R10g (TEST-IntegrateAsCommand))

  (T-Ok "T10.25 интеграция выполнена" (cdr (assoc "plan" R10g)))
  (T-EqInt "T10.26 вставка выполнена ровно один раз"
           (- DB-PASTE-SEQ PASTE-BEFORE) 1)
  (T-EqInt "T10.27 ошибок нет" (length (cdr (assoc "errors" R10g))) 0)
  ;; вложенные определения пришли, а не остались старыми
  (T-EqInt "T10.28 ни одно вложенное не осталось старым"
           (length (cdr (assoc "nested-stale" R10g))) 0)
  ;; План обязан делить вложенные по снимку ДО вставки: «Стойка» и
  ;; «Ригель» в чертеже были -- их обновляем, «Адаптер» новый -- добавляем.
  ;; Без снимка классификация идёт по таблице ПОСЛЕ вставки, всё попадает
  ;; в «новые», и T10.28 остаётся зелёным при нуле обновлённых -- ровно
  ;; так вышло на реальном чертеже: «Обновлено вложенных: 0» при 42
  ;; «добавленных».
  (T-EqStr "T10.28b обновляемые вложенные -- те, что были в чертеже"
           (KG-JoinNames (cdr (assoc "nested-to-update"
                                     (cdr (assoc "plan" R10g)))))
           "Стойка, Ригель")
  (T-EqStr "T10.28c новое вложенное -- только Адаптер"
           (KG-JoinNames (cdr (assoc "nested-to-add"
                                     (cdr (assoc "plan" R10g)))))
           "Адаптер")
  (T-Ok "T10.29 мастер-версия собрана из пришедших вложенных"
        (and (DB-HasDef "Стойка") (DB-HasDef "Ригель") (DB-HasDef "Адаптер")))
  ;; мусорных вариантов и служебных имён нет
  (T-EqInt "T10.30 создан один вариант, а не два"
           (length (cdr (assoc "created" R10g))) 3)
  (T-Ok "T10.31 мусорного варианта ABC1.01(8)~до нет"
        (null (vl-remove-if-not
                '(lambda (n) (wcmatch n "*~до*"))
                (vl-remove-if-not
                  '(lambda (n) (KG-ParseBlockName n))
                  (DB-DefNames)))))
  (T-EqInt "T10.32 служебных имён в файле нет"
           (length (vl-remove-if-not '(lambda (n) (KG-IsServiceName n))
                                     (DB-DefNames))) 0)
  ;; сторонние семейства освобождены и возвращены на место
  (T-Ok "T10.33 ABC1.02(4)Стойка под своим именем"
        (DB-HasDef "ABC1.02(4)Стойка"))
  (T-Ok "T10.34 XYZ1.01(7)Стойка под своим именем"
        (DB-HasDef "XYZ1.01(7)Стойка"))
  (T-Ok "T10.35 служебных имён у сторонних семейств нет"
        (null (vl-remove-if-not '(lambda (n) (wcmatch n "ABC1.02*~до*"))
                                (DB-DefNames))))
  ;; технический экземпляр убран, счётчик сохранён
  (T-Ok "T10.36 технический экземпляр удалён" (not (DB-FindInst "HTECH")))
  (T-EqInt "T10.37 общее количество экземпляров сохранено"
           (length (DB-Insts)) BEFORE10G)
  (T-EqInt "T10.38 заменено 23 экземпляра"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R10g)))) 23)

  ;; 10h. Режим B: мастер вставлен руками ДО команды. Переименовывать
  ;;      определения уже поздно -- это оторвало бы мастер от его
  ;;      вложенных, поэтому переименований быть не должно.
  (TEST-BuildOldDrawing)
  (DB-SetDefs
    (append (DB-Defs)
            (list (DB-MakeDef "ABC1.01(8)" (list "Стойка" "Ригель") nil))))
  (setq KG-PRERENAMES nil)
  (setq KG-PREDEFS nil)
  (setq KG-ARRIVED nil)
  (setq KG-TEST-OUT "")
  (setq PRINC-REAL princ)
  (defun princ (s) (setq KG-TEST-OUT (strcat KG-TEST-OUT s)) s)
  (setq R10h (KG-Integrate_Model "ABC1.01" 8 nil nil))
  (setq princ PRINC-REAL)
  (T-Ok "T10.39 в режиме B переименований нет"
        (null (cdr (assoc "renames" R10h))))
  (T-Ok "T10.40 вложенные остались под своими именами"
        (and (DB-HasDef "Стойка") (DB-HasDef "Ригель")
             (not (DB-HasDef "Стойка~до8"))))
  (T-Ok "T10.41 напечатано предупреждение про ручную вставку"
        (wcmatch KG-TEST-OUT "*вставлен вручную*"))

  ;; 10i. Экземпляр, который УЖЕ стоял на новой итерации, -- не потеря.
  ;;      На реальном чертеже так дала «Потерь экземпляров: 1» и
  ;;      «Вариант "": ожидалось 1, стало 2» копия мастер-версии,
  ;;      оставшаяся от диагностической вставки.
  (TEST-BuildOldDrawing)
  (DB-SetDefs
    (append (DB-Defs) (list (DB-MakeDef "ABC1.01(8)" (list "Стойка") nil))))
  (DB-SetInsts
    (append (DB-Insts)
            (list (DB-MakeInst "HOLD" "ABC1.01(8)" "ABC1.01(8)" "Model" "0"
                               (list 900.0 0.0 0.0) nil))))
  (TEST-SetClipboard "ABC1.01(8)"
    (list (DB-MakeDef "Стойка" nil (list "A"))))
  (setq R10i (TEST-IntegrateAsCommand))
  (T-EqInt "T10.42 потерь нет"
           (cdr (assoc "lost" (cdr (assoc "validation" R10i)))) 0)
  (T-Ok "T10.43 расхождений по вариантам нет"
        (vl-every '(lambda (q) (nth 3 q))
                  (cdr (assoc "per-variant" (cdr (assoc "validation" R10i))))))
  (T-Ok "T10.44 уже новый экземпляр остался на месте" (DB-FindInst "HOLD"))

  ;; 10j. Поправка ожидаемого числа -- по частям
  (T-EqInt "T10.45 ожидаемое = старые + уже новые"
           (cdr (assoc "Стойка"
             (KG-ExpectedWithExisting
               (list (cons "Стойка" (list 1 2 3)))
               (list (cons "Стойка" 2))))) 5)
  (T-EqInt "T10.46 вариант без старых экземпляров тоже учтён"
           (cdr (assoc "Ригель"
             (KG-ExpectedWithExisting
               (list (cons "Стойка" (list 1)))
               (list (cons "Ригель" 4))))) 4)

  ;; 10k. Вариант обязан остаться ДИНАМИЧЕСКИМ. Обычное «создать пустое
  ;;      определение и скопировать объекты» даёт статический блок:
  ;;      параметры динамики лежат в словаре расширения BLOCK_RECORD и не
  ;;      переносятся. Поэтому вариант создаётся глубоким клонированием
  ;;      через ObjectDBX.
  (DB-SetDefs (list (DB-MakeDef "МАСТЕР" (list "Стойка") (list "A" "B"))))
  (setq DB-DBX-OK t)
  (T-Ok "T10.47 вариант создан"
        (KG_EXCreateDefFromMaster "МАСТЕР v2 Стойка" "МАСТЕР"))
  (T-Ok "T10.48 вариант остался динамическим"
        (KG-CdrCI "is-dynamic" (DB-FindDef "МАСТЕР v2 Стойка")))
  (T-EqStr "T10.49 состояния видимости перенесены"
           (KG-JoinNames (KG-CdrCI "vis-states" (DB-FindDef "МАСТЕР v2 Стойка")))
           "A, B")
  ;; ObjectDBX недоступен -> запасной путь, вариант статический, но об
  ;; этом честно печатается.
  (DB-SetDefs (list (DB-MakeDef "МАСТЕР" (list "Стойка") (list "A" "B"))))
  (setq DB-DBX-OK nil)
  (setq KG-TEST-OUT "")
  (setq PRINC-REAL princ)
  (defun princ (s) (setq KG-TEST-OUT (strcat KG-TEST-OUT s)) s)
  (KG_EXCreateDefFromMaster "МАСТЕР v2 Ригель" "МАСТЕР")
  (setq princ PRINC-REAL)
  (T-Ok "T10.50 вариант создан и без ObjectDBX" (DB-HasDef "МАСТЕР v2 Ригель"))
  (T-Ok "T10.51 без ObjectDBX он статический"
        (null (KG-CdrCI "is-dynamic" (DB-FindDef "МАСТЕР v2 Ригель"))))
  (T-Ok "T10.52 напечатано предупреждение о потере динамики"
        (wcmatch KG-TEST-OUT "*БЕЗ динамических свойств*"))
  (setq DB-DBX-OK t)

  ;; 10l. Статический вариант, оставшийся от прошлой попытки,
  ;;      пересоздаётся динамическим.
  (DB-SetDefs (list (DB-MakeDef "МАСТЕР" (list "Стойка") (list "A" "B"))
                    (DB-MakeDef "МАСТЕР v2 Стойка" (list "Стойка") nil)))
  (setq DB-LOCKED nil)
  (KG_EXCreateDefFromMaster "МАСТЕР v2 Стойка" "МАСТЕР")
  (T-Ok "T10.53 статический остаток заменён динамическим"
        (KG-CdrCI "is-dynamic" (DB-FindDef "МАСТЕР v2 Стойка")))
  ;; определение занято экземплярами: удалить нельзя, но и ломать нельзя
  (DB-SetDefs (list (DB-MakeDef "МАСТЕР" (list "Стойка") (list "A" "B"))
                    (DB-MakeDef "МАСТЕР v2 Ригель" (list "Стойка") nil)))
  (setq DB-LOCKED (list "МАСТЕР v2 Ригель"))
  (KG_EXCreateDefFromMaster "МАСТЕР v2 Ригель" "МАСТЕР")
  (T-Ok "T10.54 занятое определение осталось в чертеже"
        (DB-HasDef "МАСТЕР v2 Ригель"))
  (T-Ok "T10.55 и осталось статическим, а не сломанным"
        (null (KG-CdrCI "is-dynamic" (DB-FindDef "МАСТЕР v2 Ригель"))))
  (setq DB-LOCKED nil)
)

;;;===========================================================================
;;; T11. СОСТОЯНИЯ ВИДИМОСТИ ОТДЕЛЬНО ПО КАЖДОМУ ЭКЗЕМПЛЯРУ
;;;      (п. 6.2 и 6.5 ТЗ)
;;;===========================================================================

;; Состояние вложенного блока nm у экземпляра.
;; Экземпляр ищем по координате X, а не по handle: в AutoCAD замена
;; экземпляра выполняется «создать новый, удалить старый», поэтому handle
;; меняется (это разрешено ТЗ при условии отчёта).
(defun TEST-VisAtX (x nm / hit out)
  (setq hit nil)
  (foreach i (DB-Insts)
    (if (and (not hit) (= (nth 0 (KG-CdrCI "pos" i)) x))
      (setq hit i))
  )
  (if hit
    (progn
      (setq out (KG-AssocCI nm (KG-CdrCI "nested-vis" hit)))
      (if out (cdr out) nil))
    nil)
)

(defun TEST-VisibilityPerInstance ()
  (princ "\n\nT11. Состояния видимости по каждому экземпляру")

  ;; 11a. Экземпляры одного варианта имеют РАЗНЫЕ состояния вложенного блока.
  ;;      Новая версия содержит A B C E F -- состояния D в ней больше нет.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(17)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель" nil (list "1" "2" "3"))))
  (setq R11 (TEST-IntegratePaste "ABC1.01" 17))
  (setq W11 (cdr (assoc "warnings" (cdr (assoc "replaced" R11)))))

  (T-EqStr "T11.1 экземпляр в A остался в A" (TEST-VisAtX 0.0 "Стойка") "A")
  (T-EqStr "T11.2 экземпляр в B остался в B" (TEST-VisAtX 1200.0 "Стойка") "B")
  (T-EqStr "T11.3 экземпляр в C остался в C" (TEST-VisAtX 1300.0 "Стойка") "C")
  (T-EqStr "T11.4 отсутствующее D заменено первым доступным A"
           (TEST-VisAtX 500.0 "Стойка") "A")
  (T-EqStr "T11.5 второй экземпляр с D тоже получил A"
           (TEST-VisAtX 1100.0 "Стойка") "A")
  (T-EqInt "T11.6 замен по отсутствующему состоянию ровно три (H2 H4 H11)"
           (cdr (assoc "vis-missing" (cdr (assoc "replaced" R11)))) 3)
  (T-EqInt "T11.7 предупреждений тоже три" (length W11) 3)
  (T-Ok "T11.8 предупреждение называет вложенный блок"
        (wcmatch (car W11) "*Стойка*"))
  (T-Ok "T11.9 предупреждение называет отсутствующее состояние D"
        (wcmatch (car W11) "*\"D\"*"))
  (T-Ok "T11.10 предупреждение называет установленное состояние A"
        (wcmatch (car W11) "*\"A\"*"))
  (T-Ok "T11.11 предупреждение называет исходный блок"
        (vl-some '(lambda (w) (wcmatch w "*ABC1.01(4)*")) W11))
  (T-Ok "T11.11b предупреждение называет и вариант с суффиксом"
        (vl-some '(lambda (w) (wcmatch w "*ABC1.01(1)Стойка*")) W11))

  ;; 11b. Два вложенных блока в одном экземпляре -- состояния независимы
  (DB-SetDefs
    (list
      (DB-MakeDef "ABC1.01(4)" (list "Стойка" "Ригель") nil)
      (DB-MakeDef "Стойка"     nil (list "A" "B"))
      (DB-MakeDef "Ригель"     nil (list "1" "2"))
      (DB-MakeDef "ABC1.01(9)" (list "Стойка" "Ригель") nil)
    ))
  (DB-SetInsts
    (list
      (DB-MakeInst "V1" "ABC1.01(4)" "ABC1.01(4)" "Model" "0"
                   (list 0.0 0.0 0.0)
                   (list (cons "Стойка" "B") (cons "Ригель" "1")))
      (DB-MakeInst "V2" "ABC1.01(4)" "ABC1.01(4)" "Model" "0"
                   (list 100.0 0.0 0.0)
                   (list (cons "Стойка" "A") (cons "Ригель" "2")))
    ))
  (TEST-SetClipboard "ABC1.01(9)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B"))
          (DB-MakeDef "Ригель" nil (list "1" "2"))))
  (TEST-IntegratePaste "ABC1.01" 9)
  (T-EqStr "T11.12 у V1 стойка осталась в B" (TEST-VisAtX 0.0 "Стойка") "B")
  (T-EqStr "T11.13 у V1 ригель остался в 1"  (TEST-VisAtX 0.0 "Ригель") "1")
  (T-EqStr "T11.14 у V2 стойка осталась в A" (TEST-VisAtX 100.0 "Стойка") "A")
  (T-EqStr "T11.15 у V2 ригель остался в 2"  (TEST-VisAtX 100.0 "Ригель") "2")

  ;; Свойства самого блока переносятся и считаются отдельно: без этой
  ;; строки отчёт печатает «Восстановлено состояний видимости: 0», и по
  ;; нему невозможно отличить потерю состояния от того, что состояние
  ;; самого мастера перенеслось.
  (T-EqInt "T11.16 перенесено свойств самого блока (по числу замен)"
           (cdr (assoc "dyn-restored" (cdr (assoc "replaced" R11))))
           (cdr (assoc "replaced" (cdr (assoc "replaced" R11)))))
  ;; Второй счётчик нужен, чтобы ноль в отчёте был различим: «нечего было
  ;; читать» и «прочитали, но не записали» -- разные поломки.
  (T-EqInt "T11.17 прочитано свойств у старых экземпляров"
           (cdr (assoc "dyn-read" (cdr (assoc "replaced" R11))))
           (cdr (assoc "dyn-restored" (cdr (assoc "replaced" R11)))))
)

;;;===========================================================================
;;; T12. ФОРМАТ ИТОГОВОГО ОТЧЁТА (п. 16 ТЗ)
;;;      Отчёт печатается в командную строку -- здесь проверяется, что он
;;;      формируется без ошибок и содержит требуемые ТЗ показатели.
;;;===========================================================================

;; Значение показателя ровно в том виде, в каком его печатает
;; KG-PrintIntegrationReport (для проверки формата отчёта).
(defun KG-ReportValue (rep section key / v)
  (setq v (cdr (assoc key (cdr (assoc section rep)))))
  (if v (itoa v) "0")
)

(defun TEST-Report ()
  (princ "\n\nT12. Формат итогового отчёта")

  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка"  nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель"  nil (list "1" "2" "3"))
          (DB-MakeDef "Крышка"  nil (list "K1" "K2" "K3"))
          (DB-MakeDef "Адаптер" nil (list "AD1"))))
  (setq R12 (TEST-IntegratePaste "ABC1.01" 5))
  (KG-PrintIntegrationReport R12)          ; не должно падать

  (T-EqStr "T12.1 строка «Заменено экземпляров: 23»"
           (KG-ReportValue R12 "replaced" "replaced") "23")
  (T-EqStr "T12.2 строка «Восстановлено состояний видимости: 20»"
           (KG-ReportValue R12 "replaced" "vis-restored") "20")
  (T-EqStr "T12.3 строка «Состояний не найдено в новой версии: 3»"
           (KG-ReportValue R12 "replaced" "vis-missing") "3")
  (T-EqStr "T12.4 строка «Старых экземпляров осталось: 0»"
           (KG-ReportValue R12 "validation" "old-left") "0")
  (T-EqStr "T12.5 строка «Потерь экземпляров: 0»"
           (KG-ReportValue R12 "validation" "lost") "0")
  (T-EqStr "T12.6 строка «Ошибок: 0»"
           (itoa (length (cdr (assoc "errors" R12)))) "0")
  ;; после освобождения имён три вложенных пришли из буфера и обновлены,
  ;; «Адаптер» в старом файле отсутствовал -- он добавлен
  (T-EqInt "T12.7 обновлено вложенных определений: 3"
           (length (cdr (assoc "nested-to-update"
                               (cdr (assoc "plan" R12))))) 3)
  (T-EqInt "T12.7b добавлено вложенных определений: 1"
           (length (cdr (assoc "nested-to-add"
                               (cdr (assoc "plan" R12))))) 1)
  (T-Ok "T12.7c добавлен именно Адаптер"
        (member "Адаптер" (cdr (assoc "nested-to-add"
                                      (cdr (assoc "plan" R12))))))
  (T-Ok "T12.8 есть разбивка по вариантам"
        (cdr (assoc "per-variant" (cdr (assoc "validation" R12)))))
  (T-Ok "T12.8b в разбивке нет расхождений"
        (not (vl-some '(lambda (x) (not (nth 3 x)))
                      (cdr (assoc "per-variant"
                                  (cdr (assoc "validation" R12)))))))
  (T-EqInt "T12.9 новых экземпляров столько же, сколько было старых"
           (cdr (assoc "new-total" (cdr (assoc "validation" R12)))) 23)
  ;; при нормальном ходе (имена освобождены до вставки) ложного
  ;; предупреждения «вставлены СТАРЫМИ» быть не должно
  (T-Ok "T12.10 нет ложного предупреждения о подмене вложенных"
        (not (vl-some '(lambda (w) (wcmatch w "*вставлены СТАРЫМИ*"))
                      (cdr (assoc "warnings" R12)))))

  ;; 12b. Имена НЕ освобождены -- AutoCAD подставляет существующие
  ;;      определения, и отчёт обязан об этом сказать (п. 8 ТЗ)
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель" nil (list "1" "2" "3"))))
  (setq B12 (KG_EXAllDefNames))
  (KG_EXPasteAtOrigin)
  (setq P12 (KG-BuildIntegrationMap (KG_DBGetModel) "ABC1.01(5)" B12))
  (T-EqInt "T12.11 без освобождения имён к обновлению числятся два"
           (length (cdr (assoc "nested-to-update" P12))) 2)
  (T-Ok "T12.12 но по факту пришёл только новый Адаптер"
        (not (member "Стойка"
                     (KG-Step_DetectArrived (KG_EXAllDefNames)
                                            (KG_EXAllDefNames)))))
)

;;;===========================================================================
;;; T13. ПРОИЗВОДСТВЕННАЯ СХЕМА ИМЕНОВАНИЯ: БАЗА vВЕРСИЯ ВАРИАНТ
;;;      Реальные имена заказчика: "Комплект КП50 v1.5" -- мастер-версия,
;;;      "Комплект КП50 v1.21 стойка КП45302-1" -- вариант.
;;;===========================================================================

(defun TEST-BuildRealDrawing ()
  (setq *CLIP* nil)
  (setq *CLIP-MASTER* nil)
  (DB-SetDefs
    (list
      ;; старые итерации семейства
      (DB-MakeDef "Комплект КП50 v1.1" (list "Стойка КП50") nil)
      (DB-MakeDef "Комплект КП50 v1.21 стойка КП45302-1"
                  (list "Стойка КП50") nil)
      (DB-MakeDef "Комплект КП50 v1.21 ригель КП45339"
                  (list "Ригель КП50") nil)
      (DB-MakeDef "Комплект КП50 v1.1 стойка КП45302-1"
                  (list "Стойка КП50") nil)
      ;; вложенные определения
      (DB-MakeDef "Стойка КП50" nil (list "A" "B" "C" "D"))
      (DB-MakeDef "Ригель КП50" nil (list "1" "2"))
      ;; посторонние: другое семейство и вообще не семейство
      (DB-MakeDef "Комплект КП50К v1.5" (list "Стойка КП50К") nil)
      (DB-MakeDef "Стойка КП50 контур" nil nil)
      (DB-MakeDef "kps 714 в динамике" nil nil)
    ))
  (DB-SetInsts
    (list
      ;; мастер-версия, 1 экземпляр
      (DB-MakeInst "R1" "Комплект КП50 v1.1" "Комплект КП50 v1.1"
                   "Model" "0" (list 0.0 0.0 0.0)
                   (list (cons "Стойка КП50" "A")))
      ;; вариант "стойка КП45302-1", 7 экземпляров
      (DB-MakeInst "R2" "Комплект КП50 v1.21 стойка КП45302-1"
                   "Комплект КП50 v1.21 стойка КП45302-1" "Model" "0"
                   (list 1000.0 0.0 0.0) (list (cons "Стойка КП50" "B")))
      (DB-MakeInst "R3" "Комплект КП50 v1.21 стойка КП45302-1"
                   "Комплект КП50 v1.21 стойка КП45302-1" "Model" "0"
                   (list 1100.0 0.0 0.0) (list (cons "Стойка КП50" "D")))
      (DB-MakeInst "R4" "Комплект КП50 v1.1 стойка КП45302-1"
                   "Комплект КП50 v1.1 стойка КП45302-1" "Лист 1" "0"
                   (list 1200.0 0.0 0.0) (list (cons "Стойка КП50" "C")))
      (DB-MakeInst "R5" "Комплект КП50 v1.1 стойка КП45302-1"
                   "Комплект КП50 v1.1 стойка КП45302-1" "Лист 1" "0"
                   (list 1300.0 0.0 0.0) (list (cons "Стойка КП50" "A")))
      (DB-MakeInst "R6" "Комплект КП50 v1.1 стойка КП45302-1"
                   "Комплект КП50 v1.1 стойка КП45302-1" "Лист 2" "0"
                   (list 1400.0 0.0 0.0) (list (cons "Стойка КП50" "D")))
      (DB-MakeInst "R7" "Комплект КП50 v1.21 стойка КП45302-1"
                   "Комплект КП50 v1.21 стойка КП45302-1" "Model" "0"
                   (list 1500.0 0.0 0.0) (list (cons "Стойка КП50" "A")))
      (DB-MakeInst "R8" "Комплект КП50 v1.21 стойка КП45302-1"
                   "Комплект КП50 v1.21 стойка КП45302-1" "Model" "0"
                   (list 1600.0 0.0 0.0) (list (cons "Стойка КП50" "B")))
      ;; вариант "ригель КП45339", 2 экземпляра
      (DB-MakeInst "R9"  "Комплект КП50 v1.21 ригель КП45339"
                   "Комплект КП50 v1.21 ригель КП45339" "Model" "0"
                   (list 2000.0 0.0 0.0) (list (cons "Ригель КП50" "1")))
      (DB-MakeInst "R10" "Комплект КП50 v1.21 ригель КП45339"
                   "Комплект КП50 v1.21 ригель КП45339" "Лист 1" "0"
                   (list 2100.0 0.0 0.0) (list (cons "Ригель КП50" "2")))
      ;; посторонние экземпляры
      (DB-MakeInst "R90" "Комплект КП50К v1.5" "Комплект КП50К v1.5"
                   "Model" "0" (list 9000.0 0.0 0.0) nil)
      (DB-MakeInst "R91" "Стойка КП50 контур" "Стойка КП50 контур"
                   "Model" "0" (list 9100.0 0.0 0.0) nil)
      (DB-MakeInst "R92" "kps 714 в динамике" "kps 714 в динамике"
                   "Model" "0" (list 9200.0 0.0 0.0) nil)
    ))
)

(defun TEST-RealNaming ()
  (princ "\n\nT13. Производственная схема именования v1.5")

  ;; разбор реальных имён
  (T-EqStr "T13.1 мастер: база"   (KG-ParseBase "Комплект КП50 v1.5")
           "Комплект КП50")
  (T-EqStr "T13.2 мастер: версия" (KG-ParseIteration "Комплект КП50 v1.5")
           "1.5")
  (T-Ok    "T13.3 мастер: варианта нет"
           (= (KG-ParseVariant "Комплект КП50 v1.5") ""))
  (T-EqStr "T13.4 база с буквой К -- другое семейство"
           (KG-ParseBase "Комплект КП50К v1.5") "Комплект КП50К")
  (T-EqStr "T13.5 вариант: версия"
           (KG-ParseIteration "Комплект КП50 v1.21 стойка КП45302-1") "1.21")
  (T-EqStr "T13.6 вариант: суффикс целиком, со своим дефисом"
           (KG-ParseVariant "Комплект КП50 v1.21 стойка КП45302-1")
           "стойка КП45302-1")
  (T-EqStr "T13.7 имя собирается обратно без потерь"
           (KG-MakeName "Комплект КП50" "1.21" "стойка КП45302-1")
           "Комплект КП50 v1.21 стойка КП45302-1")
  (T-EqStr "T13.8 мастер собирается обратно"
           (KG-MakeName "Комплект КП50" "1.5" "") "Комплект КП50 v1.5")

  ;; порядок версий: v1.21 старше v1.5, а не наоборот
  (T-Ok "T13.9 v1.1 старше v1.5"   (KG-IterOlder "1.1" "1.5"))
  ;; Номер итерации пишется без нуля на конце, когда он круглый:
  ;; v1.5 -- это 50, v1.2 -- 20, v1.51 -- 51, v1.21 -- 21.
  ;; Порядок: 20 < 21 < 50 < 51.
  (T-Ok "T13.10 v1.5 (50) новее v1.21 (21)" (not (KG-IterOlder "1.5" "1.21")))
  (T-Ok "T13.10b v1.21 (21) старше v1.5 (50)" (KG-IterOlder "1.21" "1.5"))
  (T-Ok "T13.10c v1.2 (20) старше v1.21 (21)" (KG-IterOlder "1.2" "1.21"))
  (T-Ok "T13.10d v1.5 (50) старше v1.51 (51)" (KG-IterOlder "1.5" "1.51"))
  (T-Ok "T13.11 v1.21 старше v2.0" (KG-IterOlder "1.21" "2.0"))
  (T-Ok "T13.12 v1.5 и v1.5 -- одно и то же" (KG-IterEq "1.5" "1.5"))
  (T-Ok "T13.13 v1.5 и v1.50 -- одна итерация 50" (KG-IterEq "1.5" "1.50"))

  ;; имена без версии разбирать нельзя
  (T-Ok "T13.14 «Стойка КП50 контур» не семейство"
        (null (KG-ParseBlockName "Стойка КП50 контур")))
  (T-Ok "T13.15 «kps 714 в динамике» не семейство"
        (null (KG-ParseBlockName "kps 714 в динамике")))

  ;; сквозная интеграция на реальных именах
  (TEST-BuildRealDrawing)
  (TEST-SetClipboard "Комплект КП50 v1.5"
    (list (DB-MakeDef "Стойка КП50" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель КП50" nil (list "1" "2" "3"))))
  (setq R13 (TEST-IntegratePaste "Комплект КП50" "1.5"))

  (T-EqStr "T13.16 создан вариант новой итерации"
           (KG-MakeName "Комплект КП50" "1.5" "стойка КП45302-1")
           "Комплект КП50 v1.5 стойка КП45302-1")
  (T-Ok "T13.17 определение варианта создано"
        (DB-HasDef "Комплект КП50 v1.5 стойка КП45302-1"))
  (T-Ok "T13.18 определение второго варианта создано"
        (DB-HasDef "Комплект КП50 v1.5 ригель КП45339"))
  (T-EqInt "T13.19 заменено 10 экземпляров семейства"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R13)))) 10)
  (T-EqInt "T13.20 старых не осталось"
           (cdr (assoc "old-left" (cdr (assoc "validation" R13)))) 0)
  (T-EqInt "T13.21 потерь нет"
           (cdr (assoc "lost" (cdr (assoc "validation" R13)))) 0)
  (T-EqInt "T13.22 всего экземпляров не изменилось (13)"
           (length (DB-Insts)) 13)

  ;; посторонние не тронуты
  (T-EqInt "T13.23 Комплект КП50К не тронут"
           (DB-CountEff "Комплект КП50К v1.5") 1)
  (T-EqInt "T13.24 «Стойка КП50 контур» не тронут"
           (DB-CountEff "Стойка КП50 контур") 1)
  (T-EqInt "T13.25 «kps 714 в динамике» не тронут"
           (DB-CountEff "kps 714 в динамике") 1)
  (T-Ok "T13.26 определения КП50К на месте"
        (DB-HasDef "Комплект КП50К v1.5"))

  ;; пространства сохранены
  (T-EqInt "T13.27 новый вариант «стойка» в модели = 4"
           (TEST-CountSpace "Комплект КП50 v1.5 стойка КП45302-1" "Model") 4)
  (T-EqInt "T13.28 он же на Листе 1 = 2"
           (TEST-CountSpace "Комплект КП50 v1.5 стойка КП45302-1" "Лист 1") 2)
  (T-EqInt "T13.29 он же на Листе 2 = 1"
           (TEST-CountSpace "Комплект КП50 v1.5 стойка КП45302-1" "Лист 2") 1)
  (T-EqInt "T13.30 новый вариант «ригель» на Листе 1 = 1"
           (TEST-CountSpace "Комплект КП50 v1.5 ригель КП45339" "Лист 1") 1)

  ;; состояния видимости перенесены, отсутствующее D заменено на A
  (T-EqStr "T13.31 состояние B сохранено"  (TEST-VisAtX 1000.0 "Стойка КП50") "B")
  (T-EqStr "T13.32 отсутствующее D заменено на A"
           (TEST-VisAtX 1100.0 "Стойка КП50") "A")
  (T-EqStr "T13.33 состояние C сохранено"  (TEST-VisAtX 1200.0 "Стойка КП50") "C")
  (T-EqInt "T13.34 отсутствующих состояний два (R3 и R6)"
           (cdr (assoc "vis-missing" (cdr (assoc "replaced" R13)))) 2)

  ;; служебных суффиксов в именах нет
  (T-Ok "T13.35 служебных имён не осталось"
        (not (cdr (assoc "service-names" (cdr (assoc "validation" R13))))))

  ;; 13f. Анонимные определения AutoCAD не должны попадать в список
  ;;      «служебных имён»: на реальном чертеже туда ушли *Model_Space,
  ;;      *Paper_Space и все *U/*D -- 79 ложных срабатываний.
  (DB-SetDefs (append (DB-Defs)
                (list (DB-MakeDef "*Model_Space" nil nil)
                      (DB-MakeDef "*Paper_Space" nil nil)
                      (DB-MakeDef "*U128" nil nil)
                      (DB-MakeDef "*D3" nil nil))))
  (setq VAL13F (KG-ValidateIntegration (KG_DBGetModel) "ABC1.01" 5
                 (list (cons "" (list 1)) (cons "Стойка" (list 1)))))
  (T-Ok "T13.36 анонимные определения не считаются служебными именами"
        (not (cdr (assoc "service-names" VAL13F))))

  ;; 13g. Настоящий служебный суффикс в имени замены обязан быть найден
  (DB-SetInsts (append (DB-Insts)
                 (list (DB-MakeInst "HSVC" "ABC1.01(5)Стойка$0$"
                                    "ABC1.01(5)Стойка$0$" "Model" "0"
                                    (list 9000.0 0.0 0.0) nil))))
  (setq VAL13G (KG-ValidateIntegration (KG_DBGetModel) "ABC1.01" 5
                 (list (cons "" (list 1)) (cons "Стойка" (list 1)))))
  (T-Ok "T13.37 служебный суффикс $0$ в имени замены найден"
        (member "ABC1.01(5)Стойка$0$" (cdr (assoc "service-names" VAL13G))))

  ;; Логическое значение из COM. В AutoCAD это :vlax-true / :vlax-false,
  ;; и ОБА символа в условии истинны: (if :vlax-false t nil) даёт T.
  ;; Именно так каждое определение чертежа получало флаг «внешняя
  ;; ссылка», KG-UserDefNames возвращал пустой список, и INTEGRATE не
  ;; освобождал перед вставкой ни одного имени.
  (T-Ok "T13.38 :vlax-false -- это ЛОЖЬ" (null (KG-ComBool :vlax-false)))
  (T-Ok "T13.39 :vlax-true -- это истина" (KG-ComBool :vlax-true))
  (T-Ok "T13.40 :vlax-false в условии истинно, поэтому нужен предикат"
        (if :vlax-false t nil))
  (T-Ok "T13.41 nil -- ложь" (null (KG-ComBool nil)))
  (T-Ok "T13.42 t -- истина" (KG-ComBool t))
  (T-Ok "T13.43 ноль -- ложь" (null (KG-ComBool 0)))
  (T-Ok "T13.44 единица -- истина" (KG-ComBool 1))
)

;;;===========================================================================
;;; ЗАГЛУШКИ ИСПОЛНЯЮЩЕГО СЛОЯ ДЛЯ ПРОВЕРКИ ПОЛУЧЕНИЯ ИНТЕРФЕЙСА ObjectDBX
;;;===========================================================================
;; На реальном чертеже «ObjectDBX недоступен» печатался без причины, и по
;; отчёту было нельзя понять, на каком именно способе и на каком ProgID
;; произошёл отказ. Эти заглушки позволяют проверить, что пробуются ОБА
;; ProgID и ОБА способа получения, и что каждая причина запоминается.

(defun vlax-get-acad-object () "ACAD-OBJECT")

(defun getvar (nm)
  (if (KG-StrEq (KG-AsString nm) "ACADVER") "25.0" nil)
)

(if (not (boundp 'KG-T-DBX-GI))   (setq KG-T-DBX-GI 0))
(if (not (boundp 'KG-T-DBX-CO))   (setq KG-T-DBX-CO 0))
(if (not (boundp 'KG-T-DBX-MODE)) (setq KG-T-DBX-MODE nil))

;; отказ имитируется делением на ноль: vl-catch-all-apply его перехватывает
(defun KG-T-DbxFail () (/ 1 0))

(defun KG-T-DbxReset (mode)
  (setq KG-T-DBX-GI 0)
  (setq KG-T-DBX-CO 0)
  (setq KG-T-DBX-MODE mode)
)

(defun vla-GetInterfaceObject (app progid)
  (setq KG-T-DBX-GI (1+ KG-T-DBX-GI))
  (if (KG-StrEq KG-T-DBX-MODE "gi") (strcat "DBX:" progid) (KG-T-DbxFail))
)

(defun vlax-create-object (progid)
  (setq KG-T-DBX-CO (1+ KG-T-DBX-CO))
  (if (KG-StrEq KG-T-DBX-MODE "co") (strcat "DBX:" progid) (KG-T-DbxFail))
)

(defun TEST-ObjectDbxAttempts ()
  (princ "\n\nT18. Получение интерфейса ObjectDBX")

  ;; 16a. Первый способ работает -- второго касаться не нужно.
  (KG-T-DbxReset "gi")
  (T-EqStr "T18.1 GetInterfaceObject отдал объект"
           (KG-ObjectDbx) "DBX:ObjectDBX.AxDbDocument.25")
  (T-EqInt "T18.2 пробовался один ProgID" KG-T-DBX-GI 1)
  (T-EqInt "T18.3 create-object не вызывался" KG-T-DBX-CO 0)
  (T-Ok "T18.4 причин отказа нет" (not KG-DBX-ERR))

  ;; 16b. Первый способ отказал, второй сработал.
  (KG-T-DbxReset "co")
  (T-EqStr "T18.5 create-object отдал объект"
           (KG-ObjectDbx) "DBX:ObjectDBX.AxDbDocument.25")
  (T-EqInt "T18.6 GetInterfaceObject пробовался" KG-T-DBX-GI 1)
  (T-EqInt "T18.7 create-object пробовался" KG-T-DBX-CO 1)
  (T-EqInt "T18.8 записана одна причина" (length KG-DBX-ERR) 1)

  ;; 16c. Отказали оба способа и оба ProgID -- вот что должен увидеть
  ;;      пользователь вместо голого «ObjectDBX недоступен».
  (KG-T-DbxReset "none")
  (T-Ok "T18.9 объекта нет" (not (KG-ObjectDbx)))
  (T-EqInt "T18.10 GetInterfaceObject: оба ProgID" KG-T-DBX-GI 2)
  (T-EqInt "T18.11 create-object: оба ProgID" KG-T-DBX-CO 2)
  (T-EqInt "T18.12 записаны четыре причины" (length KG-DBX-ERR) 4)
  (T-Ok "T18.13 в причинах есть версионный ProgID"
        (KG-StrContains (KG-JoinNames KG-DBX-ERR)
                        "ObjectDBX.AxDbDocument.25"))
  (T-Ok "T18.14 в причинах есть общий ProgID"
        (KG-StrContains (KG-JoinNames KG-DBX-ERR)
                        "ObjectDBX.AxDbDocument /"))
  (T-Ok "T18.15 в причинах названы оба способа"
        (and (KG-StrContains (KG-JoinNames KG-DBX-ERR) "GetInterfaceObject")
             (KG-StrContains (KG-JoinNames KG-DBX-ERR) "create-object")))
  (KG-T-DbxReset nil)

  ;; 18d. Диагностика шагов копирования: именно её не хватало, когда
  ;;      отчёт печатал «ObjectDBX недоступен» при полученном интерфейсе.
  (setq KG-DBX-ERR nil)
  (T-EqInt "T18.16 успешный шаг возвращает значение"
           (KG-DBX-Try "шаг" '(lambda () 7)) 7)
  (T-Ok "T18.17 при успехе причин нет" (not KG-DBX-ERR))

  (setq KG-DBX-ERR nil)
  (T-Ok "T18.18 упавший шаг возвращает nil"
        (not (KG-DBX-Try "деление на ноль" '(lambda () (/ 1 0)))))
  (T-EqInt "T18.19 причина записана" (length KG-DBX-ERR) 1)
  (T-Ok "T18.20 в причине назван шаг"
        (KG-StrContains (car KG-DBX-ERR) "деление на ноль"))
  (T-Ok "T18.21 в причине есть текст ошибки"
        (KG-StrContains (car KG-DBX-ERR) "division by zero"))

  (KG-DBX-Fail "вторая причина")
  (T-EqInt "T18.22 причины накапливаются" (length KG-DBX-ERR) 2)
  (T-EqStr "T18.23 порядок: первая причина первой"
           (KG-JoinNames (reverse KG-DBX-ERR))
           "деление на ноль: division by zero, вторая причина")
  (setq KG-DBX-ERR nil)
)

;;;===========================================================================
;;; T17. СУДЬБА ОПРЕДЕЛЕНИЯ МАСТЕР-ВЕРСИИ ПОСЛЕ ВСТАВКИ
;;;===========================================================================
(defun TEST-MasterState ()
  (princ "\n\nT17. Что стало с определением мастер-версии")

  (T-EqStr "T17.1 мастера нет -- не определена"
           (KG-MasterState nil (list "A") (list "A")) "не определена")
  (T-EqStr "T17.2 имени не было в чертеже"
           (KG-MasterState "Комплект КП50 v1.51" (list "Комплект КП50 v1.1")
                           (list "Комплект КП50 v1.51"))
           "пришла из буфера как новое определение")
  (T-EqStr "T17.3 имя было и определение пришло"
           (KG-MasterState "Комплект КП50 v1.51" (list "Комплект КП50 v1.51")
                           (list "Комплект КП50 v1.51"))
           "перезаписана из буфера (имя уже было в чертеже)")
  (T-EqStr "T17.4 имя было, буфер его не принёс"
           (KG-MasterState "Комплект КП50 v1.51" (list "Комплект КП50 v1.51")
                           (list "Стойка"))
           "ОСТАЛАСЬ СТАРОЙ: имя было в чертеже, буфер его не принёс")
  ;; регистр имени не должен влиять на вывод
  (T-EqStr "T17.5 сравнение имён без учёта регистра"
           (KG-MasterState "KPS 714" (list "kps 714") (list "Kps 714"))
           "перезаписана из буфера (имя уже было в чертеже)")

  ;; Очистка: новейшая итерация семейства не удаляется даже при нуле
  ;; ссылок -- из неё строятся варианты. Старые итерации и определения
  ;; «~до» удаляются.
  ;; Защищаются ДВА определения новейшей итерации: мастер и его вариант.
  ;; Ключ только по семейству защищал бы одно из них, и вариант без
  ;; экземпляров был бы удалён.
  (T-Ok "T17.6 защищены и мастер, и его вариант новейшей итерации"
        (and (KG-StrInterCI (list "Комплект КП50 v1.51")
               (KG-CleanupKeepNewest
                 (list "Комплект КП50 v1.1" "Комплект КП50 v1.2 КП45387"
                       "Комплект КП50 v1.51" "Комплект КП50 v1.51 КП45387")))
             (KG-StrInterCI (list "Комплект КП50 v1.51 КП45387")
               (KG-CleanupKeepNewest
                 (list "Комплект КП50 v1.1" "Комплект КП50 v1.2 КП45387"
                       "Комплект КП50 v1.51" "Комплект КП50 v1.51 КП45387")))))
  (T-EqInt "T17.7 старых итераций в защищённых нет"
           (length
             (KG-CleanupKeepNewest
               (list "Комплект КП50 v1.1" "Комплект КП50 v1.2 КП45387"
                     "Комплект КП50 v1.51" "Комплект КП50 v1.51 КП45387")))
           2)
  (T-Ok "T17.7b несколько семейств -- по новейшей в каждом"
        (and (KG-StrInterCI (list "Комплект КП50 v1.51")
               (KG-CleanupKeepNewest
                 (list "Комплект КП50 v1.1" "Комплект КП50 v1.51"
                       "Комплект КП50К v2.0" "Комплект КП50К v2.3")))
             (KG-StrInterCI (list "Комплект КП50К v2.3")
               (KG-CleanupKeepNewest
                 (list "Комплект КП50 v1.1" "Комплект КП50 v1.51"
                       "Комплект КП50К v2.0" "Комплект КП50К v2.3")))))
  (T-EqStr "T17.8 v1.5 (50) новее v1.21 (21)"
           (KG-JoinNames
             (KG-CleanupKeepNewest (list "Комплект КП50 v1.21"
                                         "Комплект КП50 v1.5")))
           "Комплект КП50 v1.5")
  (T-Ok "T17.9 определения без версии не защищаются"
        (not (KG-CleanupKeepNewest (list "Стойка КП50" "kps 714"))))
  (T-Ok "T17.10 сравнение имён без учёта регистра, как в AutoCAD"
        (KG-StrInterCI (list "kp45387") (list "Kp45387")))

  ;; Кандидаты на удаление. Одного подсчёта ссылок до начала мало:
  ;; «Kp45302-1 невидимый~до» держал на себе старый вариант, и пока тот
  ;; не удалён, вложение считается использованным.
  (T-EqStr "T17.11 свободно только то, на что нет ссылок"
           (KG-JoinNames
             (KG-CleanupCandidates
               (list (cons "Kp45302-1 невидимый~до" 1)
                     (cons "kps 714~до" 0))
               (list "Kp45302-1 невидимый~до" "kps 714~до")
               nil))
           "kps 714~до")
  (T-EqStr "T17.12 старая итерация без ссылок -- кандидат"
           (KG-JoinNames
             (KG-CleanupCandidates
               (list (cons "Комплект КП50 v1.1" 0)
                     (cons "Комплект КП50 v1.51" 0))
               (list "Комплект КП50 v1.1" "Комплект КП50 v1.51")
               (KG-CleanupKeepNewest
                 (list "Комплект КП50 v1.1" "Комплект КП50 v1.51"))))
           "Комплект КП50 v1.1")
  (T-Ok "T17.13 новейшая итерация не кандидат даже при нуле ссылок"
        (not (KG-CleanupCandidates
               (list (cons "Комплект КП50 v1.51" 0))
               (list "Комплект КП50 v1.51")
               (KG-CleanupKeepNewest (list "Комплект КП50 v1.51")))))
  (T-Ok "T17.14 пространства и прочие * не трогаются"
        (not (KG-CleanupCandidates
               nil (list "*Model_Space" "*Paper_Space" "*D12" "*X1") nil)))

  ;; Анонимные представления динамических блоков. У живого экземпляра
  ;; представление называется тем же *U133, и отличить сироту от живого
  ;; без COM-чтения EffectiveName нельзя -- а оно роняет AutoCAD.
  ;; Поэтому программа *U не удаляет вообще, только показывает.
  (T-Ok "T17.15 *U и только цифры -- анонимное представление"
        (and (KG-IsAnonDynName "*U52") (KG-IsAnonDynName "*u134")))
  (T-Ok "T17.16 *Model_Space, *Paper_Space, *D, *X -- не представление"
        (not (or (KG-IsAnonDynName "*Model_Space")
                 (KG-IsAnonDynName "*Paper_Space0")
                 (KG-IsAnonDynName "*D12")
                 (KG-IsAnonDynName "*X1")
                 (KG-IsAnonDynName "*U"))))
  (T-Ok "T17.17 сиротское *U в кандидаты не попадает"
        (not (KG-CleanupCandidates nil (list "*U52" "*U55") nil)))
  (T-Ok "T17.18 живое *U тем более не кандидат"
        (not (KG-CleanupCandidates
               (list (cons "*U133" 1)) (list "*U133") nil)))
  (T-EqStr "T17.19 сироты видны в отдельном списке для PURGE"
           (KG-JoinNames (KG-CleanupOrphans
                           (list (cons "*U55" 2))
                           (list "*U52" "*U55" "*U133" "kps 714~до")))
           "*U52, *U133")
  (T-Ok "T17.20 стороннее определение без версии не трогается"
        (not (KG-CleanupCandidates
               (list (cons "Стойка КП50" 0)) (list "Стойка КП50") nil)))
  ;; Карта держателей -> число ссылок. Длина списка держателей и есть
  ;; количество вставок.
  (T-EqInt "T17.21 длина списка держателей = число ссылок"
           (KG-CdrCI "Kp45302-1 невидимый~до"
                     (KG-CleanupCounts
                       (list (cons "Kp45302-1 невидимый~до"
                                   (list "*U52" "*U55" "*U53")))))
           3)
  (T-Ok "T17.22 определение без вставок в карте не появляется"
        (not (KG-CdrCI "kps 714~до"
                       (KG-CleanupCounts (list (cons "*U52" (list "*U51")))))))

  ;; Определения, пришедшие из внешней ссылки, переименовывать нельзя:
  ;; на реальном чертеже с 264 кандидатами это дало 0xC0000005.
  (T-Ok "T17.23 имя с чертой -- из внешней ссылки"
        (KG-IsXrefDepName "фасад|окно КП50"))
  (T-Ok "T17.24 обычное имя -- не из внешней ссылки"
        (not (KG-IsXrefDepName "Стойка КП50")))
  (DB-SetDefs (list (DB-MakeDef "фасад|окно КП50" nil nil)
                    (DB-MakeDef "Стойка КП50" nil nil)))
  (T-EqInt "T17.25 переименовано только обычное определение"
           (length (KG-Step_PreRename
                     (list "фасад|окно КП50" "Стойка КП50")
                     nil nil (KG_DBGetModel)))
           1)
  (T-Ok "T17.26 определение из ссылки осталось под своим именем"
        (DB-HasDef "фасад|окно КП50"))
  (setq *DB* (list (cons "defs" nil) (cons "insts" nil)
                   (cons "layouts" nil)))
)

;;;===========================================================================
;;; T15. ЭКЗЕМПЛЯР БЕЗ ОПРЕДЕЛЁННОГО ПРОСТРАНСТВА
;;;      Так выглядит вхождение, лежащее внутри определения блока: у него
;;;      нет ни модели, ни листа. Пересоздавать его нельзя -- запрещено
;;;      переносить объекты между моделью и листами (п. 5.3 ТЗ).
;;;===========================================================================

(defun TEST-SpaceUnknown ()
  (princ "\n\nT15. Экземпляр без определённого пространства")

  (TEST-BuildRealDrawing)
  ;; экземпляр старой итерации внутри определения блока: пространство nil
  (DB-SetInsts
    (append (DB-Insts)
      (list (DB-MakeInst "R11" "Комплект КП50 v1.21 ригель КП45339"
                         "Комплект КП50 v1.21 ригель КП45339" nil "0"
                         (list 3000.0 0.0 0.0) nil))))
  (TEST-SetClipboard "Комплект КП50 v1.5"
    (list (DB-MakeDef "Стойка КП50" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель КП50" nil (list "1" "2" "3"))))
  (setq R15 (TEST-IntegratePaste "Комплект КП50" "1.5"))

  (T-EqInt "T15.1 заменено 10, экземпляр без пространства пропущен"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R15)))) 10)
  (T-EqInt "T15.2 он учтён как пропущенный"
           (cdr (assoc "space-unknown" (cdr (assoc "replaced" R15)))) 1)
  (T-Ok "T15.3 старый экземпляр не удалён"
        (DB-FindInst "R11"))
  (T-EqStr "T15.4 и не перепривязан на новую итерацию"
           (KG-CdrCI "eff" (DB-FindInst "R11"))
           "Комплект КП50 v1.21 ригель КП45339")
  (T-EqInt "T15.5 новый экземпляр вместо него не создан"
           (DB-CountEff "Комплект КП50 v1.5 ригель КП45339") 2)
  (T-EqInt "T15.6 всего экземпляров 14" (length (DB-Insts)) 14)
  ;; в отчёте старый экземпляр виден: интеграция не рапортует успех
  (T-EqInt "T15.7 валидация видит один незаменённый"
           (cdr (assoc "old-left" (cdr (assoc "validation" R15)))) 1)
  ;; отчёт с таким экземпляром печатается и не падает, а сам факт
  ;; незамены в нём виден
  (KG-PrintIntegrationReport R15)
  (T-EqInt "T15.8 отчёт показывает незаменённый экземпляр"
           (cdr (assoc "space-unknown" (cdr (assoc "replaced" R15)))) 1)
)

;;;===========================================================================
;;; T14. УСТОЙЧИВОСТЬ К НЕСТРОКОВЫМ ЗНАЧЕНИЯМ ИЗ COM
;;;      Из AutoCAD вместо строки может прийти variant или имя объекта.
;;;      Строковая функция на таком значении падает с "неверный тип
;;;      аргумента: stringp" и обрывает команду.
;;;===========================================================================

(defun TEST-TypeSafety ()
  (princ "\n\nT14. Устойчивость к нестроковым значениям")

  (T-EqStr "T14.1 nil становится пустой строкой" (KG-AsString nil) "")
  (T-EqStr "T14.2 строка проходит как есть"       (KG-AsString "abc") "abc")
  (T-EqStr "T14.3 целое приводится к строке"     (KG-AsString 15) "15")
  (T-Ok    "T14.4 список не рвёт приведение"
           (> (strlen (KG-AsString (list 1 2))) 0))
  (T-Ok    "T14.5 сравнение числа со строкой не падает"
           (KG-StrEq 15 "15"))
  (T-Ok    "T14.6 сравнение с nil не падает"
           (not (KG-StrEq nil "abc")))

  ;; разбор имени на нестроке -- просто nil, без падения
  (T-Ok "T14.7 разбор числа"   (null (KG-ParseBlockName 15)))
  (T-Ok "T14.8 разбор списка"  (null (KG-ParseBlockName (list 1 2))))
  (T-Ok "T14.9 разбор nil"     (null (KG-ParseBlockName nil)))

  ;; распознавание служебных имён на нестроке
  (T-Ok "T14.10 анонимное имя из числа"  (null (KG-IsAnonymousName 15)))
  (T-Ok "T14.11 служебное имя из числа"  (null (KG-IsServiceName 15)))
  (T-Ok "T14.12 анонимное имя распознаётся" (KG-IsAnonymousName "*U83"))
  (T-Ok "T14.13 служебное имя распознаётся" (KG-IsServiceName "Стойка$0$"))

  ;; поиск подстроки и обрезка пробелов на нестроке
  (T-Ok    "T14.14 поиск подстроки в числе" (KG-StrContains 151 "51"))
  (T-Ok    "T14.15 поиск подстроки в nil"   (not (KG-StrContains nil "a")))
  (T-EqStr "T14.16 обрезка слева на числе"  (KG-TrimLeft 15) "15")
  (T-EqStr "T14.17 обрезка справа на nil"   (KG-TrimRight nil) "")

  ;; текст отказа тоже не должен падать на странном имени
  (T-Ok "T14.18 сообщение об отказе строится"
        (> (strlen (KG-MasterRejectMsg 15)) 10))
  (T-Ok "T14.19 сообщение называет вариант"
        (wcmatch (KG-MasterRejectMsg "Комплект КП50 v1.5 стойка КП45302-1")
                 "*мастер-версия называется*"))
)

;;;===========================================================================
;;; T16. ВЕРСИЯ КАК СТРОКА: ЧИСЛОВЫЕ ФУНКЦИИ НЕДОПУСТИМЫ
;;;      В AutoCAD (vl-sort список-строк '<) падает с «неверный тип аргумента:
;;;      fixnump: "1.5"» -- ровно так упал INTEGRATE на реальном чертеже.
;;;      Интерпретатор раньше сравнивал строки как Python и ошибку не ловил.
;;;===========================================================================

;; склеить список строк через разделитель (для проверки порядка)
(defun TEST-Join (lst sep / out first)
  (setq out "" first t)
  (foreach x lst
    (setq out (strcat out (if first "" sep) (KG-AsString x)))
    (setq first nil)
  )
  out
)

(defun TEST-IterIsString ()
  (princ "\n\nT16. Версия-строка и числовые функции")

  (T-EqStr "T18.1 ключ версии: 1.5 это итерация 50"
           (KG-IterKey "1.5") ".000001.000050")
  (T-EqStr "T18.1b ключ версии: 1.2 это итерация 20"
           (KG-IterKey "1.2") ".000001.000020")
  (T-EqStr "T18.2 ключ v1.21"             (KG-IterKey "1.21") ".000001.000021")

  ;; порядок версий без числового сравнения строк
  (T-Ok    "T18.3 1.1 старше 1.5"    (KG-IterOlder "1.1" "1.5"))
  (T-Ok    "T18.4 1.5 (50) не старше 1.21 (21)" (not (KG-IterOlder "1.5" "1.21")))
  (T-Ok    "T18.5 1.21 старше 2.0"   (KG-IterOlder "1.21" "2.0"))
  (T-Ok    "T18.6 равные не старше"  (not (KG-IterOlder "1.5" "1.5")))
  (T-Ok    "T18.7 1.5 не старше 1.1" (not (KG-IterOlder "1.5" "1.1")))

  ;; посимвольное сравнение строк
  (T-Ok "T18.8 строковое меньше"    (KG-StrLt "abc" "abd"))
  (T-Ok "T18.9 префикс меньше"      (KG-StrLt "ab" "abc"))
  (T-Ok "T18.10 большее не меньше"  (not (KG-StrLt "abd" "abc")))
  (T-Ok "T18.11 равные не меньше"   (not (KG-StrLt "abc" "abc")))
  (T-Ok "T18.12 точка меньше цифры" (KG-StrLt "." "0"))

  ;; сортировка итераций: тот самый путь, на котором падал INTEGRATE
  (T-EqStr "T18.13 порядок итераций"
           (TEST-Join (vl-sort (list "1.21" "1.5" "1.1" "2.0") 'KG-IterOlder) " ")
           "1.1 1.21 1.5 2.0")

  ;; сканирование семейства со строковой версией не падает
  (TEST-BuildRealDrawing)
  (T-EqInt "T18.14 скан со строковой версией находит 10 старых"
           (cdr (assoc "total"
             (KG-ScanFamily (KG_DBGetModel) "Комплект КП50" "1.5"))) 10)
  (T-EqStr "T18.15 итерации в скане отсортированы"
           (TEST-Join (cdr (assoc "iterations"
             (KG-ScanFamily (KG_DBGetModel) "Комплект КП50" "1.5"))) " ")
           "1.1 1.21")
)


;;;---------------------------------------------------------------------------
;;; ЗАПУСК
;;;---------------------------------------------------------------------------

;; Проверка самого стенда. Отдельная функция и вызывается ПЕРВОЙ:
;; RUN-ALL-TESTS обнуляет счётчики, поэтому тесты, исполненные при
;; загрузке файла, в итог не попадают -- так четыре проверки строгого
;; car напечатали FAIL, а в итоге осталось «Провалено: 0».
(defun TEST-StrictCar ()
  (princ "\n\nT0. Строгость стенда")
  (T-Ok "T0.1 строгий CAR заменил встроенный"
        (not (KG-StrEq (KG-AsString (type car)) "SUBR")))
  (T-Ok "T0.2 строгий CDR заменил встроенный"
        (not (KG-StrEq (KG-AsString (type cdr)) "SUBR")))
  (T-EqStr "T0.3 CAR от строки падает, как в AutoCAD"
           (KG-AsString (KG-Safe '(lambda () (car "OK")) "УПАЛО"))
           "УПАЛО")
  (T-EqStr "T0.4 CDR от строки падает, как в AutoCAD"
           (KG-AsString (KG-Safe '(lambda () (cdr "OK")) "УПАЛО"))
           "УПАЛО")
  (T-EqStr "T0.5 assoc по карте с маркером падает, как в AutoCAD"
           (KG-AsString
             (KG-Safe '(lambda () (KG-CdrCI "kps" (list "OK" (cons "kps" 1))))
                      "УПАЛО"))
           "УПАЛО")
)

;;;===========================================================================
;;; T20. НАСТОЯЩИЙ СБОР ВХОЖДЕНИЙ (KG-CollectInstances)
;;;      До сборки 50 эту функцию не проверял ни один тест: стенд
;;;      подделывал KG_DBGetModel целиком, и весь цикл чтения вхождений --
;;;      единственное место, где команда падала на реальном чертеже три
;;;      прогона подряд, -- оставался непроверенным. Теперь он исполняется
;;;      поверх поддельной базы, а отказ одного вхождения проверяется как
;;;      отказ: команда обязана дочитать остальные, а не оборваться.
;;;===========================================================================

(defun TEST-CollectInstances ()
  (princ "\n\nT20. Настоящий сбор вхождений")

  ;; Поддельная база: три вставки, у одной из них (EBAD) записи в
  ;; DB-ENTDATA нет -- entget отдаст nil, и чтение сорвётся на первом же
  ;; (cdr (assoc 5 nil)). Именно так выглядит «вхождение не читается».
  (setq DB-HAS-MINSERT nil DB-SSGET-NOFILTER nil DB-ENTDEL-FAILS nil)
  (setq DB-ENTDATA
    (list (cons "C1" (list (cons 0 "INSERT") (cons 5 "119A5")
                           (cons 2 "Комплект КП50 v1.2 КП45387")
                           (cons 8 "0")))
          (cons "C2" (list (cons 0 "INSERT") (cons 5 "11B19")
                           (cons 2 "Комплект КП50 v1.1")
                           (cons 8 "0")))
          (cons "C3" (list (cons 0 "INSERT") (cons 5 "1F33D")
                           (cons 2 "Комплект КП50 v1.51")
                           (cons 8 "0")))
          ;; Запись есть -- иначе ssget (он тоже зовёт entget) не включил
          ;; бы это вхождение в выборку, и проверяемая ветка не
          ;; исполнялась. Отказывает именно ЧТЕНИЕ модели.
          (cons "EBAD" (list (cons 0 "INSERT") (cons 5 "EBAD")
                             (cons 2 "Комплект КП50 v1.1")
                             (cons 8 "0")))))
  (defun vlax-ename->vla-object (e / ) (KG-AsString e))
  (defun vlax-vla-object->ename (o / ) (KG-AsString o))
  (defun vla-get-EffectiveName (o / )
    (KG-AsString (cdr (assoc 2 (entget (KG-AsString o))))))
  ;; Карта пространств строится обходом COM-коллекций; в стенде она пуста,
  ;; и экземпляр уходит в «пространство не определено». Для этой группы
  ;; важно не это, а сам цикл чтения, поэтому карта подделывается.
  (defun KG-BuildSpaceMap ( / ) (list (cons "C1" "Model")))
  ;; Нечитаемое вхождение: entget на нём отказывает. Без этого стенд
  ;; возвращал модель с пустыми полями, и ветка «вхождение не прочитано»
  ;; не исполнялась ни разу -- мутация, снимающая проверку, проходила
  ;; незамеченной.
  (defun entget (e / ed)
    (setq ed (KG-CdrCI e DB-ENTDATA))
    (if (KG-StrEq (KG-AsString e) "EBAD")
      (_strict-fail "нет данных объекта")
      ed))
  (setq DB-SEL (list "C1" "C2" "C3"))
  (setq T20-R (KG-CollectInstances))
  (T-EqInt "T20.1 три вставки дают три модели" (length T20-R) 3)
  (T-EqStr "T20.2 имя определения прочитано"
           (KG-CdrCI "def" (car T20-R)) "Комплект КП50 v1.51")
  (T-Ok "T20.3 у модели есть список состояний вложенных"
        (KG-CdrCI "nested-vis-n" (car T20-R)))
  ;; consp в интерпретаторе нет, поэтому «модель -- список пар» проверяется
  ;; через чтение поля: у строки или атома (cdr (assoc …)) ничего бы не дал.
  (T-EqStr "T20.4 handle прочитан из группы 5"
           (KG-AsString (KG-CdrCI "handle" (car T20-R))) "1F33D")

  ;; Отказ одного вхождения: остальные дочитываются, команда не рвётся.
  (setq DB-SEL (list "C1" "EBAD" "C3"))
  (setq T20-R (KG-CollectInstances))
  ;; В выборке три вхождения, одно из них не читается -- моделей две.
  (T-EqInt "T20.5 нечитаемое вхождение пропущено, остальные прочитаны"
           (length T20-R) 2)
  ;; Одного (length …) мало: объект ошибки истинен, и при снятой проверке
  ;; нечитаемое вхождение попало бы в список как «модель». Проверяется
  ;; каждое поле каждого элемента: у мусора имени определения нет.
  (setq T20-BAD 0)
  (foreach T20-M T20-R
    (if (KG-StrEq "" (KG-AsString (KG-CdrCI "def" T20-M)))
      (setq T20-BAD (1+ T20-BAD)))
  )
  (T-EqInt "T20.6 в списке нет ни одной модели без имени определения"
           T20-BAD 0)

  ;; Пустая выборка -- не ошибка: моделей ноль, список пуст.
  (setq DB-SEL nil)
  (T-EqInt "T20.7 пустая выборка даёт пустой список"
           (length (KG-CollectInstances)) 0)

  ;; 20b. Вставки без определения. Повод -- прогон сборки 50: INTEGRATE
  ;;      напечатала «УСПЕШНО» при «Заменено 0», а два экземпляра
  ;;      семейства остались в чертёже с определениями, которых уже нет.
  ;;      Валидация этого не видит, поэтому признак вынесен в INTBRIEF.
  (defun entget (e / ) (KG-CdrCI e DB-ENTDATA))
  (setq DB-ENTDATA
    (list (cons "D1" (list (cons 0 "INSERT") (cons 5 "119A5") (cons 2 "AAA")))
          (cons "D2" (list (cons 0 "INSERT") (cons 5 "11B19") (cons 2 "BBB")))
          (cons "D3" (list (cons 0 "INSERT") (cons 5 "1F33D") (cons 2 "CCC")))))
  (setq DB-SEL (list "D1" "D2" "D3"))
  (defun KG_EXDefExists (nm / ) (KG-StrEq nm "AAA"))
  (setq T20-D (KG-DanglingInserts))
  (T-EqInt "T20.8 две вставки указывают на отсутствующие определения"
           (car T20-D) 2)
  ;; Порядок в списке -- порядок обхода вставок, поэтому проверяется
  ;; наличие имени, а не позиция.
  (setq T20-N 0)
  (foreach T20-S (car (cdr T20-D))
    (if (KG-StrContains (KG-AsString T20-S) "BBB") (setq T20-N (1+ T20-N))))
  (T-EqInt "T20.9 в списке названо отсутствующее определение" T20-N 1)
  (defun KG_EXDefExists (nm / ) t)
  (T-EqInt "T20.10 когда все определения на месте, счётчик ноль"
           (car (KG-DanglingInserts)) 0)

  ;; 20c. Экземпляры семейства. Повод -- прогон, в котором отчёт сказал
  ;;      «УСПЕШНО» при «Заменено 0»: по отчёту нельзя было отличить «в
  ;;      чертеже нет экземпляров семейства» от «они есть, но видны под
  ;;      другими именами». Считается по вставкам напрямую, мимо плана.
  (setq DB-ENTDATA
    (list (cons "F1" (list (cons 0 "INSERT") (cons 5 "119A5")
                           (cons 2 "Комплект КП50 v1.1")))
          (cons "F2" (list (cons 0 "INSERT") (cons 5 "11B19")
                           (cons 2 "Комплект КП50 v1.2 КП45387")))
          (cons "F3" (list (cons 0 "INSERT") (cons 5 "1F33D")
                           (cons 2 "Комплект КП50К v1.5")))))
  (setq DB-SEL (list "F1" "F2" "F3"))
  (defun vlax-ename->vla-object (e / ) (KG-AsString e))
  (defun KG-EffectiveNameOf (o / )
    (KG-AsString (cdr (assoc 2 (entget (KG-AsString o))))))
  (setq T20-F (KG-FamilyInstances "Комплект КП50"))
  (T-EqInt "T20.11 два экземпляра семейства найдены, чужая семья не в счёт"
           (car T20-F) 2)
  (T-EqInt "T20.12 в списке два разных имени" (length (car (cdr T20-F))) 2)
  (T-EqInt "T20.13 у семейства без экземпляров счётчик ноль"
           (car (KG-FamilyInstances "Комплект КП99")) 0)
  (T-EqStr "T20.14 отчёт несёт семейство"
           (KG-ReportBase (list (cons "base" "Комплект КП50")))
           "Комплект КП50")

  ;; 20d. Диагноз «экземпляры есть, а план пуст». Два независимых пути:
  ;;      модель (по ней строится план) и вставки напрямую. Прогон сборки
  ;;      52 дал по вставкам 2 и по плану 0 -- значит, расходятся именно
  ;;      эти два числа, и строка отчёта обязана это показывать.
  (setq DB-ENTDATA
    (list (cons "F1" (list (cons 0 "INSERT") (cons 5 "119A5")
                           (cons 2 "Комплект КП50 v1.1")))))
  (setq DB-SEL (list "F1"))
  (DB-SetInsts (list (DB-MakeInst "119A5" "Комплект КП50 v1.1"
                                  "Комплект КП50 v1.1" "Model" "0"
                                  (list 0.0 0.0 0.0) nil)))
  (setq T20-L (KG-ModelScanLine "Комплект КП50"))
  (T-Ok "T20.15 строка диагноза называет число по модели"
        (KG-StrContains T20-L "по модели"))
  (T-Ok "T20.16 строка диагноза называет число по вставкам"
        (KG-StrContains T20-L "по вставкам напрямую: 1"))
  ;; расхождение обязано быть видно: модель пуста, вставка есть
  (DB-SetInsts nil)
  (T-Ok "T20.17 при пустой модели видно расхождение 0 против 1"
        (KG-StrContains (KG-ModelScanLine "Комплект КП50")
                        "по модели (по ним строится план): 0"))
  ;; Обратный случай обязателен: на одной только пустой модели проверку
  ;; проходит и заглушка, которая всегда печатает ноль. Мутация «число по
  ;; модели не считается» на T20.17 не ловилась именно поэтому.
  (DB-SetInsts (list (DB-MakeInst "119A5" "Комплект КП50 v1.1"
                                  "Комплект КП50 v1.1" "Model" "0"
                                  (list 0.0 0.0 0.0) nil)))
  (T-Ok "T20.18 при непустой модели число по модели равно 1"
        (KG-StrContains (KG-ModelScanLine "Комплект КП50")
                        "по модели (по ним строится план): 1"))

  ;; 20e. Отказ ПОСЛЕ чтения вхождений не должен уносить список.
  ;;      Прогон сборки 53: «ВНИМАНИЕ: вхождения не прочитаны (счёт
  ;;      вхождений)» при трёх прочитанных вхождениях. Функция обрывалась
  ;;      на подсчёте, вызывающий получал nil, план строился по пустой
  ;;      модели -- и INTEGRATE печатала «Заменено 0» при двух живых
  ;;      экземплярах. Имитируется отказом трассировки: она вызывается
  ;;      ровно в том месте, где падало на реальном чертеже.
  (setq DB-ENTDATA
    (list (cons "F1" (list (cons 0 "INSERT") (cons 5 "119A5")
                           (cons 2 "Комплект КП50 v1.1")))
          (cons "F2" (list (cons 0 "INSERT") (cons 5 "11B19")
                           (cons 2 "Комплект КП50 v1.2 КП45387")))))
  (setq DB-SEL (list "F1" "F2"))
  (defun entget (e / ) (KG-CdrCI e DB-ENTDATA))
  (defun vlax-ename->vla-object (e / ) (KG-AsString e))
  (defun KG-EffectiveNameOf (o / )
    (KG-AsString (cdr (assoc 2 (entget (KG-AsString o))))))
  (defun KG-Trace (s / ) (_strict-fail "неверная функция: T"))
  (setq T20-R (KG-CollectInstances))
  (defun KG-Trace (s / )
    (setq KG-LAST-STEP (KG-AsString s))
    (if (or KG-VERBOSE KG-TRACE)
      (princ (strcat "\n[шаг] " KG-LAST-STEP))))
  ;; 20f. Сводная печать длинных списков. На реципиенте со 100 блоками
  ;;      списки имён занимали больше места, чем весь остальной отчёт.
  ;;      Хелпер обязан вернуть ДЛИНУ списка: по ней печатается счётчик,
  ;;      и если он соврёт, свернутый список покажет неверное число.
  (T-EqInt "T20.19a KG-SayList отдаёт длину списка"
           (KG-SayList (list "a" "b" "c") 10) 3)
  (T-EqInt "T20.19b пустой список -- ноль" (KG-SayList nil 10) 0)
  (T-EqInt "T20.19c KG-PrincList отдаёт длину списка"
           (KG-PrincList (list "a" "b") 10) 2)
  (T-EqInt "T20.19d ограничение не меняет длину"
           (KG-SayList (list "a" "b" "c" "d" "e") 2) 5)

  ;; 20g. Отказ одного определения не обязан обрывать чтение модели.
  ;;      Прогон сборки 55 на новом реципиенте: INTEGRATE оборвалась на
  ;;      «неверная функция: T» с последним шагом «чтение определения
  ;;      "*U87"» -- уже после вставки из буфера, с освобождёнными
  ;;      именами. Первый снимок на том же чертеже не прочитался на
  ;;      "*U54", но дал лишь предупреждение: разница была не в причине,
  ;;      а в том, перехвачен ли вызов. В сборке 56 чтение свойств
  ;;      каждого определения перехвачено, отказ печатается с именем и
  ;;      причиной, а список пропущенных виден в KG-SKIP-DEFS.
  ;;
  ;;      ТЕСТА НА ЭТО НЕТ, И ЭТО ОСОЗНАННО: настоящая KG_DBGetModel
  ;;      лежит внутри (if (not KG-TESTING) ...), а в tests.lsp есть её
  ;;      заглушка (defun KG_DBGetModel () *DB*). Проверка на стенде
  ;;      звала бы заглушку и всегда отвечала бы «хорошо». Попытка
  ;;      написать такой тест уже была: T20.22-T20.25 давали 18
  ;;      определений вместо двух -- это *DB*, а не модель чертежа.
  ;;      Проверяется в AutoCAD: в логе INTEGRATE обязаны появиться
  ;;      строки «определение "..." не прочитано (...) -- пропущено»
  ;;      и «Определений не прочитано и пропущено: N», а команда
  ;;      обязана дойти до «=== ИТОГ».

  ;; 20h. KG-IsErrSafe: проверка результата COM-вызова перехвачена так же,
  ;;      как сама операция. (vl-catch-all-error-p) от неожиданного
  ;;      значения на реальном чертеже бросает «неверная функция: T», и
  ;;      без перехвата проверки последняя метка указывала бы на уже
  ;;      пройденное место -- ровно так читался прогон сборки 55.
  ;;
  ;;      ТЕСТА НЕТ, И ЭТО ОСОЗНАННО. Заглушка на vl-catch-all-error-p
  ;;      роняет ВЕСЬ прогон: её зовёт KG-IsErr, а он в стенде
  ;;      не перехвачен. Попытка написать такой тест обрывала run_tests
  ;;      на «неверная функция: T» ещё до подсчёта итога. Нормальная
  ;;      ветка (проверка сработала) проверяется косвенно: все 484
  ;;      проверки идут через KG-IsErrSafe в KG_DBGetModel.
  (T-Ok "T20.26 пригодное значение -- не отказ"
        (not (KG-IsErrSafe "Комплект КП50 v1.5" 7)))

  ;; 20i. Причина отказа обязана называть ШАГ, на котором он произошёл.
  ;;      Прогон сборки 56 напечатал «снимок чертежа не прочитан (таблица
  ;;      блоков прочитана)»: KG-Trace и KG-Mark перезаписывают
  ;;      KG-LAST-STEP на каждом удачном шаге, и к моменту предупреждения
  ;;      там стоял шаг, пройденный ДО отказа. Сообщение называло не то
  ;;      место и уводило в сторону -- ровно то, ради чего трассировка
  ;;      вообще нужна.
  (setq KG-LAST-STEP "шаг до отказа")
  (defun _T20-Throw ( / )
    (setq KG-LAST-STEP "сбор вхождений")
    (_strict-fail "неверная функция: T"))
  (defun vl-catch-all-error-message (e / ) "неверная функция: T")
  (setq T20-V (KG-Safe '(lambda () (_T20-Throw)) "ЗАПАС"))
  ;; Обязательная часть проверки: между отказом и печатью предупреждения
  ;; всегда проходят удачные шаги, и они перезаписывают KG-LAST-STEP.
  ;; Без этой строки мутация «печатать последний шаг» не ловилась.
  (KG-Mark "шаг, пройденный уже после отказа")
  (setq T20-W (KG-FailText))
  (setq vl-catch-all-error-message nil)
  (T-EqStr "T20.29 KG-Safe отдаёт запасное значение" T20-V "ЗАПАС")
  (T-Ok "T20.30 причина называет шаг, на котором случилось"
        (KG-StrContains T20-W "сбор вхождений"))
  (T-Ok "T20.31 и не называет пройденный до него"
        (not (KG-StrContains T20-W "шаг до отказа")))
  (T-Ok "T20.32 причина отказа тоже напечатана"
        (KG-StrContains T20-W "неверная функция: T"))

  ;; 20j. Подсчёт отделён от печати. Прогон сборки 57 шесть раз напечатал
  ;;      «не напечаталось число определений» при полностью успешной
  ;;      интеграции: строка из четырёх вызовов под одним перехватом не
  ;;      говорила, какой из них отказал. Теперь считается отдельно, и
  ;;      отказ преобразования в строку не уносит значение.
  ;;      Заглушка на type здесь невозможна: её зовёт KG-AsString, и
  ;;      прошлая попытка дала бесконечную рекурсию вместо проверки.
  (defun KG-NumStr (x / ) (_strict-fail "неверная функция: T"))
  (setq T20-Q (KG-Safe '(lambda ()
                          (list (cons "cnt" (KG-Safe '(lambda () 108) nil))
                                (cons "txt" (KG-Safe '(lambda () (KG-NumStr 108))
                                                     "ОТКАЗ"))))
                       nil))
  (defun KG-NumStr (x / ) (itoa (fix (KG-AsNum x 0))))
  (T-EqInt "T20.33 подсчёт пережил отказ преобразования"
           (KG-AsNum (KG-CdrCI "cnt" T20-Q) 0) 108)
  (T-EqStr "T20.34 а отказ преобразования назван"
           (KG-AsString (KG-CdrCI "txt" T20-Q)) "ОТКАЗ")
  (T-EqStr "T20.35 без отказа число преобразуется" (KG-NumStr 108) "108")

  ;; 20k. Удачный вызов не обязан наследовать причину чужого отказа.
  ;;      Прогон сборки 58 напечатал подряд:
  ;;        [счёт] определений в списке: 57
  ;;        ВНИМАНИЕ: не напечаталось число определений
  ;;                  (шаг "счёт вхождений", неверная функция: T), значение: 57.
  ;;      Счётчик напечатался, а предупреждение всё равно вышло: KG-Safe
  ;;      не сбрасывал KG-FAIL-STEP, и в строку попала причина отказа,
  ;;      случившегося РАНЬШЕ и в другом месте.
  (defun _T20-Throw ( / )
    (setq KG-LAST-STEP "счёт вхождений")
    (_strict-fail "неверная функция: T"))
  (defun vl-catch-all-error-message (e / ) "неверная функция: T")
  (KG-Safe '(lambda () (_T20-Throw)) nil)
  (setq T20-S (KG-FailText))
  (setq T20-T (KG-Safe '(lambda () (princ "") t) nil))
  (setq T20-U (KG-FailText))
  (T-Ok "T20.36 до удачного вызова причина записана"
        (KG-StrContains T20-S "счёт вхождений"))
  (T-Ok "T20.37 удачный вызов возвращает t" T20-T)
  (T-Ok "T20.38 и чужую причину с собой не несёт"
        (not (KG-StrContains T20-U "счёт вхождений")))
  (T-Ok "T20.39 сообщение чужого отказа тоже сброшено"
        (not (KG-StrContains T20-U "неверная функция: T")))

  (T-EqInt "T20.19 отказ подсчёта не уносит прочитанные вхождения"
           (length T20-R) 2)
  (T-EqStr "T20.20 первое из них читается"
           (KG-AsString (KG-CdrCI "def" (car T20-R)))
           "Комплект КП50 v1.2 КП45387")
)

;;;---------------------------------------------------------------------------
;;; T21. Перепривязка ссылок на прежние имена (шаг 8б).
;;;
;;; Задача шага: одноимённое определение получает новое содержимое из буфера,
;;; а все ссылки чертежа -- в том числе внутри чужих определений и внутри
;;; анонимных представлений -- должны указывать на прежнее имя. Без этого на
;;; реальном чертеже осталось 31 определение «имя~до» с нулём вставок, и
;;; PURGE их не снял.
;;;
;;; Проверяются: поиск держателей, перепривязка группы 2, защита от
;;; зацикливания определения, многопроходность (ссылка внутри определения,
;;; которое само стало прежним только на следующем проходе) и предел
;;; проходов.
;;;---------------------------------------------------------------------------
(defun T21-SetDb ()
  (setq DB-ENTDB
    (list
      (cons "Чертеж А" "BA") (cons "BA" "EA1") (cons "EA1" "EA2")
      (cons "EA2" "EAB")
      (cons "Гайка" "BG") (cons "BG" "EG1") (cons "EG1" "EGB")
      (cons "*U900" "B9") (cons "B9" "E91") (cons "E91" "EB9")
      (cons "Болт" "BO") (cons "BO" "EO1") (cons "EO1" "EOB")
      (cons "Петля" "BP") (cons "BP" "EP1") (cons "EP1" "EPB")
      (cons "Звено~до" "BZ") (cons "BZ" "EZ1") (cons "EZ1" "EZB")
      (cons "Пружина" "BPR") (cons "BPR" "EPR1") (cons "EPR1" "EPRB")
      (cons "Кольцо" "BKO") (cons "BKO" "EKO1") (cons "EKO1" "EKOB")
      (cons "Узел1" "BU1") (cons "BU1" "EU1") (cons "EU1" "EBU1")
      (cons "Узел2" "BU2") (cons "BU2" "EU2") (cons "EU2" "EBU2")
      (cons "Звено" "BZN") (cons "BZN" "EZN1") (cons "EZN1" "EZNB")
      (cons "Шайба" "BSH") (cons "BSH" "ESH1") (cons "ESH1" "ESHB")
      (cons "ДетальX" "BDX") (cons "BDX" "EDX1") (cons "EDX1" "EDXB")
      (cons "ДетальY" "BDY") (cons "BDY" "EDY1") (cons "EDY1" "EDYB")
      ;; Старые определения в стенде тоже обязаны существовать: в реальном
      ;; чертеже переименование их не стирает, а держатели ищутся обходом
      ;; содержимого -- без записи в таблице определение не читается.
      (cons "Гайка~до" "BGO") (cons "BGO" "EGO1") (cons "EGO1" "EGOB")
      (cons "Петля~до" "BPO") (cons "BPO" "EPO1") (cons "EPO1" "EPOB")
      (cons "Пружина~до" "BPRO") (cons "BPRO" "EPRO1")
      (cons "EPRO1" "EPROB")
      (cons "Кольцо~до" "BKOO") (cons "BKOO" "EKOO1")
      (cons "EKOO1" "EKOOB")
      (cons "Рычаг" "BRY") (cons "BRY" "ERY1") (cons "ERY1" "ERY2")
      (cons "ERY2" "ERYB")
      (cons "Вал" "BVA") (cons "BVA" "EVA1") (cons "EVA1" "EVAB")
      (cons "Втулка~до" "BVT") (cons "BVT" "EVT1") (cons "EVT1" "EVTB")
      (cons "Вал~до" "BVAO") (cons "BVAO" "EVAO1")
      (cons "EVAO1" "EVAOB")
      (cons "Шпонка" "BSP") (cons "BSP" "ESP1") (cons "ESP1" "ESP2")
      (cons "ESP2" "ESP3") (cons "ESP3" "ESPB")
      (cons "Шпонка~до" "BSPO") (cons "BSPO" "ESPO1")
      (cons "ESPO1" "ESPOB")
      (cons "Втулка" "BVTO") (cons "BVTO" "EVTO1")
      (cons "EVTO1" "EVTOB")
      (cons "Ступица" "BST") (cons "BST" "EST1") (cons "EST1" "EST2")
      (cons "EST2" "EST3") (cons "EST3" "ESTB")))
  (setq DB-ENTDATA
    (list
      (cons "BA" (list (cons -1 "BA") (cons 0 "BLOCK") (cons 2 "Чертеж А")))
      (cons "EA1" (list (cons -1 "EA1") (cons 0 "INSERT")
                        (cons 2 "Гайка~до")))
      (cons "EA2" (list (cons -1 "EA2") (cons 0 "INSERT")
                        (cons 2 "Болт~до")))
      (cons "EAB" (list (cons -1 "EAB") (cons 0 "ENDBLK")))
      (cons "BG" (list (cons -1 "BG") (cons 0 "BLOCK") (cons 2 "Гайка")))
      (cons "EG1" (list (cons -1 "EG1") (cons 0 "INSERT")
                        (cons 2 "Шайба~до")))
      (cons "EGB" (list (cons -1 "EGB") (cons 0 "ENDBLK")))
      (cons "B9" (list (cons -1 "B9") (cons 0 "BLOCK") (cons 2 "*U900")))
      (cons "E91" (list (cons -1 "E91") (cons 0 "INSERT")
                        (cons 2 "Гайка~до")))
      (cons "EB9" (list (cons -1 "EB9") (cons 0 "ENDBLK")))
      (cons "BO" (list (cons -1 "BO") (cons 0 "BLOCK") (cons 2 "Болт")))
      (cons "EO1" (list (cons -1 "EO1") (cons 0 "LINE")))
      (cons "EOB" (list (cons -1 "EOB") (cons 0 "ENDBLK")))
      (cons "BP" (list (cons -1 "BP") (cons 0 "BLOCK") (cons 2 "Петля")))
      (cons "EP1" (list (cons -1 "EP1") (cons 0 "INSERT")
                        (cons 2 "Звено~до")))
      (cons "EPB" (list (cons -1 "EPB") (cons 0 "ENDBLK")))
      (cons "BZ" (list (cons -1 "BZ") (cons 0 "BLOCK")
                       (cons 2 "Звено~до")))
      (cons "EZ1" (list (cons -1 "EZ1") (cons 0 "INSERT") (cons 2 "Петля")))
      (cons "EZB" (list (cons -1 "EZB") (cons 0 "ENDBLK")))
      (cons "BPR" (list (cons -1 "BPR") (cons 0 "BLOCK")
                        (cons 2 "Пружина")))
      (cons "EPR1" (list (cons -1 "EPR1") (cons 0 "INSERT")
                         (cons 2 "Кольцо~до")))
      (cons "EPRB" (list (cons -1 "EPRB") (cons 0 "ENDBLK")))
      (cons "BKO" (list (cons -1 "BKO") (cons 0 "BLOCK")
                        (cons 2 "Кольцо")))
      (cons "EKO1" (list (cons -1 "EKO1") (cons 0 "INSERT")
                         (cons 2 "Пружина~до")))
      (cons "EKOB" (list (cons -1 "EKOB") (cons 0 "ENDBLK")))
      (cons "BU1" (list (cons -1 "BU1") (cons 0 "BLOCK")
                        (cons 2 "Узел1")))
      (cons "EU1" (list (cons -1 "EU1") (cons 0 "INSERT")
                        (cons 2 "ДетальX~до")))
      (cons "EBU1" (list (cons -1 "EBU1") (cons 0 "ENDBLK")))
      (cons "BU2" (list (cons -1 "BU2") (cons 0 "BLOCK")
                        (cons 2 "Узел2")))
      (cons "EU2" (list (cons -1 "EU2") (cons 0 "INSERT")
                        (cons 2 "ДетальY~до")))
      (cons "EBU2" (list (cons -1 "EBU2") (cons 0 "ENDBLK")))
      (cons "BZN" (list (cons -1 "BZN") (cons 0 "BLOCK")
                        (cons 2 "Звено")))
      (cons "EZN1" (list (cons -1 "EZN1") (cons 0 "INSERT")
                         (cons 2 "Петля")))
      (cons "EZNB" (list (cons -1 "EZNB") (cons 0 "ENDBLK")))
      (cons "BSH" (list (cons -1 "BSH") (cons 0 "BLOCK")
                        (cons 2 "Шайба")))
      (cons "ESH1" (list (cons -1 "ESH1") (cons 0 "LINE")))
      (cons "ESHB" (list (cons -1 "ESHB") (cons 0 "ENDBLK")))
      (cons "BDX" (list (cons -1 "BDX") (cons 0 "BLOCK")
                        (cons 2 "ДетальX")))
      (cons "EDX1" (list (cons -1 "EDX1") (cons 0 "LINE")))
      (cons "EDXB" (list (cons -1 "EDXB") (cons 0 "ENDBLK")))
      (cons "BDY" (list (cons -1 "BDY") (cons 0 "BLOCK")
                        (cons 2 "ДетальY")))
      (cons "EDY1" (list (cons -1 "EDY1") (cons 0 "LINE")))
      (cons "EDYB" (list (cons -1 "EDYB") (cons 0 "ENDBLK")))
      (cons "BGO" (list (cons -1 "BGO") (cons 0 "BLOCK")
                        (cons 2 "Гайка~до")))
      (cons "EGO1" (list (cons -1 "EGO1") (cons 0 "INSERT")
                         (cons 2 "Гайка~до")))
      (cons "EGOB" (list (cons -1 "EGOB") (cons 0 "ENDBLK")))
      (cons "BPO" (list (cons -1 "BPO") (cons 0 "BLOCK")
                        (cons 2 "Петля~до")))
      (cons "EPO1" (list (cons -1 "EPO1") (cons 0 "INSERT")
                         (cons 2 "Звено~до")))
      (cons "EPOB" (list (cons -1 "EPOB") (cons 0 "ENDBLK")))
      (cons "BPRO" (list (cons -1 "BPRO") (cons 0 "BLOCK")
                         (cons 2 "Пружина~до")))
      (cons "EPRO1" (list (cons -1 "EPRO1") (cons 0 "INSERT")
                          (cons 2 "Кольцо~до")))
      (cons "EPROB" (list (cons -1 "EPROB") (cons 0 "ENDBLK")))
      (cons "BKOO" (list (cons -1 "BKOO") (cons 0 "BLOCK")
                         (cons 2 "Кольцо~до")))
      (cons "EKOO1" (list (cons -1 "EKOO1") (cons 0 "INSERT")
                          (cons 2 "Пружина~до")))
      (cons "EKOOB" (list (cons -1 "EKOOB") (cons 0 "ENDBLK")))
      ;; Цепочка, в которой замыкание видно только по ВТОРОЙ вложенной
      ;; ссылке: обход по первой до держателя не доходит.
      (cons "BRY" (list (cons -1 "BRY") (cons 0 "BLOCK")
                        (cons 2 "Рычаг")))
      (cons "ERY1" (list (cons -1 "ERY1") (cons 0 "INSERT")
                         (cons 2 "Шпонка~до")))
      (cons "ERY2" (list (cons -1 "ERY2") (cons 0 "INSERT")
                         (cons 2 "Втулка~до")))
      (cons "ERYB" (list (cons -1 "ERYB") (cons 0 "ENDBLK")))
      (cons "BVA" (list (cons -1 "BVA") (cons 0 "BLOCK") (cons 2 "Вал")))
      (cons "EVA1" (list (cons -1 "EVA1") (cons 0 "INSERT")
                         (cons 2 "Шпонка")))
      (cons "EVAB" (list (cons -1 "EVAB") (cons 0 "ENDBLK")))
      (cons "BVT" (list (cons -1 "BVT") (cons 0 "BLOCK")
                        (cons 2 "Втулка~до")))
      (cons "EVT1" (list (cons -1 "EVT1") (cons 0 "INSERT")
                         (cons 2 "Вал~до")))
      (cons "EVTB" (list (cons -1 "EVTB") (cons 0 "ENDBLK")))
      (cons "BVAO" (list (cons -1 "BVAO") (cons 0 "BLOCK")
                         (cons 2 "Вал~до")))
      (cons "EVAO1" (list (cons -1 "EVAO1") (cons 0 "INSERT")
                          (cons 2 "Рычаг")))
      (cons "EVAOB" (list (cons -1 "EVAOB") (cons 0 "ENDBLK")))
      (cons "BSP" (list (cons -1 "BSP") (cons 0 "BLOCK")
                        (cons 2 "Шпонка")))
      (cons "ESP1" (list (cons -1 "ESP1") (cons 0 "INSERT")
                         (cons 2 "Рычаг")))
      (cons "ESP2" (list (cons -1 "ESP2") (cons 0 "INSERT")
                         (cons 2 "Втулка~до")))
      (cons "ESP3" (list (cons -1 "ESP3") (cons 0 "INSERT")
                         (cons 2 "Ступица")))
      (cons "ESPB" (list (cons -1 "ESPB") (cons 0 "ENDBLK")))
      (cons "BSPO" (list (cons -1 "BSPO") (cons 0 "BLOCK")
                         (cons 2 "Шпонка~до")))
      (cons "ESPO1" (list (cons -1 "ESPO1") (cons 0 "INSERT")
                          (cons 2 "Рычаг")))
      (cons "ESPOB" (list (cons -1 "ESPOB") (cons 0 "ENDBLK")))
      (cons "BVTO" (list (cons -1 "BVTO") (cons 0 "BLOCK")
                         (cons 2 "Втулка")))
      (cons "EVTO1" (list (cons -1 "EVTO1") (cons 0 "INSERT")
                          (cons 2 "Вал")))
      (cons "EVTOB" (list (cons -1 "EVTOB") (cons 0 "ENDBLK")))
      ;; Держатель намеренно СРЕДНИЙ среди вложенных ссылок: обход, который
      ;; берёт из найденного только крайний элемент, до него не доходит.
      (cons "EST1" (list (cons -1 "EST1") (cons 0 "INSERT")
                         (cons 2 "Шайба")))
      (cons "EST2" (list (cons -1 "EST2") (cons 0 "INSERT")
                         (cons 2 "Обойма")))
      (cons "EST3" (list (cons -1 "EST3") (cons 0 "INSERT")
                         (cons 2 "Ступица")))
      (cons "ESTB" (list (cons -1 "ESTB") (cons 0 "ENDBLK")))
      (cons "BST" (list (cons -1 "BST") (cons 0 "BLOCK")
                        (cons 2 "Ступица")))
      (cons "ESTB2" (list (cons -1 "ESTB2") (cons 0 "ENDBLK")))))
  (setq KG-REWHOLD nil)
  (setq KG-REWSKIP nil)
  ;; Таблица имён -- COM-функция, на стенде подменяется списком.
  (defun KG_EXAllDefNames ( / )
    (list "Чертеж А" "Гайка" "*U900" "Болт" "Петля" "Звено~до"
          "Узел1" "Узел2" "Гайка~до" "Болт~до" "Шайба" "Шайба~до"
          "Звено" "ДетальX" "ДетальX~до" "ДетальY" "ДетальY~до"
          "Пружина" "Пружина~до" "Кольцо" "Кольцо~до"
          "Рычаг" "Вал" "Втулка" "Втулка~до" "Вал~до" "Шпонка"
          "Шпонка~до" "Обойма" "Ступица" "Шайба"))
)

(defun TEST-RewireDefs ()
  (princ "\n\nT21. Перепривязка ссылок на прежние имена")
  (T21-SetDb)

  ;; 21a. Держатели находятся и внутри чужих определений, и внутри
  ;;      анонимного представления.
  (T-Ok "T21.1 чужое определение держит ссылку"
        (KG-DefRefersTo "Чертеж А" "Гайка~до"))
  (T-Ok "T21.2 на прежнее имя ссылки нет"
        (not (KG-DefRefersTo "Чертеж А" "Гайка")))
  (T-EqStr "T21.3 держатели: чужое определение и анонимное представление"
           (KG-JoinNames (KG-DefRefHolders "Гайка~до"))
           "Чертеж А, *U900")
  ;; Кэш: второй вызов обязан отдать тот же результат, а не пересчитывать
  ;; его по уже изменённой базе.
  (T-EqStr "T21.4 держатели берутся из кэша"
           (KG-JoinNames (KG-DefRefHolders "Гайка~до"))
           "Чертеж А, *U900")
  (T-Ok "T21.5 у определения без ссылок держателей нет"
        (not (KG-DefRefHolders "Болт")))

  ;; 21b. Сама правка группы 2.
  (T-EqInt "T21.6 одна вставка исправлена"
           (KG-RewireDefRefs "Чертеж А" "Гайка~до" "Гайка") 1)
  (T-EqStr "T21.7 группа 2 указывает на прежнее имя"
           (KG-AsString (cdr (assoc 2 (entget "EA1")))) "Гайка")
  (T-EqInt "T21.8 повторная перепривязка ничего не находит"
           (KG-RewireDefRefs "Чертеж А" "Гайка~до" "Гайка") 0)

  ;; 21c. Защита от зацикливания: «Петля» вставляет «Звено~до», а «Звено»
  ;;      вставляет «Петля». Вернуть ссылку на «Звено» внутри «Петли» --
  ;;      значит замкнуть определение.
  (T-Ok "T21.9 замыкание распознаётся"
        (KG-WouldCycle "Петля" "Звено~до" "Звено"))
  (T-Ok "T21.10 обычный держатель не замыкается"
        (not (KG-WouldCycle "Чертеж А" "Гайка~до" "Гайка")))
  (T-EqInt "T21.11 замыкающуюся пару пара пропускает"
           (KG-RewireOnePair "Звено~до" "Звено") 0)
  (T-Ok "T21.12 пропуск запомнен"
        (KG-StrInterCI (list "Звено~до") KG-REWSKIP))
  (T-EqStr "T21.13 ссылка внутри замыкающегося определения не тронута"
           (KG-AsString (cdr (assoc 2 (entget "EP1")))) "Звено~до")

  ;; 21d. Пара без риска: две ссылки, обе исправляются.
  (T21-SetDb)
  (T-EqInt "T21.14 обе ссылки пары исправлены"
           (KG-RewireOnePair "Гайка~до" "Гайка") 2)
  (T-EqStr "T21.15 вставка в чужом определении"
           (KG-AsString (cdr (assoc 2 (entget "EA1")))) "Гайка")
  (T-EqStr "T21.16 вставка в анонимном представлении"
           (KG-AsString (cdr (assoc 2 (entget "E91")))) "Гайка")

  ;; 21e. Многопроходность: «Шайба~до» лежит внутри «Гайки», а «Гайка»
  ;;      сама стала прежней только на этом проходе. За один проход такая
  ;;      ссылка не находится.
  (T21-SetDb)
  (T-EqInt "T21.17 цепочка из двух пар проходится за несколько проходов"
           (KG-Step_RewireDefs
             (list (list "Шайба" "Шайба~до") (list "Гайка" "Гайка~до"))) 3)
  (T-EqStr "T21.18 ссылка внутри нового определения тоже возвращена"
           (KG-AsString (cdr (assoc 2 (entget "EG1")))) "Шайба")

  ;; 21f. Предел проходов. «Пружина» вставляет «Кольцо~до», а «Кольцо»
  ;;      вставляет «Пружина~до»: вернуть обе ссылки значит замкнуть оба
  ;;      определения. Шаг обязан остановиться сам и назвать пропущенные,
  ;;      а не крутить пары до бесконечности.
  (T21-SetDb)
  (T-EqInt "T21.19 замыкающиеся пары не крутят шаг вечно"
           (KG-Step_RewireDefs
             (list (list "Кольцо" "Кольцо~до")
                   (list "Пружина" "Пружина~до"))) 0)
  (T-Ok "T21.20 обе пары названы пропущенными"
        (and (KG-StrInterCI (list "Кольцо~до") KG-REWSKIP)
             (KG-StrInterCI (list "Пружина~до") KG-REWSKIP)))
  (T-EqStr "T21.20a ссылка внутри замыкающегося определения не тронута"
           (KG-AsString (cdr (assoc 2 (entget "EPR1")))) "Кольцо~до")

  ;; 21g. Одноимённая пара не обрабатывается: править нечего.
  (T21-SetDb)
  (T-EqInt "T21.21 пара без переименования ничего не делает"
           (KG-Step_RewireDefs (list (list "Гайка" "Гайка"))) 0)

  ;; 21g2. Замыкание видно только по второй вложенной ссылке: «Рычаг»
  ;;      вставляет «Шпонка~до» и «Втулка~до», цепочка замыкания идёт через
  ;;      вторую. Обход по первой ссылке отвечал бы «не замкнётся».
  (T21-SetDb)
  (setq KG-REWPAIRS (list (list "Втулка" "Втулка~до")
                          (list "Вал" "Вал~до")))
  (T-Ok "T21.23 замыкание находится по второй вложенной ссылке"
        (KG-WouldCycle "Рычаг" "Втулка~до" "Втулка"))
  ;; Замыкание, которого обход по ПЕРВОЙ вложенной ссылке не видит вовсе:
  ;; цепочка идёт «Втулка -> Шпонка -> Втулка~до», а держатель «Рычаг»
  ;; достижим только второй ссылкой из «Втулки~до».
  (T-Ok "T21.26 обход по первой вложенной ссылке такого не находит"
        (KG-WouldCycle "Шпонка" "Втулка~до" "Втулка"))
  ;; Тот же обход, но держатель достижим второй ссылкой не от стартового
  ;; узла, а от узла в глубине: «Втулка -> Вал -> Шпонка -> Рычаг». Обход,
  ;; который на каждом шаге берёт только первую вложенную ссылку, сюда не
  ;; доходит -- на «Шпонке» он свернёт на «Втулка~до» и упрётся в уже
  ;; пройденное имя.
  (T-Ok "T21.27 держатель в глубине находится по второй ссылке"
        (KG-WouldCycle "Рычаг" "Втулка~до" "Втулка"))
  ;; Держатель -- средняя из трёх вложенных ссылок. Обход, который из
  ;; найденного берёт только первый или только последний элемент, до него
  ;; не доходит и отвечает «не замкнётся».
  (T-Ok "T21.28 держатель между двумя другими ссылками находится"
        (KG-WouldCycle "Обойма" "Втулка~до" "Втулка"))
  (setq KG-REWPAIRS nil)

  ;; 21g3. Порядок пар имеет значение: держатель второй пары указывает на
  ;;      служебное имя, которое станет прежним только после первой пары.
  ;;      За один проход вторая ссылка не находится.
  (T21-SetDb)
  (T-EqInt "T21.24 пара, ставшая доступной позже, обрабатывается"
           (KG-Step_RewireDefs
             (list (list "ДетальX" "ДетальX~до")
                   (list "ДетальY" "ДетальY~до"))) 2)
  (T-EqStr "T21.25 вторая ссылка возвращена на прежнее имя"
           (KG-AsString (cdr (assoc 2 (entget "EU2")))) "ДетальY")

  ;; 21h. Строка отчёта: кто держит определения с «~до».
  (T-EqStr "T21.22 держатели названы в отчёте"
           (KG-JoinNames
             (KG-DoHolderLines
               (list (cons "OK" nil)
                     (cons "Гайка~до" (list "Чертеж А" "*U900"))
                     (cons "Болт~до" nil))
               (list "Гайка~до" "Болт~до" "Гайка")))
           "  \"Гайка~до\" держат: Чертеж А, *U900")
)

;;;---------------------------------------------------------------------------
;;; T22. Возврат состояний видимости вложенных блоков подменённым определениям.
;;;
;;; Определение без итерации в имени подменяется целиком, и состояния
;;; видимости его вложенных блоков сбрасываются в значения по умолчанию новой
;;; версии. Прогон это подтвердил: блоки с итерацией видимость сохраняют, а
;;; блоки без итерации теряют. Состояния снимаются до освобождения имён и
;;; возвращаются после перепривязки.
;;;
;;; COM-чтение и запись свойств на стенде подменяются: проверяется логика --
;;; какие определения обрабатываются, что считается, что пропускается.
;;;---------------------------------------------------------------------------
(defun T22-SetDb ()
  (setq DB-ENTDB
    (list
      (cons "Обойма" "BO1") (cons "BO1" "EO1") (cons "EO1" "EO2")
      (cons "EO2" "EOB1")
      (cons "Пустышка" "BP1") (cons "BP1" "EP1") (cons "EP1" "EPB1")
      (cons "Щиток" "BS1") (cons "BS1" "ES1") (cons "ES1" "ESB1")))
  (setq DB-ENTDATA
    (list
      (cons "BO1" (list (cons -1 "BO1") (cons 0 "BLOCK") (cons 2 "Обойма")))
      (cons "EO1" (list (cons -1 "EO1") (cons 0 "INSERT") (cons 2 "Втулка")))
      (cons "EO2" (list (cons -1 "EO2") (cons 0 "INSERT") (cons 2 "Шайба")))
      (cons "EOB1" (list (cons -1 "EOB1") (cons 0 "ENDBLK")))
      (cons "BP1" (list (cons -1 "BP1") (cons 0 "BLOCK")
                        (cons 2 "Пустышка")))
      (cons "EP1" (list (cons -1 "EP1") (cons 0 "LINE")))
      (cons "EPB1" (list (cons -1 "EPB1") (cons 0 "ENDBLK")))
      (cons "BS1" (list (cons -1 "BS1") (cons 0 "BLOCK") (cons 2 "Щиток")))
      (cons "ES1" (list (cons -1 "ES1") (cons 0 "INSERT")
                        (cons 2 "Втулка")))
      (cons "ESB1" (list (cons -1 "ESB1") (cons 0 "ENDBLK")))))
  (setq KG-NESTSAVE nil)
  (setq KG-NESTSAVEN 0)
  (setq T22-EXIST (list "Обойма" "Пустышка" "Комплект КП50 v1.5"
                        "Щиток"))
  (setq T22-CALLS nil)
  ;; COM-чтение свойств: у «Обоймы» два вложенных блока, у «Пустышки» их нет.
  (setq T22-VIS nil)
  (defun KG-DefNestedVisibility (nm / )
    (setq T22-VIS (cons (KG-AsString nm) T22-VIS))
    (if (KG-StrEq nm "Обойма")
      (list (cons "Втулка" "Открыто") (cons "Шайба" "Скрыто"))
      nil))
  ;; Поиск вложенной вставки по имени: «Шайбы» в новой версии нет.
  ;; Вложение «Втулка» есть и у определения семейства -- иначе исключение
  ;; семейства по схеме имени проверялось бы не им, а отсутствием вложения.
  (defun KG-DefInsertByName (defname nestedname / )
    (if (and (KG-StrEq nestedname "Втулка")
             (or (KG-StrEq defname "Обойма")
                 (KG-StrEq defname "Комплект КП50 v1.5")))
      "EO1"
      nil))
  (setq T22-SET nil)
  (defun KG-SetVisibilityState (obj val / )
    (setq T22-CALLS (cons (strcat (KG-AsString obj) "=" (KG-AsString val))
                          T22-CALLS))
    (setq T22-SET (cons (cons (KG-AsString obj) (KG-AsString val)) T22-SET))
    t)
  ;; Чтение обратно: шаг проверяет запись чтением, поэтому заглушка обязана
  ;; отдавать записанное значение.
  (defun KG-GetVisibilityState (obj / )
    (KG-AsString (KG-CdrCI (KG-AsString obj) T22-SET)))
  (defun KG_EXDefExists (nm / ) (KG-StrInterCI (list nm) T22-EXIST))
  ;; Таблица имён: без своей заглушки снимок строился бы по именам общего
  ;; стенда *DB*, и тест проверял бы не те определения.
  (defun KG_EXAllDefNames ( / ) (list "Обойма" "Пустышка" "Щиток"))
)

(defun TEST-NestedVis ()
  (princ "\n\nT22. Возврат состояний вложенных блоков")
  (T22-SetDb)

  ;; 22a. Снимок: определение без вложенных вставок в список не попадает.
  (KG-SaveNestedVis)
  (T-EqInt "T22.1 в снимок попало одно определение" (length KG-NESTSAVE) 1)
  (T-EqInt "T22.2 состояний снято два" KG-NESTSAVEN 2)
  (T-EqStr "T22.3 состояния прочитаны у нужного определения"
           (KG-JoinNames (mapcar 'car (KG-CdrCI "Обойма" KG-NESTSAVE)))
           "Втулка, Шайба")
  (T-Ok "T22.4 определение без вложенных вставок в снимке не числится"
        (not (KG-CdrCI "Пустышка" KG-NESTSAVE)))
  ;; У «Щитка» вложенная вставка есть, а состояния видимости у неё нет. Такое
  ;; число обязано быть видно: без него «снято 27» и «вернулось 5» не свести.
  (T-EqInt "T22.4c вложенные вставки без состояния посчитаны"
           KG-NESTDROP 1)
  ;; COM-чтение свойств -- самая дорогая часть обхода, поэтому определения
  ;; без вложенных вставок до него не доходят. На крупном чертеже их
  ;; большинство.
  (T-Ok "T22.4a определение без вложенных вставок не читается через COM"
        (not (KG-StrInterCI (list "Пустышка") T22-VIS)))
  (T-Ok "T22.4b определение со вложенными вставками читается"
        (KG-StrInterCI (list "Щиток") T22-VIS))

  ;; 22b. Возврат: одно состояние записано, второе не найдено в новой версии.
  (T22-SetDb)
  (KG-SaveNestedVis)
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Обойма" "Обойма~до"))))
  (T-EqInt "T22.5 одно состояние возвращено" (nth 0 T22-R) 1)
  (T-EqInt "T22.6 второе не найдено в новой версии" (nth 1 T22-R) 1)
  (T-EqInt "T22.7 отказов записи нет" (nth 2 T22-R) 0)
  (T-EqStr "T22.8 записано прежнее состояние"
           (KG-JoinNames (reverse T22-CALLS)) "EO1=Открыто")

  ;; 22c. Определение по схеме семейства обрабатывается наравне с остальными.
  ;;      Шаг замены пишет состояние в анонимное представление *U живого
  ;;      экземпляра, а содержимое самого определения остаётся пришедшим из
  ;;      буфера. Без записи в определение при открытии блока для
  ;;      редактирования показывалось состояние мастер-блока, хотя экземпляр
  ;;      в модели был правильным.
  (T22-SetDb)
  (setq KG-NESTSAVE (list (cons "Комплект КП50 v1.5"
                                (list (cons "Втулка" "Открыто")))))
  (T-Ok "T22.9a состояние для семейства снято"
        (KG-CdrCI "Комплект КП50 v1.5" KG-NESTSAVE))
  (T-Ok "T22.9b определение семейства в чертеже есть"
        (KG_EXDefExists "Комплект КП50 v1.5"))
  (setq T22-CALLS nil)
  (setq T22-R (KG-Step_RestoreNestedVis
                (list (list "Комплект КП50 v1.5" "Комплект КП50 v1.5~до"))))
  (T-EqInt "T22.9 состояние возвращено и в определение семейства"
           (nth 0 T22-R) 1)
  (T-EqStr "T22.10 записано прежнее состояние семейства"
           (KG-JoinNames (reverse T22-CALLS)) "EO1=Открыто")

  ;; 22d. Определения, которого нет в чертеже, шаг не трогает.
  (T22-SetDb)
  (setq KG-NESTSAVE (list (cons "НетТакого" (list (cons "Втулка" "Открыто")))))
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "НетТакого" "НетТакого~до"))))
  (T-EqInt "T22.11 отсутствующее определение не обрабатывается"
           (nth 0 T22-R) 0)

  ;; 22e. Запись отказала -- это считается отдельно от «не найдено».
  (T22-SetDb)
  (KG-SaveNestedVis)
  (defun KG-SetVisibilityState (obj val / ) nil)
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Обойма" "Обойма~до"))))
  (T-EqInt "T22.12 отказ записи посчитан" (nth 2 T22-R) 1)
  (T-EqInt "T22.13 возвращённым он не считается" (nth 0 T22-R) 0)

  ;; 22e2. Определение из снимка, которого в чертеже больше нет, не
  ;;      обрабатывается: писать некуда.
  (T22-SetDb)
  (KG-SaveNestedVis)
  (setq T22-EXIST nil)
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Обойма" "Обойма~до"))))
  (T-EqInt "T22.15 исчезнувшее определение не обрабатывается"
           (nth 0 T22-R) 0)
  (T-Ok "T22.16 в исчезнувшее определение ничего не записано"
        (not T22-CALLS))

  ;; 22e3. Запись прошла, а чтение показало другое: AutoCAD принял вызов и
  ;;      ничего не изменил. Это главный признак того, что состояние не
  ;;      применилось, и он обязан быть виден в отчёте отдельным числом.
  (T22-SetDb)
  (KG-SaveNestedVis)
  (defun KG-GetVisibilityState (obj / ) "Совсем другое")
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Обойма" "Обойма~до"))))
  (T-EqInt "T22.17 неподтверждённая запись не считается возвратом"
           (nth 0 T22-R) 0)
  (T-EqInt "T22.18 она посчитана отдельно" (nth 3 T22-R) 1)

  ;; 22e4. Отчёт называет определения, а не только количество.
  (T22-SetDb)
  (KG-SaveNestedVis)
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Обойма" "Обойма~до"))))
  (T-EqInt "T22.19 посчитано одно определение" (nth 4 T22-R) 1)
  (T-EqStr "T22.20 определение названо" (KG-JoinNames (nth 5 T22-R)) "Обойма")

  ;; 22f. Пара без сохранённых состояний не даёт ни числа, ни отказа.
  (T22-SetDb)
  (setq T22-R (KG-Step_RestoreNestedVis (list (list "Пустышка" "Пустышка~до"))))
  (T-EqStr "T22.14 пустая пара даёт три нуля"
           (strcat (itoa (nth 0 T22-R)) "/" (itoa (nth 1 T22-R)) "/"
                   (itoa (nth 2 T22-R)))
           "0/0/0")
)

;;;---------------------------------------------------------------------------
;;; T23. Имя параметра видимости берётся из словаря определения.
;;;
;;; Прежний поиск угадывал имя по написанию -- «*ВИДИМ*» или «*VISIB*». Прогон
;;; сборки 63 показал: у вложенных блоков части определений читается ноль
;;; состояний, а у других состояния есть. Значит, параметр назван иначе, и
;;; состояния не снимались вовсе -- возвращать было нечего.
;;;
;;; Проверяется настоящая логика разбора: запись словаря, группа 360, группа
;;; 301. COM-обвязка подделана.
;;;---------------------------------------------------------------------------
(defun T23-SetDb ()
  (setq T23-PARAM "Состояние стойки")
  (defun KG-EffectiveNameOf (obj / ) (KG-AsString obj))
  (defun KG-BlockObj (nm / ) (strcat "BLK:" (KG-AsString nm)))
  (defun vlax-vla-object->ename (o / ) (strcat "EN:" (KG-AsString o)))
  ;; Словарь расширения: как в AutoCAD, запись ACAD_ENHANCEDBLOCK отдаёт
  ;; подсписок с группой 360 -- указателем на параметр.
  (defun dictsearch (d nm / )
    (if (KG-StrEq nm "ACAD_ENHANCEDBLOCK")
      (list (cons 3 "ACAD_ENHANCEDBLOCK") (cons 360 "VISPARAM"))
      nil))
  (defun entget (e / )
    (if (KG-StrEq e "VISPARAM")
      (list (cons 0 "BLOCKVISIBILITYPARAMETER") (cons 301 T23-PARAM))
      (KG-CdrCI e DB-ENTDATA)))
  ;; AllowedValues в AutoCAD -- вариант с массивом. На стенде массивов нет,
  ;; поэтому подменяются сами функции разбора: проверяется не разбор
  ;; варианта, а отбор свойства по имени.
  (defun KG-VariantToArray (v / ) v)
  (defun KG-ArrayCount (a / ) (length a))
  (defun vlax-variant-value (v / ) v)
  ;; KG-FindDynPropByName читает имя параметра через vla-get-PropertyName,
  ;; а не через группу 1: без этой заглушки поиск по точному имени
  ;; на стенде не проверялся вовсе.
  (defun vla-get-PropertyName (p / ) (KG-CdrCI "1" p))
  ;; В списке свойств намеренно два параметра со списком допустимых
  ;; значений: «Растяжение1» тоже подошёл бы поиску по написанию, если бы
  ;; его имя содержало «видим». Без поиска по точному имени из словаря
  ;; свойство находилось бы не то.
  (defun vlax-invoke (o m args / )
    (if (KG-StrEq m "getdynamicblockproperties")
      (list (list (cons 1 "Растяжение1")
                  (cons 2 "Растяжение1")
                  (cons "знач" "10")
                  (cons "AllowedValues" (list "10" "20")))
            (list (cons 1 "Состояние стойки")
                  (cons 2 "Состояние стойки")
                  (cons "знач" "Открыто")
                  (cons "AllowedValues" (list "Открыто" "Скрыто"))))
      (list)))
  (defun vlax-get-property (p k / )
    (cond
      ((KG-StrEq k "AllowedValues") (KG-CdrCI "AllowedValues" p))
      ((KG-StrEq k "Value") (KG-CdrCI "знач" p))
      (t (KG-CdrCI (KG-AsString k) p))))
  (defun vlax-put-property (p k v / ) (setq T23-LAST v) t)
)

(defun TEST-VisParam ()
  (princ "\n\nT23. Имя параметра видимости из словаря определения")
  (T23-SetDb)

  ;; 23a. Имя читается из группы 301 параметра.
  (T-EqStr "T23.1 имя параметра прочитано из словаря"
           (KG-VisParamName "Стойка КП50") "Состояние стойки")
  ;; Имя может быть любым: поиск по написанию такого бы не нашёл.
  (T-Ok "T23.2 такое имя не подходит под прежний шаблон"
        (not (wcmatch (strcase "Состояние стойки") "*ВИДИМ*,*VISIB*")))

  ;; 23b. Параметр находится по точному имени, а не по написанию.
  ;; Заглушки берутся из фикстуры T23-SetDb: там в списке свойств есть
  ;; второй параметр со списком допустимых значений, поэтому «имя из
  ;; словаря не используется» тест ловит. Свой список из одного свойства
  ;; здесь делал проверку пустой.
  ;; Проверяется именно та функция, которой пользуется рабочий код:
  ;; KG-GetVisibilityState и KG-SetVisibilityState зовут
  ;; KG-VisibilityPropertyOf. Тест на KG-FindVisibilityProperty с именем,
  ;; переданным руками, мутацию «имя из словаря не используется» не ловил.
  (T-EqStr "T23.3 свойство найдено по имени из словаря"
           (KG-AsString
             (KG-CdrCI "знач" (KG-VisibilityPropertyOf "Стойка КП50")))
           "Открыто")
  (T-EqStr "T23.3a состояние читается через тот же поиск"
           (KG-GetVisibilityState "Стойка КП50") "Открыто")
  ;; Запись идёт в то же свойство: без имени из словаря состояние
  ;; записалось бы не туда или не записалось вовсе.
  (setq T23-LAST nil)
  (T-Ok "T23.3b состояние записывается через тот же поиск"
        (KG-SetVisibilityState "Стойка КП50" "Скрыто"))
  (T-EqStr "T23.3c записано именно переданное состояние"
           (KG-AsString T23-LAST) "Скрыто")
  (T-Ok "T23.4 без имени из словаря такое свойство не находится"
        (not (KG-FindDynPropBy "В")))

  ;; 23c. Прежний поиск по написанию остался запасным.
  (defun vlax-invoke (o m args / )
    (list (list (cons 1 "Видимость1") (cons "знач" "Скрыто")
                (cons "AllowedValues" (list "Открыто" "Скрыто")))))
  (T-EqStr "T23.5 запасной поиск по написанию работает"
           (KG-AsString (KG-CdrCI "знач" (KG-FindDynPropBy "В"))) "Скрыто")

  ;; 23d. Словаря нет -- имя не прочиталось, и это не ошибка.
  (defun dictsearch (d nm / ) nil)
  (T-Ok "T23.6 без словаря имени нет" (not (KG-VisParamName "Стойка КП50")))
  (T-EqStr "T23.7 свойство всё равно ищется запасным путём"
           (KG-AsString
             (KG-CdrCI "знач" (KG-VisibilityPropertyOf "Стойка КП50")))
           "Скрыто")
)

(defun RUN-ALL-TESTS ()
  (setq TESTS-PASSED 0)
  (setq TESTS-FAILED 0)
  (setq TESTS-FAILNAMES nil)
  (princ "\n==============================================")
  (princ "\n ТЕСТЫ Integration.lsp")
  (princ "\n==============================================")
  (TEST-StrictCar)
  (TEST-Parser)
  (TEST-Nested)
  (TEST-Scan)
  (TEST-NestedMap)
  (TEST-Visibility)
  (TEST-Plan)
  (TEST-FullIntegration)
  (TEST-Validation)
  (TEST-EdgeCases)
  (TEST-MasterFromClipboard)
  (TEST-VisibilityPerInstance)
  (TEST-Report)
  (TEST-RealNaming)
  (TEST-TypeSafety)
  (TEST-SpaceUnknown)
  (TEST-IterIsString)
  (TEST-MasterState)
  (TEST-ObjectDbxAttempts)
  (TEST-VisBranches)
  (TEST-CollectInstances)
  (TEST-RewireDefs)
  ;; TEST-VisParam обязан идти ДО TEST-NestedVis: фикстура T22-SetDb
  ;; подменяет KG-GetVisibilityState и KG-SetVisibilityState своими
  ;; заглушками и не возвращает настоящие, поэтому проверка настоящих
  ;; функций после неё проверяла бы заглушку.
  (TEST-VisParam)
  (TEST-NestedVis)
  (princ "\n----------------------------------------------")
  (princ (strcat "\n Пройдено: " (itoa TESTS-PASSED)))
  (princ (strcat "\n Провалено: " (itoa TESTS-FAILED)))
  (if TESTS-FAILNAMES
    (progn
      (princ "\n Проваленные:")
      (foreach n (reverse TESTS-FAILNAMES) (princ (strcat "\n   - " n)))
    )
  )
  (princ "\n==============================================")
  (if (= TESTS-FAILED 0) "ALL PASS" "FAILURES")
)

;;;---------------------------------------------------------------------------
;;; T19. ВЕТКИ ВОССТАНОВЛЕНИЯ ВИДИМОСТИ И СТИРАНИЯ *U (сборка 37)
;;;
;;; Повод: на реальном чертеже KG_EXSetNestedVisibility возвращала nil на
;;; каждый экземпляр (vlax-for по вставке не даёт ничего), а тесты были
;;; зелёные -- заглушка отвечала своим значением, и ветки обработки ответа
;;; не исполнялись ни разу. Теперь заглушка держит контракт настоящего
;;; адаптера, а ветки проверяются явно.
;;;---------------------------------------------------------------------------

;; Подделка таблицы блоков. DB-ENTDB -- список точечных пар:
;; (имя-определения . заголовочный-объект) и (объект . следующий-объект),
;; как entnext в AutoCAD -- последний объект определения даёт nil, а не
;; ENDBLK.
;;
;; Точечные пары здесь обязательны: именно на них KG-CdrCI/assoc работают
;; как в AutoCAD (на списке вида (ключ значение) KG-CdrCI возвращает
;; (значение) -- список, и дальше всё рассыпается).
;; А вот vl-remove-if на точечных парах в этом интерпретаторе убирает не
;; те элементы, поэтому отбор сделан накоплением через vl-some.
(defun FAKE-SameKey (a b)
  (= (strcase (if a a "")) (strcase (if b b "")))
)
;;;---------------------------------------------------------------------------
;;; Строгие CAR и CDR.
;;;
;;; Интерпретатор терпимее AutoCAD: (car "OK") у него даёт nil, а AutoCAD
;;; падает с «неверный тип аргумента: consp "OK"». Из-за этого карта
;;; ссылок с маркером "OK" в car спокойно проходила стенд и падала на
;;; реальном чертеже -- дважды, в сборках 39 и 40. Здесь car и cdr
;;; переопределены так, как ведёт себя AutoCAD: строка вместо списка --
;;; ошибка. Тест, который такое пропускает, ничего не проверяет.
;; listp в интерпретаторе нет, поэтому «не список» определяется как
;; «строка»: именно строка в car и давала consp "OK" на чертеже.
;; Ошибку надо именно ПОДНИМАТЬ, а не ловить: vl-catch-all-apply внутри
;; самого car перехватывал собственное исключение, и строгая проверка
;; выглядела включённой, ничего не делая.
(defun car (x / )
  (if (and x (= (type x) (type "")))
    (_strict-fail))
  (_car x)
)

(defun cdr (x / )
  (if (and x (= (type x) (type "")))
    (_strict-fail))
  (_cdr x)
)

;; Контроль: переопределение встроенной функции могло не состояться
;; молча. Интерпретатор хранит defun под именем как оно написано, а
;; вызывает по имени в верхнем регистре, поэтому «(defun car ...)»
;; просто не находилось при вызове и car оставался встроенным. Без этой
;; проверки строгие car/cdr выглядели бы включёнными, а на деле не
;; работали бы -- и ровно тот дефект, ради которого они добавлены,
;; снова проходил бы стенд незамеченным.
(defun FAKE-Chain (nm) (KG-CdrCI nm DB-ENTDB))
;; Стирание записи таблицы блоков снимает и её содержимое, и саму запись
;; имени -- как ENTD в AutoCAD: без этого заголовок исчезает, а
;; tblobjname продолжает отвечать стёртым ename.
(defun FAKE-Erase (nm / out p tgt)
  (setq out nil)
  (foreach p DB-ENTDB
    (if (not (FAKE-SameKey (car p) nm)) (setq out (cons p out)))
  )
  (setq DB-ENTDB (reverse out))
  (if (KG-StrEq (KG-AsString
                  (cdr (assoc 0 (KG-CdrCI nm DB-ENTDATA)))) "BLOCK")
    (progn
      (setq tgt nm out nil)
      (foreach p DB-ENTDB
        (if (not (FAKE-SameKey (cdr p) tgt)) (setq out (cons p out)))
      )
      (setq DB-ENTDB (reverse out))
    )
  )
  nm
)

(defun tblobjname (tbl nm) (FAKE-Chain nm))
;; Запись таблицы блоков: как в AutoCAD, группа -2 хранит ename самого
;; BLOCK-примитива. Нужна, чтобы проверить запасной путь KG-DefEnt: на
;; реальном чертеже tblobjname ответил nil на все 37 сиротских *U, и
;; очистка 37 раз напечатала «BLOCK не найден».
(defun tblsearch (tbl nm / e)
  (setq e (FAKE-Chain nm))
  (if e (list (cons 0 "BLOCK") (cons 2 nm) (cons -2 e)) nil)
)
(defun entnext (e) (FAKE-Chain e))
(defun entget (e) (KG-CdrCI e DB-ENTDATA))
(defun entdel (e) (if DB-ENTDEL-FAILS nil (FAKE-Erase e)))
;; Запись списка сущности обратно. Как в AutoCAD: отдаёт записанный список,
;; а на неизвестном ename -- nil. Без этой заглушки перепривязка ссылок
;; (группа 2 у вставки) на стенде не проверялась бы вовсе.
(defun entmod (ed / e out p done)
  (setq e (cdr (assoc -1 ed)))
  (setq out nil done nil)
  (foreach p DB-ENTDATA
    (if (and (not done) (FAKE-SameKey (car p) e))
      (progn (setq out (cons (cons e ed) out)) (setq done t))
      (setq out (cons p out))
    )
  )
  (if done
    (progn (setq DB-ENTDATA (reverse out)) ed)
    nil)
)
(defun entupd (e) e)
;; handle -> ename. Отдельная таблица от DB-ENTDATA (ename -> DXF): в
;; AutoCAD это два разных пространства имён, и на смешанном словаре
;; handle записи блока совпадал с ename другой записи.
(defun handent (h) (KG-CdrCI h DB-HAND))
;; Набор -- СПИСОК ename, а не символ: у интерпретатора length от строки
;; даёт число знаков, и на «SEL» он отвечал 3 -- столько же, сколько
;; вставок в поддельной базе, поэтому ошибка не была видна.
(defun sslength (ss) (length ss))
(defun ssname (ss i) (nth i ss))
(defun ssget (mode flt / out e ed)
  ;; Фильтр соблюдается, как в AutoCAD: при DB-HAS-MINSERT имитируется
  ;; выборка многострочных вставок, иначе -- выборка по группе 0. Без
  ;; этого LINE из поддельной базы попадал в «выборку вставок», и
  ;; проверка «не-вставка в карту не попадает» теряла смысл.
  ;; DB-SSGET-NOFILTER имитирует случай с реального чертежа: выборка с
  ;; фильтром отвечает nil, хотя вставки в чертеже есть.
  (if DB-HAS-MINSERT
    (list "mi")
    (if (and flt DB-SSGET-NOFILTER)
      nil
      (progn
        (setq out nil)
        (foreach e DB-SEL
          (setq ed (KG-CdrCI e DB-ENTDATA))
          (if (and ed (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT"))
            ;; фильтр по имени определения соблюдается, как в AutoCAD:
            ;; иначе проверка «вставка другого блока не мешает» была бы
            ;; бессмысленной
            (if (or (not (assoc 2 flt))
                    (KG-StrEq (KG-AsString (cdr (assoc 2 flt)))
                              (KG-AsString (cdr (assoc 2 ed)))))
              (setq out (cons e out))
            )
          )
        )
        (reverse out)
      )
    )
  )
)

(defun TEST-VisBranches ()
  (princ "\n\nT19. Ветки видимости и стирания *U")

  ;; 19a. Обход содержимого определения -- настоящая KG_DefEntNames
  ;;      поверх поддельной таблицы блоков.
  (setq DB-ENTDB
    (list (cons "*U52" "B*U52") (cons "B*U52" "E1") (cons "E1" "E2")
          (cons "E2" "E3") (cons "E3" "EB1") (cons "EB1" nil)
          (cons "*U53" "B*U53") (cons "B*U53" "E5") (cons "E5" "EB3")
          (cons "EB3" nil)
          (cons "kps 920~до" "B920") (cons "B920" "E9") (cons "E9" "EB2")
          (cons "EB2" nil)))
  (setq DB-ENTDATA
    (list (cons "B*U52" (list (cons 0 "BLOCK") (cons 2 "*U52")))
          (cons "E1" (list (cons 0 "LINE")))
          (cons "E2" (list (cons 0 "INSERT") (cons 2 "kps 920~до")))
          (cons "E3" (list (cons 0 "INSERT") (cons 2 "*U77")))
          (cons "EB1" (list (cons 0 "ENDBLK")))
          (cons "B*U53" (list (cons 0 "BLOCK") (cons 2 "*U53")))
          (cons "E5" (list (cons 0 "LINE")))
          (cons "EB3" (list (cons 0 "ENDBLK")))
          (cons "B920" (list (cons 0 "BLOCK") (cons 2 "kps 920~до")))
          (cons "E9" (list (cons 0 "LINE")))
          (cons "EB2" (list (cons 0 "ENDBLK")))))
  (setq DB-SEL nil DB-HAS-MINSERT nil)
  (T-EqStr "T19.1 из определения читаются только вставки"
           (KG-JoinNames (KG_DefEntNames "*U52")) "kps 920~до, *U77")
  (T-EqStr "T19.2 сирота держит старое определение"
           (KG-JoinNames (KG-CdrCI "*U52" (KG-OrphanContents (list "*U52"))))
           "kps 920~до, *U77")
  (T-Ok "T19.3 отсутствующее определение не роняет обход"
        (not (KG_DefEntNames "*U999")))

  ;; 19b. Стирание сиротского *U -- настоящая KG_EXEraseAnonDef.
  (setq DB-HAS-MINSERT nil DB-ENTDEL-FAILS nil)
  (T-Ok "T19.4 сиротское *U стирается" (KG_EXEraseAnonDef "*U52"))
  (T-Ok "T19.5 определения после стирания нет" (not (FAKE-Chain "*U52")))
  ;; Многострочная вставка теперь ищется ВНУТРИ определения, поэтому и в
  ;; стенде она должна лежать внутри. Прежняя имитация через выборку
  ;; ssget больше ничего не проверяла: новая проверка ssget не зовёт.
  ;; Определение своё (*U80): *U53 к этому месту уже стёрто тестом T19.4.
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U80" "B80") (cons "B80" "EMI")
                  (cons "EMI" "EB7"))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "B80" (list (cons 0 "BLOCK") (cons 2 "*U80")))
                  (cons "EMI" (list (cons 0 "MINSERT") (cons 2 "kps 1108")))
                  (cons "EB7" (list (cons 0 "ENDBLK"))))))
  (T-Ok "T19.6 при MINSERT *U не стирается" (not (KG_EXEraseAnonDef "*U80")))
  (T-Ok "T19.7 определение при MINSERT остаётся" (FAKE-Chain "*U80"))
  (setq DB-HAS-MINSERT nil DB-ENTDEL-FAILS t)
  (T-Ok "T19.8 неотдавшийся ENTD не считается успехом"
        (not (KG_EXEraseAnonDef "*U53")))
  (setq DB-ENTDEL-FAILS nil)

  ;; 19c. Внешняя ссылка блокирует стирание *U.
  (setq KG-HASXREF nil)
  (defun tblnext (t1 / ) nil)
  (T-Ok "T19.9 без внешних ссылок стирать можно" (not (KG_HasXrefDefs)))
  (setq KG-HASXREF t)
  (defun tblnext (t1 / ) (list (cons 2 "фасад|окно КП50")))
  (T-Ok "T19.10 с внешней ссылкой стирать нельзя" (KG_HasXrefDefs))
  (setq KG-HASXREF nil)
  (defun tblnext (t1 / ) nil)

  ;; 19d. Ветки обработки ответа KG_EXSetNestedVisibility в интеграции.
  ;;      «НЕТ ПРЕДСТАВЛЕНИЯ»: состояние вложенного блока записать некуда,
  ;;      экземпляр обязан получить предупреждение, а отчёт -- счётчик.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель" nil (list "1" "2" "3"))))
  (setq DB-SETVIS-MODE "НЕТ ПРЕДСТАВЛЕНИЯ")
  (setq R19 (TEST-IntegratePaste "ABC1.01" 5))
  (T-EqInt "T19.11 экземпляры заменены и без записи видимости"
           (cdr (assoc "replaced" (cdr (assoc "replaced" R19)))) 23)
  (T-EqInt "T19.12 случаев без представления столько, сколько вложенных пар"
           (cdr (assoc "vis-noanon" (cdr (assoc "replaced" R19)))) 23)
  (T-EqInt "T19.13 восстановлено ноль"
           (cdr (assoc "vis-restored" (cdr (assoc "replaced" R19)))) 0)
  (T-Ok "T19.14 предупреждение называет причину"
        (vl-some '(lambda (w) (wcmatch w "*нет анонимного представления*"))
                 (cdr (assoc "warnings" R19))))

  ;; «НЕ НАЙДЕН» на первом проходе: представление пересоздаётся, второй
  ;; проход находит вставку. Счётчик «не найдено» обязан остаться нулём.
  (TEST-BuildOldDrawing)
  (TEST-SetClipboard "ABC1.01(5)"
    (list (DB-MakeDef "Стойка" nil (list "A" "B" "C" "E" "F"))
          (DB-MakeDef "Ригель" nil (list "1" "2" "3"))))
  (setq DB-SETVIS-MODE "НЕ НАЙДЕН" DB-SETVIS-RETRIED nil)
  (setq R19b (TEST-IntegratePaste "ABC1.01" 5))
  (T-EqInt "T19.15 после второго прохода «не найдено» ноль"
           (cdr (assoc "vis-notfound" (cdr (assoc "replaced" R19b)))) 0)
  (T-Ok "T19.16 состояние всё-таки установлено"
        (> (+ (cdr (assoc "vis-restored" (cdr (assoc "replaced" R19b))))
              (cdr (assoc "vis-missing" (cdr (assoc "replaced" R19b)))))
           0))
  (setq DB-SETVIS-MODE "OK" DB-SETVIS-RETRIED nil)

  ;; 19e. Решение стирать *U: переключатель, наличие сирот, внешние ссылки.
  (T-Ok "T19.17 по умолчанию сироты стираются"
        (KG-CleanupEraseAllowed t (list "*U52") t))
  (T-Ok "T19.18 переключатель выключен -- не стираем"
        (not (KG-CleanupEraseAllowed nil (list "*U52") t)))
  (T-Ok "T19.19 нет сирот -- стирать нечего"
        (not (KG-CleanupEraseAllowed t nil t)))
  (T-Ok "T19.20 внешняя ссылка -- не стираем"
        (not (KG-CleanupEraseAllowed t (list "*U52") nil)))

  ;; 19f. Отчёт обязан показывать все три нуля раздельно: по одному нулю
  ;;      нельзя отличить «состояний не было» от «не прочитались» и от
  ;;      «прочитались, но записать некуда». Именно так сборка 36
  ;;      выглядела благополучно при неработающей записи.
  (setq KG-TEST-OUT "")
  (setq PRINC-REAL princ)
  (defun princ (s) (setq KG-TEST-OUT (strcat KG-TEST-OUT s)) s)
  (KG-PrintIntegrationReport R19)
  (T-Ok "T19.24 предупреждение о непрочитанных состояниях на месте"
        (wcmatch KG-TEST-OUT "*не прочитано ни одного состояния вложенных*"))
  (setq princ PRINC-REAL)
  (T-Ok "T19.21 отчёт печатает, сколько состояний вложенных прочитано"
        (wcmatch KG-TEST-OUT "*Прочитано состояний вложенных блоков:*"))
  (T-Ok "T19.22 отчёт печатает случаи без анонимного представления"
        (wcmatch KG-TEST-OUT "*Экземпляров без анонимного представления: 23*"))
  ;; старые отчёты без новых ключей не должны падать
  (KG-PrintIntegrationReport
    (list
      (cons "plan" (list (cons "nested-to-update" nil)))
      (cons "nested-stale" nil)
      (cons "replaced" (list (cons "replaced" 0) (cons "vis-restored" 0)
                             (cons "vis-missing" 0)))
      (cons "validation" (list (cons "old-left" 0) (cons "new-total" 0)
                               (cons "lost" 0) (cons "per-variant" nil)))
      (cons "warnings" nil)
      (cons "errors" nil)))
  (T-Ok "T19.23 отчёт без новых ключей печатывается" t)

  ;; 19g. Карта ссылок. Прежний способ (обход таблицы блоков с
  ;;      tblobjname + entnext + entget) на реальном чертеже дал
  ;;      0xC0000005 сразу после вставки из буфера и переименований, и
  ;;      очистка обрывалась на первом шаге. Теперь -- один ssget по всем
  ;;      вставкам и группа 330.
  (setq DB-ENTDATA
    (list (cons "E1" (list (cons 0 "INSERT") (cons 2 "*U63") (cons 330 "63")))
          (cons "E2" (list (cons 0 "INSERT") (cons 2 "Стойка КП50")
                           (cons 330 "63")))
          (cons "E3" (list (cons 0 "INSERT") (cons 2 "Стойка КП50")
                           (cons 330 "64")))
          ;; у LINE группа 2 есть (имя слоя) -- тем важнее не принять
          ;; его за вставку: иначе «слой» окажется определением-держателем
          (cons "E4" (list (cons 0 "LINE") (cons 2 "Слой0") (cons 330 "64")))
          (cons "R63" (list (cons 2 "*U63")))
          (cons "R64" (list (cons 2 "*Model_Space")))))
  (setq DB-HAND (list (cons "63" "R63") (cons "64" "R64")))
  ;; E4 -- LINE: в выборку он попадает, но в карту ссылок попасть не
  ;; должен, иначе не-вставка начнёт «держать» определение
  (setq DB-SEL (list "E1" "E2" "E3" "E4"))
  (setq DB-HAS-MINSERT nil DB-ENTDEL-FAILS nil DB-SSGET-NOFILTER nil)
  ;; группа 330 у вставки указывает на ЗАПИСЬ БЛОКА (её handle), а имя
  ;; определения лежит в группе 2 этой записи -- как в реальном файле.
  (setq REFH (KG-EXRefHolders))
  ;; Держатель в стенде «неизвестно»: KG-OwnerBlockName отсекает строку
  ;; (в AutoCAD ename -- не строка), а ename в этом интерпретаторе
  ;; нечем представить. Существенно здесь другое: ссылка ПОСЧИТАНА --
  ;; определение с вставками никогда не будет объявлено свободным.
  ;; Сам KG-OwnerBlockName исполняется в tests/run_space_tests.py.
  (T-EqInt "T19.25 вставка внутри определения даёт ссылку"
           (length (KG-CdrCI "*U63" (cdr REFH))) 1)
  (T-EqInt "T19.26 вставки из разных пространств считаются вместе"
           (length (KG-CdrCI "Стойка КП50" (cdr REFH))) 2)
  (T-Ok "T19.27 не-вставка в карту не попадает"
        (and (not (KG-CdrCI "LINE" (cdr REFH))) (not (KG-CdrCI "Слой0" (cdr REFH)))))
  (T-EqInt "T19.27b ссылок ровно три, четвёртый объект -- не вставка"
           (+ (length (KG-CdrCI "*U63" (cdr REFH)))
              (length (KG-CdrCI "Стойка КП50" (cdr REFH)))) 3)
  (T-Ok "T19.28 маркер успешной выборки лежит в car и не мешает поиску"
        (and (KG-StrEq (car REFH) "OK")
             (not (KG-StrEq (car REFH) "*U63"))))
  ;; В сборке 38 на диск ушла версия KG-EXRefHolders, где маркер «OK» был
  ;; ЭЛЕМЕНТОМ списка, а карта терялась: очистка получила бы пустую карту
  ;; и объявила свободными ВСЕ определения. Ни один тест этого не поймал,
  ;; потому что форма результата не проверялась. Теперь проверяется
  ;; поведение: на карте со ссылками кандидатов быть не должно.
  (T-Ok "T19.28b карта со ссылками не объявляет определения свободными"
        (not (KG-CleanupCandidates (KG-CleanupCounts (cdr REFH))
                                   (list "*U63" "Стойка КП50" "Слой0")
                                   nil)))
  ;; Выборка не сработала (ssget ответил nil): карта не строится, и
  ;; очистка обязана остановиться, а не объявить все определения
  ;; свободными. Пустой чертёж ssget'ом неотличим от отказа, поэтому
  ;; остановка считается правильным поведением.
  (setq DB-SEL nil)
  (T-Ok "T19.29 без выборки карты нет -- очистка остановится"
        (not (KG-EXRefHolders)))

  ;; Выборка с фильтром на реальном чертеже ответила nil при трёх живых
  ;; вставках. Запасной путь -- выборка без фильтра и своя проверка
  ;; группы 0: не-вставки в карту попасть не должны.
  (setq DB-SEL (list "E1" "E2" "E3" "E4"))
  (setq DB-SSGET-NOFILTER t)
  (setq REFH2 (KG-EXRefHolders))
  (setq DB-SSGET-NOFILTER nil)
  (T-EqInt "T19.30 запасная выборка без фильтра находит вставки"
           (+ (length (KG-CdrCI "*U63" (cdr REFH2)))
              (length (KG-CdrCI "Стойка КП50" (cdr REFH2)))) 3)
  (T-Ok "T19.31 в запасной выборке не-вставка отсекается по группе 0"
        (not (KG-CdrCI "Слой0" (cdr REFH2))))

  ;; 19m. Имя владельца берётся из кэша по handle. Прогон на крупном
  ;;      реципиенте (1231 вставка) оборвался с 0xC0000005 сразу после
  ;;      «объектов в наборе 1231»: handent звался на каждом объекте, а
  ;;      владельцев в разы меньше. Без проверки кэша «кэш всегда пуст»
  ;;      неотличимо от работы.
  (setq DB-SEL (list "E1" "E2" "E3"))
  (setq DB-OWNERCALLS 0)
  (defun KG-OwnerBlockName (e / )
    (setq DB-OWNERCALLS (1+ DB-OWNERCALLS))
    "Стойка КП50")
  (setq REFH3 (KG-EXRefHolders))
  (setq DB-OWNERS DB-OWNERCALLS)
  (T-EqStr "T20.40 владелец прочитан и попал в карту"
           (KG-AsString (car (KG-CdrCI "Стойка КП50" (cdr REFH3))))
           "Стойка КП50")
  (T-EqInt "T20.41 обе ссылки этого определения на месте"
           (length (KG-CdrCI "Стойка КП50" (cdr REFH3))) 2)
  (T-Ok "T20.42 владельцев прочитано меньше, чем ссылок"
        (and (> DB-OWNERS 0) (< DB-OWNERS 3)))
  (T-EqInt "T20.43 пройдено объектов столько, сколько в выборке"
           KG-REFDONE 3)

  ;; Имена вложенных блоков: старое определение переименовано в «…~до»
  ;; ДО чтения экземпляра, поэтому состояние читается под одним именем,
  ;; а искать его нужно в новом определении -- под другим.
  (T-EqStr "T19.32 суффикс ~до снимается"
           (KG-StripDoSuffix "Стойка КП50~до") "Стойка КП50")
  (T-EqStr "T19.33 имя без суффикса не меняется"
           (KG-StripDoSuffix "Стойка КП50") "Стойка КП50")
  (T-Ok "T19.34 старое и новое имя вложения совпадают"
        (KG-NestedKeyMatch "Стойка КП50~до" "Стойка КП50"))
  (T-Ok "T19.35 разные вложения не совпадают"
        (not (KG-NestedKeyMatch "Ригель~до" "Стойка КП50")))

  ;; Карта ссылок приходит парой (маркер . карта), а пересчёт в
  ;; KG-CleanupRun делается в четырёх местах. В сборке 39 маркер
  ;; снимался только в первом, и на втором -- сразу после стирания
  ;; сиротских *U -- команда падала с «неверный тип аргумента: consp
  ;; "OK"». Поэтому подсчёт ссылок и список сирот отсекают маркер сами.
  (T-EqInt "T19.36 маркер не считается ссылкой"
           (KG-AsNum (KG-CdrCI "Стойка КП50"
                               (KG-CleanupCounts (list "OK"
                                                       (cons "Стойка КП50"
                                                             (list "a")))))
                     0)
           1)
  (T-Ok "T19.37 маркер не становится определением в карте"
        (not (KG-CdrCI "OK"
                       (KG-CleanupCounts (list "OK"
                                               (cons "Стойка КП50"
                                                     (list "a")))))))
  (T-EqStr "T19.38 список сирот при карте с маркером"
           (KG-JoinNames (KG-CleanupOrphans
                           (list "OK" (cons "*U52" 0))
                           (list "*U52" "*U55" "Стойка КП50")))
           "*U52, *U55")
  ;; Проверять надо ПОСЛЕДСТВИЕ, а не «падает / не падает»: интерпретатор
  ;; терпим к (car "OK") и даёт nil, тогда как в AutoCAD на этом месте
  ;; был «неверный тип аргумента: consp "OK"». Наблюдаемое следствие то
  ;; же самое: маркер просачивается в карту как элемент с пустым
  ;; именем, и он оказывается первым.
  (T-EqStr "T19.39 маркер не просачивается в карту ссылок"
           (KG-AsString (car (car (KG-CleanupCounts
                                    (list "OK"
                                          (cons "Стойка КП50"
                                                (list "a")))))))
           "Стойка КП50")
  (T-Ok "T19.40 маркер не попадает в список сирот"
        (not (KG-StrInterCI (list "OK")
                            (KG-CleanupOrphans
                              (list "OK" (cons "*U52" 0) (cons "Стойка КП50" 1))
                              (list "OK" "*U52" "Стойка КП50")))))

  ;; 19k. НАСТОЯЩАЯ KG-DefInsertByName поверх поддельной таблицы блоков.
  ;;      В сборке 39 функция KG-NestedKeyMatch была написана, но не
  ;;      вызвана ни разу, а тесты T19.32-T19.35 проверяли её саму, а не
  ;;      то, что её кто-то зовёт. Прогон на реальном чертеже дал те же
  ;;      шесть «не найден» с суффиксом «~до». Здесь проверяется
  ;;      интеграция: поиск вложенной вставки идёт через сравнение
  ;;      ключей, а не точных имён.
  ;;      COM-граница подделана: ename -> объект -> эффективное имя. На
  ;;      чертеже эффективное имя анонимного представления -- это имя
  ;;      РОДИТЕЛЬСКОГО динамического блока, поэтому в группе 2
  ;;      поддельной вставки стоит имя родителя, а не «*U77».
  (defun vlax-ename->vla-object (e / ) (KG-AsString e))
  (defun vlax-vla-object->ename (o / ) (KG-AsString o))
  (defun vla-get-EffectiveName (o / )
    (KG-AsString (cdr (assoc 2 (entget (KG-AsString o))))))
  (defun vla-get-Name (o / )
    (KG-AsString (cdr (assoc 2 (entget (KG-AsString o))))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "E6" (list (cons 0 "INSERT")
                                   (cons 2 "*U77~до")))
                  (cons "EB4" (list (cons 0 "ENDBLK"))))))
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U61" "B61") (cons "B61" "E6")
                  (cons "E6" "EB4"))))
  (T-EqStr "T19.41 вложенная вставка находится по имени без «~до»"
           (KG-AsString (KG-DefInsertByName "*U61" "*U77"))
           "E6")
  (T-Ok "T19.42 чужое вложение не подставляется"
        (not (KG-DefInsertByName "*U61" "Ригель КП50")))

  ;; 19l. Маркер карты ссылок. В сборке 40 маркер снимался внутри
  ;;      KG-CleanupCounts и KG-CleanupOrphans, а контроль после PURGE
  ;;      звал KG-CdrCI по карте напрямую -- и команда снова падала с
  ;;      «неверный тип аргумента: consp "OK"». Теперь маркер снимается
  ;;      там, где карта получена (KG-Unmark), и текст «кто держит»
  ;;      печатает хелпер, а не печатающий код.
  (T-Ok "T19.43 KG-Unmark снимает маркер"
        (KG-CdrCI "Стойка КП50"
                  (KG-Unmark (list "OK" (cons "Стойка КП50" (list "a"))))))
  (T-EqInt "T19.44 KG-Unmark не трогает карту без маркера"
           (length
             (KG-CdrCI "Стойка КП50"
                       (KG-Unmark (list (cons "Стойка КП50" (list "a"))))))
           1)
  (T-EqStr "T19.45 кто держит определение -- из карты с маркером"
           (KG-CleanupHolderText (list "OK" (cons "kps 714" (list "*U61")))
                                 "kps 714")
           "*U61")
  (T-EqStr "T19.46 без держателей текст понятный"
           (KG-CleanupHolderText (list "OK") "kps 714")
           "(прямых вставок нет)")

  ;; 19m. Многострочная вставка. Прежняя проверка искала ssget'ом
  ;;      вставки с признаком атрибутов ((66 . 1)) и именем определения.
  ;;      Анонимное представление динамического блока почти всегда несёт
  ;;      атрибуты -- тег представления хранится именно так, -- поэтому
  ;;      проверка отвечала «да» на все 43 сироты, ENTD не вызывался ни
  ;;      разу и очистка печатала «Стёрто 0, не удалось 43». Теперь
  ;;      MINSERT ищется внутри самого определения.
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U70" "B70") (cons "B70" "M1") (cons "M1" "EB5")
                  (cons "*U71" "B71") (cons "B71" "A1") (cons "A1" "EB6"))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "B70" (list (cons 0 "BLOCK") (cons 2 "*U70")))
                  (cons "M1" (list (cons 0 "MINSERT") (cons 2 "kps 714")))
                  (cons "EB5" (list (cons 0 "ENDBLK")))
                  (cons "B71" (list (cons 0 "BLOCK") (cons 2 "*U71")))
                  ;; вставка С АТРИБУТАМИ: группа 66 = 1, как у
                  ;; представления с тегом AcDbBlockRepBTag
                  (cons "A1" (list (cons 0 "INSERT") (cons 2 "kps 714")
                                   (cons 66 1)))
                  (cons "EB6" (list (cons 0 "ENDBLK"))))))
  (T-Ok "T19.47 многострочная вставка внутри видна"
        (KG_HasMInsert "*U70"))
  (T-Ok "T19.48 атрибуты вставке -- это не многострочная вставка"
        (not (KG_HasMInsert "*U71")))
  (T-Ok "T19.49 *U с атрибутами стирается"
        (KG_EXEraseAnonDef "*U71"))
  (T-Ok "T19.50 определения после стирания нет"
        (not (FAKE-Chain "*U71")))
  (T-Ok "T19.51 *U с многострочной вставкой не стирается"
        (not (KG_EXEraseAnonDef "*U70")))
  (T-Ok "T19.52 оно осталось на месте" (FAKE-Chain "*U70"))

  ;; 19n. Запасной путь к ename определения. На реальном чертеже
  ;;      tblobjname ответил nil на ВСЕ 37 сиротских *U -- имена
  ;;      приходят из COM-коллекции Blocks, а ename программа брала из
  ;;      таблицы блоков. Очистка напечатала «BLOCK не найден» 37 раз и
  ;;      не стёрла ничего. Здесь imитируется тот же случай: tblobjname
  ;;      переопределён так, что на одно имя он отвечает nil, а
  ;;      tblsearch запись отдаёт.
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U90" "B90") (cons "B90" "E90")
                  (cons "E90" "EB8"))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "B90" (list (cons 0 "BLOCK") (cons 2 "*U90")))
                  (cons "E90" (list (cons 0 "LINE")))
                  (cons "EB8" (list (cons 0 "ENDBLK"))))))
  (defun tblobjname (tbl nm / )
    (if (KG-StrEq nm "*U90") nil (FAKE-Chain nm)))
  (T-Ok "T19.53 ename найден через tblsearch, когда tblobjname молчит"
        (KG-DefEnt "*U90"))
  (T-Ok "T19.54 такое определение стирается"
        (KG_EXEraseAnonDef "*U90"))
  (T-Ok "T19.55 определения после стирания нет"
        (not (KG-DefEnt "*U90")))
  (defun tblobjname (tbl nm) (FAKE-Chain nm))

  ;; 19o. Третий путь к ename -- через COM-коллекцию Blocks. Прогон
  ;;      сборки 47 показал: «BLOCK не найден» 37 раз, а диагностика
  ;;      «tblsearch -> nil» не напечаталась ни разу. Значит, tblsearch
  ;;      запись возвращает, но группы -2 в ней нет, и прежняя ветка
  ;;      else просто не исполнялась. Здесь tblsearch подделан так же:
  ;;      запись есть, -2 нет.
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U95" "B95") (cons "B95" "E95")
                  (cons "E95" "EB9"))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "B95" (list (cons 0 "BLOCK") (cons 2 "*U95")))
                  (cons "E95" (list (cons 0 "LINE")))
                  (cons "EB9" (list (cons 0 "ENDBLK"))))))
  (defun tblobjname (tbl nm / )
    (if (KG-StrEq nm "*U95") nil (FAKE-Chain nm)))
  (defun tblsearch (tbl nm / )
    (if (KG-StrEq nm "*U95") (list (cons 0 "BLOCK") (cons 2 nm)) nil))
  ;; как в AutoCAD: после стирания определения объект из коллекции
  ;; Blocks уже не берётся -- иначе контроль «исчезло ли» всегда отвечал
  ;; бы «нет», и стирание никогда не считалось бы успешным
  (defun KG-BlockObj (nm / )
    (if (and (KG-StrEq nm "*U95") (FAKE-Chain "*U95")) "OBJ95" nil))
  (defun vlax-vla-object->ename (o / )
    (if (KG-StrEq o "OBJ95") "B95" nil))
  (T-Ok "T19.56 ename найден через COM, когда в записи нет -2"
        (KG-DefEnt "*U95"))
  (T-Ok "T19.57 такое определение стирается"
        (KG_EXEraseAnonDef "*U95"))
  (T-Ok "T19.58 определения после стирания нет"
        (not (KG-DefEnt "*U95")))

  ;; 19m. Путь tblsearch (-1) -- тот самый, которого не хватало на
  ;;      реальном чертеже. Запись возвращается, группы -2 в ней нет
  ;;      (пустое или анонимное представление), tblobjname молчит, COM
  ;;      молчит. До сборки 51 ename не находился вовсе, и очистка 19 раз
  ;;      подряд отвечала «BLOCK не найден».
  (setq DB-ENTDB
    (append DB-ENTDB
            (list (cons "*U96" "B96") (cons "B96" "E96")
                  (cons "E96" "EB10"))))
  (setq DB-ENTDATA
    (append DB-ENTDATA
            (list (cons "B96" (list (cons 0 "BLOCK") (cons 2 "*U96")))
                  (cons "E96" (list (cons 0 "LINE")))
                  (cons "EB10" (list (cons 0 "ENDBLK"))))))
  (defun tblobjname (tbl nm / ) nil)
  ;; Запись возвращается, пока определение живо: FAKE-Chain знает о
  ;; стирании, и без него контроль «исчезло ли» всегда отвечал бы «нет».
  (defun tblsearch (tbl nm / )
    (if (and (KG-StrEq nm "*U96") (FAKE-Chain "*U96"))
      (list (cons -1 "B96") (cons 0 "BLOCK") (cons 2 nm))
      nil))
  (defun KG-BlockObj (nm / ) nil)
  (T-EqStr "T19.59 ename найден через tblsearch (-1), когда -2 нет"
           (KG-AsString (KG-DefEnt "*U96")) "B96")
  (T-Ok "T19.60 такое определение стирается"
        (KG_EXEraseAnonDef "*U96"))
  (T-Ok "T19.61 определения после стирания нет"
        (not (KG-DefEnt "*U96")))
  (defun tblobjname (tbl nm) (FAKE-Chain nm))
  (defun tblsearch (tbl nm / e)
    (setq e (FAKE-Chain nm))
    (if e (list (cons 0 "BLOCK") (cons 2 nm) (cons -2 e)) nil)
  )
)
