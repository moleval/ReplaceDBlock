;;;===========================================================================
;;; check-substr-space.lsp -- точечная проба на стенде (testbed).
;;;
;;; ЧТО ПРОВЕРЯЕТ
;;;
;;; A. substr на краях диапазона. Живой прогон в AutoCAD дал ответ:
;;;    start=0 и start<0 -- жёсткая ошибка «неверное значение аргумента»,
;;;    length=0 -- пустая строка, безопасно. Прежний KG-ParseParenName
;;;    звал substr с началом 0 на именах вида "12)" и падал; теперь такой
;;;    вызов не делается -- разбор честно возвращает nil. Проба печатает
;;;    и края substr, и разбор имён-случаев.
;;;
;;; B. Сценарий с пространством листа. Прогон подтвердил дефект:
;;;    KG-LayoutNameFor("блок листа") всегда возвращал имя блока, и
;;;    KG_EXCreateInstance по такому имени уводил НОВЫЙ экземпляр в
;;;    МОДЕЛЬ. После правки KG-LayoutNameFor ищет лист по имени блока,
;;;    а KG_EXCreateInstance не подставляет МОДЕЛЬ: экземпляр просто не
;;;    создаётся. Проба печатает звенья цепочки и ожидает отказ.
;;;
;;; КАК ЗАПУСКАТЬ
;;;   1. Открыть AutoCAD с нужным чертежом.
;;;   2. (load "D:/ReplaceDBlock/RepDblock.lsp")            ; путь под себя
;;;      (load "D:/ReplaceDBlock/testbed/check-substr-space.lsp")
;;;   3. CHKSUBSTR    -- только чтение.
;;;      CHKSPACE     -- только чтение; оценка риска для ЭТОГО чертежа.
;;;      CHKSPACELIVE -- ЖИВАЯ ПРОБА: создаёт временные вхождения и сам их
;;;                      удаляет; запускать на КОПИИ чертежа.
;;;   Или сразу: SCRIPT -> testbed/check-substr-space.scr
;;;
;;; ЧТО ПРИСЛАТЬ: весь вывод команд целиком, до строки "Готово".
;;;===========================================================================

(vl-load-com)

(setq CHK-VERSION "1")

;; ---------------------------------------------------------------------------
;; Печать и мелкие помощники
;; ---------------------------------------------------------------------------

(defun CHK-Say (s) (princ (strcat "\n" s)))

(defun CHK-KV (k v) (princ (strcat "\n" k ": " v)))

;; Выполнить и вернуть список (признак-успеха значение|сообщение об ошибке)
(defun CHK-Call (fn / r)
  (setq r (vl-catch-all-apply fn))
  (if (vl-catch-all-error-p r)
    (list nil (vl-catch-all-error-message r))
    (list t r)
  )
)

;; Результат CHK-Call строкой для печати
(defun CHK-ResStr (r)
  (if (car r)
    (strcat "значение " (CHK-PrStr (car (cdr r))))
    (strcat "ОШИБКА: " (car (cdr r)))
  )
)

;; Значение строкой: строки -- в кавычках, пустой результат виден как ""
(defun CHK-PrStr (v)
  (if (= (type v) (type ""))
    (strcat "\"" v "\"")
    (vl-princ-to-string v)
  )
)

;; Загружен ли RepDblock.lsp. Через boundp проверять нельзя: в AutoCAD
;; символ со значением nil «не имеет значения» (KG-TESTING в обычном
;; режиме как раз nil), и проверка ложно говорит «не загружен». Поэтому
;; пробный вызов лёгкой чистой функции: если её нет -- vl-catch-all-apply
;; поймает ошибку.
(defun CHK-Loaded ()
  (not (vl-catch-all-error-p
         (vl-catch-all-apply '(lambda () (KG-AsString "проба")))))
)

;; Счётчик в ассоциативном списке ("ключ" . число)
(defun CHK-Inc (k al / p)
  (setq p (assoc k al))
  (if p
    (subst (cons k (1+ (cdr p))) p al)
    (cons (cons k 1) al)
  )
)

;; Ассоциативный список с заменённым/добавленным ключом
(defun CHK-Put (k v al)
  (cons (cons k v) (vl-remove (assoc k al) al))
)

;; ---------------------------------------------------------------------------
;; A. substr на краях и имена-случаи парсера
;; ---------------------------------------------------------------------------

(defun CHK-DoSubstrCases ( / c)
  (CHK-Say "")
  (CHK-Say "1. ЗНАЧЕНИЯ substr НА КРАЯХ")
  (foreach c
    (list
      (cons
        "(substr \"abc\" 0 1)"
        '(lambda () (substr "abc" 0 1)))
      (cons
        "(substr \"abc\" 0)"
        '(lambda () (substr "abc" 0)))
      (cons
        "(substr \"abc\" 1 0)"
        '(lambda () (substr "abc" 1 0)))
      (cons
        "(substr \"abc\" -1 2)"
        '(lambda () (substr "abc" -1 2)))
      (cons
        "(substr \"abc\" 4 1)"
        '(lambda () (substr "abc" 4 1)))
      (cons
        "(substr \"abc\" 5)"
        '(lambda () (substr "abc" 5)))
      (cons
        "(substr \"abc\" 2 99)"
        '(lambda () (substr "abc" 2 99)))
      (cons
        "(substr \"12)\" 0 1)   <- боевой вызов при имени \"12)\""
        '(lambda () (substr "12)" 0 1)))
      (cons
        "(substr \"12)\" 4)"
        '(lambda () (substr "12)" 4)))
      (cons
        "(substr \"(15)Стойка\" 1 0)   <- боевой вызов при имени \"(15)Стойка\""
        '(lambda () (substr "(15)Стойка" 1 0)))
    )
    (CHK-Say (strcat "   " (car c) "  ->  " (CHK-ResStr (CHK-Call (cdr c)))))
  )
)

(defun CHK-DoParserCases ( / nm)
  (CHK-Say "")
  (CHK-Say "2. ИМЕНА-СЛУЧАИ ЧЕРЕЗ ПАРСЕР")
  (if (not (CHK-Loaded))
    (CHK-Say "   RepDblock.lsp не загружен -- проверка парсера пропущена")
    (progn
      (foreach nm '("12)" "0)" "1) Схема" "12)Стойка" "(15)" "(15)Стойка" "AB(15)Стойка" "Стойка (15)")
        (CHK-Say
          (strcat "   KG-ParseParenName \"" nm "\"  ->  "
                  (CHK-ResStr (CHK-Call '(lambda () (KG-ParseParenName nm))))))
      )
      (foreach nm '("12)" "1) Схема" "12)Стойка" "(15)Стойка" "Стойка")
        (CHK-Say
          (strcat "   KG-ParseBlockName \"" nm "\"  ->  "
                  (CHK-ResStr (CHK-Call '(lambda () (KG-ParseBlockName nm))))))
      )
    )
  )
)

(defun CHK-SubstrVerdicts ( / r0 r1 p1 p2)
  (CHK-Say "")
  (CHK-Say "3. СВЯЗКА: КРАЙ substr -> ПАДЕНИЕ ПОТРЕБИТЕЛЯ")
  (setq r0 (CHK-Call '(lambda () (substr "12)" 0 1))))
  (setq r1 (CHK-Call '(lambda () (substr "(15)Стойка" 1 0))))
  (CHK-Say
    (strcat "   substr со start=0  :  "
            (if (car r0)
              (strcat "допускается, результат " (vl-princ-to-string (car (cdr r0))))
              (strcat "ОШИБКА: " (car (cdr r0))))))
  (CHK-Say
    (strcat "   substr с length=0  :  "
            (if (car r1)
              (strcat "допускается, результат " (vl-princ-to-string (car (cdr r1))))
              (strcat "ОШИБКА: " (car (cdr r1))))))
  (if (not (CHK-Loaded))
    (CHK-Say "   парсер не проверялся (RepDblock.lsp не загружен)")
    (progn
      (setq p1 (CHK-Call '(lambda () (KG-ParseParenName "12)"))))
      (setq p2 (CHK-Call '(lambda () (KG-ParseParenName "(15)Стойка"))))
      (CHK-Say (strcat "   KG-ParseParenName \"12)\"        -> " (CHK-ResStr p1)))
      (CHK-Say (strcat "   KG-ParseParenName \"(15)Стойка\" -> " (CHK-ResStr p2)))
      (cond
        ((and (not (car r0)) (not (car p1)))
         (CHK-Say "   ВЫВОД 1: substr со start=0 ЗАПРЕЩЁН, и имена вида \"12)\" РОНЯЮТ парсер."))
        ((and (car r0) (car p1))
         (CHK-Say "   ВЫВОД 1: substr со start=0 допускается; парсер имена вида \"12)\" не роняет."))
        ((and (not (car r0)) (car p1))
         (CHK-Say "   ВЫВОД 1: substr со start=0 -- ошибка, но KG-ParseParenName такой вызов больше не делает (защита стоит до substr) -- вернулось nil."))
        (t
         (CHK-Say "   ВЫВОД 1: неожиданная пара -- пришлите вывод целиком."))
      )
      (cond
        ((and (not (car r1)) (not (car p2)))
         (CHK-Say "   ВЫВОД 2: substr с length=0 ЗАПРЕЩЁН, и имена вида \"(15)...\" РОНЯЮТ парсер."))
        ((and (car r1) (car p2))
         (CHK-Say "   ВЫВОД 2: substr с length=0 допускается; парсер имена вида \"(15)...\" не роняет (пустая база даёт nil, дальше простая схема)."))
        ((and (not (car r1)) (car p2))
         (CHK-Say "   ВЫВОД 2: substr с length=0 -- ошибка, но парсер \"(15)Стойка\" выжил: этот путь до боевого вызова не доходит."))
        (t
         (CHK-Say "   ВЫВОД 2: неожиданная пара -- пришлите вывод целиком."))
      )
    )
  )
)

(defun C:CHKSUBSTR ()
  (CHK-Say (strcat "=== CHKSUBSTR, версия " CHK-VERSION " ==="))
  (CHK-Say "Чертёж не изменяется.")
  (CHK-Say (strcat "RepDblock.lsp: "
                   (if (CHK-Loaded) "загружен" "НЕ загружен -- разделы 2 и 3 пропускаются.")))
  (CHK-DoSubstrCases)
  (CHK-DoParserCases)
  (CHK-SubstrVerdicts)
  (CHK-Say "")
  (CHK-Say "Готово. Пришлите вывод целиком.")
  (princ)
)

;; ---------------------------------------------------------------------------
;; B. Пространство листа -- только чтение
;; ---------------------------------------------------------------------------

(defun CHK-PickDef ( / ss i e ed d out)
  (setq out nil)
  (setq ss (vl-catch-all-apply '(lambda () (ssget "_X" '((0 . "INSERT"))))))
  (if (not (vl-catch-all-error-p ss))
    (if ss
      (progn
        (setq i (sslength ss))
        (while (and (> i 0) (null out))
          (setq i (1- i))
          (setq e (ssname ss i))
          (setq ed (vl-catch-all-apply '(lambda () (entget e))))
          (if (and (not (vl-catch-all-error-p ed)) ed)
            (progn
              (setq d (cdr (assoc 2 ed)))
              (if (and d (not (wcmatch (strcase d) "`**")))
                (setq out d)
              )
            )
          )
        )
      )
    )
  )
  out
)

(defun C:CHKSPACE ( / *error* doc lays lay bnm lname r r2 map old-map byspace c p
                      ss i e ed h oh spv tot n330 nmiss cand cand-h cand-lay ob iss)
  (defun *error* (m)
    (if (and m (/= m "Function cancelled") (/= m "quit / exit abort"))
      (princ (strcat "\nОШИБКА КОМАНДЫ: " m)))
    (princ)
  )
  (CHK-Say (strcat "=== CHKSPACE, версия " CHK-VERSION " ==="))
  (CHK-Say "Сценарий с пространством листа. Чертёж не изменяется.")
  (CHK-Say (strcat "Чертёж: " (getvar "DWGNAME")))
  (if (not (and (CHK-Loaded) (null KG-TESTING)))
    (CHK-Say (strcat "Сначала загрузите RepDblock.lsp при KG-TESTING = nil: нужен раздел 8. "
                     (if (CHK-Loaded)
                       "(Он загружен, но KG-TESTING не nil: выполните (setq KG-TESTING nil) и загрузите заново.)"
                       "(В этой сессии AutoCAD он не загружен.)")))
    (progn
      (setq old-map KG-SPACE-MAP)
      (setq map (CHK-Call '(lambda () (KG-BuildSpaceMap))))
      (if (not (car map))
        (CHK-Say (strcat "Карта пространств не построилась: " (car (cdr map))))
        (progn
          (setq KG-SPACE-MAP (car (cdr map)))
          (setq iss nil)

          (CHK-Say "")
          (CHK-Say "1. КАРТА ПРОСТРАНСТВ (handle -> пространство)")
          (CHK-KV "   объектов в карте" (itoa (length KG-SPACE-MAP)))
          (foreach p KG-SPACE-MAP
            (setq byspace (CHK-Inc (KG-AsString (cdr p)) byspace))
          )
          (if (null byspace)
            (CHK-Say "   карта пуста -- вхождений нет или таблица блоков не читается")
            (foreach c (reverse byspace)
              (CHK-KV (strcat "   «" (car c) "»") (itoa (cdr c)))
              (if (wcmatch (strcase (car c)) "`*PAPER_SPACE*")
                (setq iss
                  (cons (strcat "в карте пространство «" (car c)
                                "» -- это ИМЯ БЛОКА, а не имя листа")
                        iss))
              )
            )
          )

          (CHK-Say "")
          (CHK-Say "2. ЛИСТЫ: имя блока -> имя листа (KG-LayoutNameFor)")
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
          (setq lays (CHK-Call '(lambda () (vla-get-Layouts doc))))
          (if (not (car lays))
            (CHK-Say (strcat "   коллекцию листов прочитать не удалось: " (car (cdr lays))))
            (vlax-for lay (car (cdr lays))
              (setq bnm (CHK-Call '(lambda () (vla-get-Name (vla-get-Block lay)))))
              (if (and (car bnm) (= (type (car (cdr bnm))) (type ""))
                       (wcmatch (strcase (car (cdr bnm))) "`*PAPER_SPACE*"))
                (progn
                  (setq bnm (car (cdr bnm)))
                  (setq lname (CHK-Call '(lambda () (vla-get-Name lay))))
                  (setq lname (if (and (car lname) (= (type (car (cdr lname))) (type "")))
                                (car (cdr lname))
                                "?"))
                  (CHK-Say (strcat "   лист «" lname "», блок «" bnm "»:"))
                  (setq r (CHK-Call '(lambda () (KG-LayoutNameFor bnm))))
                  (CHK-Say (strcat "      KG-LayoutNameFor -> " (CHK-ResStr r)))
                  (cond
                    ((and (car r) (KG-StrEq (car (cdr r)) bnm) (not (KG-StrEq bnm lname)))
                     (CHK-Say "      ДЕФЕКТ: возвращено имя блока, а не имя листа")
                     (setq iss
                       (cons (strcat "KG-LayoutNameFor(\"" bnm "\") = \""
                                     (car (cdr r)) "\" (имя блока вместо листа «" lname "»)")
                             iss)))
                    ((and (car r) (KG-StrEq (car (cdr r)) lname))
                     (CHK-Say "      верно: имя листа"))
                    ((not (car r))
                     (CHK-Say "      вызов упал"))
                    (t
                     (CHK-Say (strcat "      неожиданный результат: " (CHK-ResStr r))))
                  )
                  (setq r (CHK-Call '(lambda () (KG-LayoutBlockForSpace lname))))
                  (CHK-Say
                    (strcat "      KG-LayoutBlockForSpace(\"" lname "\") -> "
                            (if (not (car r))
                              (strcat "ОШИБКА: " (car (cdr r)))
                              (if (car (cdr r))
                                "блок листа найден"
                                "nil (ДЕФЕКТ: лист не находится)"))))
                  (if (and (car r) (null (car (cdr r))))
                    (setq iss
                      (cons (strcat "KG-LayoutBlockForSpace(\"" lname
                                    "\") = nil -- лист не находится по имени")
                            iss))
                  )
                  (setq r (CHK-Call '(lambda () (KG-LayoutBlockForSpace bnm))))
                  (CHK-Say
                    (strcat "      KG-LayoutBlockForSpace(\"" bnm "\") -> "
                            (if (not (car r))
                              (strcat "ОШИБКА: " (car (cdr r)))
                              (if (car (cdr r))
                                "блок найден (не ожидалось)"
                                "nil -- ожидаемо: имя блока не имя листа"))))
                )
              )
            )
          )

          (CHK-Say "")
          (CHK-Say "3. ВХОЖДЕНИЯ: покрытие картой и группа 330")
          (CHK-Say "   (на большом чертеже считает заметное время)")
          (setq ss (CHK-Call '(lambda () (ssget "_X" '((0 . "INSERT"))))))
          (if (not (car ss))
            (CHK-Say (strcat "   ssget упал: " (car (cdr ss))))
            (progn
              (setq ss (car (cdr ss)) tot 0 n330 0 nmiss 0 cand nil cand-h nil cand-lay nil)
              (if ss
                (progn
                  (setq i (sslength ss))
                  (while (> i 0)
                    (setq i (1- i))
                    (setq e (ssname ss i))
                    (setq ed (CHK-Call '(lambda () (entget e))))
                    (if (car ed)
                      (progn
                        (setq tot (1+ tot))
                        (setq ed (car (cdr ed)))
                        (setq h (KG-AsString (cdr (assoc 5 ed))))
                        (setq oh (cdr (assoc 330 ed)))
                        (if oh (setq n330 (1+ n330)))
                        (setq spv (KG-SpaceOfHandle h))
                        (if (null spv)
                          (setq nmiss (1+ nmiss))
                          (if (and (null cand) oh (not (KG-StrEq spv "Model")))
                            (progn
                              (setq cand e)
                              (setq cand-h h)
                              (setq cand-lay spv)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              (CHK-KV "   вхождений всего" (itoa tot))
              (CHK-KV "   из них с группой 330" (itoa n330))
              (CHK-KV "   не найдено в карте" (itoa nmiss))
              (cond
                ((= tot 0)
                 (CHK-Say "   вхождений нет -- сценарий не на чем проверять"))
                ((and (= nmiss 0) (= n330 0))
                 (CHK-Say "   ОЦЕНКА: карта покрывает все вхождения, группы 330 ни у кого нет --")
                 (CHK-Say "   на этом чертеже опасная ветка недостижима"))
                ((= nmiss 0)
                 (CHK-Say
                   (strcat "   ОЦЕНКА: карта покрывает все вхождения; группа 330 есть у "
                           (itoa n330)
                           " вхождений -- запасной путь включится только при промахе карты")))
                (t
                 (CHK-Say
                   (strcat "   ОЦЕНКА: ВНИМАНИЕ, " (itoa nmiss)
                           " вхождений не в карте -- на них включится запасной путь"))
                 (setq iss (cons (strcat "не найдено в карте вхождений: " (itoa nmiss)) iss)))
              )

              (CHK-Say "")
              (CHK-Say "4. СИМУЛЯЦИЯ ЗАПАСНОГО ПУТИ (только чтение)")
              (if (null cand)
                (CHK-Say "   образец (вхождение на листе с группой 330) не найден -- цепочку проверит CHKSPACELIVE")
                (progn
                  (CHK-Say (strcat "   образец: handle " cand-h ", лист «" cand-lay "»"))
                  (setq ob (CHK-Call '(lambda () (KG-OwnerBlockName cand))))
                  (CHK-Say (strcat "   группа 330 -> запись блока: " (CHK-ResStr ob)))
                  (setq KG-SPACE-MAP nil)
                  (setq r (CHK-Call '(lambda () (KG-SpaceOf cand))))
                  (CHK-Say (strcat "   KG-SpaceOf при пустой карте -> " (CHK-ResStr r)))
                  (if (and (car r) (= (type (car (cdr r))) (type "")))
                    (progn
                      (setq r2 (CHK-Call '(lambda () (KG-LayoutBlockForSpace (car (cdr r))))))
                      (CHK-Say
                        (strcat "   KG-LayoutBlockForSpace(\"" (car (cdr r)) "\") -> "
                                (if (not (car r2))
                                  (strcat "ОШИБКА: " (car (cdr r2)))
                                  (if (car (cdr r2))
                                    "блок найден"
                                    "nil -- лист по этому имени не находится"))))
                      (if (and (car ob) (= (type (car (cdr ob))) (type ""))
                               (KG-StrEq (car (cdr r)) (car (cdr ob)))
                               (wcmatch (strcase (car (cdr r))) "`*PAPER_SPACE*"))
                        (progn
                          (CHK-Say "   ДЕФЕКТ ПОДТВЕРЖДЁН: запасной путь вернул имя блока вместо имени листа,")
                          (CHK-Say "   и KG_EXCreateInstance увёл бы пересозданный экземпляр в МОДЕЛЬ (ветка «owner = nil»)")
                          (setq iss
                            (cons "запасной путь KG-SpaceOf отдаёт имя блока; KG_EXCreateInstance уводит экземпляр в МОДЕЛЬ"
                                  iss)))
                        (CHK-Say (if (and (car r2) (car (cdr r2)))
                          (strcat "   ИТОГ: запасной путь вернул «" (car (cdr r))
                                  "», а это имя существующего листа -- правильно.")
                          "   результат не подошёл под сценарий дефекта -- см. значения выше"))
                      )
                    )
                    (CHK-Say "   запасной путь вернул nil -- безопасный отказ")
                  )
                )
              )
            )
          )

          (setq KG-SPACE-MAP old-map)

          (CHK-Say "")
          (CHK-Say "5. ИТОГ")
          (if iss
            (progn
              (CHK-KV "   проблем найдено" (itoa (length iss)))
              (foreach c (reverse iss) (CHK-Say (strcat "   - " c)))
            )
            (CHK-Say "   проблем не найдено (по этому чертежу)")
          )
          (CHK-Say "")
          (CHK-Say "Готово. Пришлите вывод целиком.")
        )
      )
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; C. Пространство листа -- живая проба (создаёт и удаляет временные
;;    вхождения; запускать только на копии чертежа)
;; ---------------------------------------------------------------------------

(defun CHK-LiveCleanup ( / h e n)
  (setq n 0)
  (foreach h (list CHK-H1 CHK-H2)
    (if h
      (progn
        (setq e (vl-catch-all-apply '(lambda () (handent (KG-AsString h)))))
        (if (and (not (vl-catch-all-error-p e)) e)
          (progn
            (vl-catch-all-apply '(lambda () (entdel e)))
            (setq n (1+ n))
          )
        )
      )
    )
  )
  (setq CHK-H1 nil CHK-H2 nil)
  (if CHK-CLAYER
    (vl-catch-all-apply '(lambda () (setvar "CLAYER" CHK-CLAYER))))
  (vl-catch-all-apply '(lambda () (KG-RefreshSpaceMap)))
  (CHK-Say (strcat "Уборка: удалено временных вхождений: " (itoa n)))
)

(defun C:CHKSPACELIVE ( / *error* doc lays lay nm blk bnm lname def
                          i1 e1 obj1 spA ownerA mA spB h2 r)
  (defun *error* (m)
    (if (and m (/= m "Function cancelled") (/= m "quit / exit abort"))
      (princ (strcat "\nОШИБКА КОМАНДЫ: " m)))
    (CHK-LiveCleanup)
    (princ)
  )
  (setq CHK-H1 nil CHK-H2 nil CHK-CLAYER nil)
  (CHK-Say (strcat "=== CHKSPACELIVE, версия " CHK-VERSION " ==="))
  (CHK-Say "ЖИВАЯ ПРОБА. Чертёж меняется: создаются временные вхождения, затем удаляются.")
  (CHK-Say "Запускайте на КОПИИ чертежа.")
  (if (not (and (CHK-Loaded) (null KG-TESTING)))
    (CHK-Say (strcat "Сначала загрузите RepDblock.lsp при KG-TESTING = nil: нужны функции раздела 8. "
                     (if (CHK-Loaded)
                       "(Он загружен, но KG-TESTING не nil: выполните (setq KG-TESTING nil) и загрузите заново.)"
                       "(В этой сессии AutoCAD он не загружен.)")))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

      ;; 1. Первый лист
      (setq lays (vl-catch-all-apply '(lambda () (vla-get-Layouts doc))))
      (setq blk nil lname nil bnm nil)
      (if (not (vl-catch-all-error-p lays))
        (vlax-for lay lays
          (if (null blk)
            (progn
              (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name (vla-get-Block lay)))))
              (if (and (not (vl-catch-all-error-p nm)) nm
                       (wcmatch (strcase nm) "`*PAPER_SPACE*"))
                (progn
                  (setq blk (vl-catch-all-apply '(lambda () (vla-get-Block lay))))
                  (if (vl-catch-all-error-p blk) (setq blk nil))
                  (if blk
                    (progn
                      (setq bnm nm)
                      (setq lname (vl-catch-all-apply '(lambda () (vla-get-Name lay))))
                      (if (vl-catch-all-error-p lname) (setq lname "?"))
                    )
                  )
                )
              )
            )
          )
        )
      )

      (if (null blk)
        (CHK-Say "В чертеже нет листов (только модель) -- проба невозможна.")
        (progn
          (CHK-Say (strcat "Лист для пробы: «" lname "» (блок «" bnm "»)"))

          ;; 2. Определение
          (setq def (CHK-PickDef))
          (if (null def)
            (CHK-Say "Не нашлось определения (вхождений с обычным именем нет) -- проба невозможна.")
            (progn
              (CHK-Say (strcat "Определение для пробы: «" def "»"))
              (setq CHK-CLAYER (getvar "CLAYER"))

              ;; 3. Пробная вставка в лист
              (setq i1
                (CHK-Call
                  '(lambda ()
                     (vla-InsertBlock blk (vlax-3d-point (list 0.0 0.0 0.0))
                                      def 1.0 1.0 1.0 0.0))))
              (if (not (car i1))
                (progn
                  ;; слой может быть заперт -- повторить на слое "0"
                  (vl-catch-all-apply '(lambda () (setvar "CLAYER" "0")))
                  (setq i1
                    (CHK-Call
                      '(lambda ()
                         (vla-InsertBlock blk (vlax-3d-point (list 0.0 0.0 0.0))
                                          def 1.0 1.0 1.0 0.0))))
                )
              )
              (if (not (car i1))
                (CHK-Say (strcat "Пробную вставку сделать не удалось: " (car (cdr i1))))
                (progn
                  (setq e1 (vlax-vla-object->ename (car (cdr i1))))
                  (setq CHK-H1 (CHK-Call '(lambda () (cdr (assoc 5 (entget e1))))))
                  (setq CHK-H1 (if (car CHK-H1) (KG-AsString (car (cdr CHK-H1))) nil))
                  (setq obj1 (vlax-ename->vla-object e1))
                  (CHK-Say (strcat "1. Пробное вхождение в лист: handle "
                                   (if CHK-H1 CHK-H1 "не прочитался")))

                  ;; контроль: карта
                  (vl-catch-all-apply '(lambda () (KG-RefreshSpaceMap)))
                  (setq r (KG-SpaceOfHandle CHK-H1))
                  (CHK-Say (strcat "   контроль по карте: «"
                                   (if r r "не найдено")
                                   "» (ожидалось «" lname "»)"))

                  ;; 4. Запасной путь при пустой карте
                  (setq KG-SPACE-MAP nil)
                  (setq spA (CHK-Call '(lambda () (KG-SpaceOf e1))))
                  (setq ownerA (CHK-Call '(lambda () (KG-OwnerBlockName e1))))
                  (CHK-Say "2. Запасной путь (карта пуста, как при промахе):")
                  (CHK-Say (strcat "   группа 330 -> "
                                   (if (and (car ownerA) (car (cdr ownerA)))
                                     (car (cdr ownerA))
                                     "отсутствует")))
                  (CHK-Say (strcat "   KG-SpaceOf -> " (CHK-ResStr spA)))

                  ;; 5. Инстмодель и создание
                  (setq mA (CHK-Call '(lambda () (KG-InstanceModel obj1 e1))))
                  (setq mA (if (car mA) (car (cdr mA)) nil))
                  (if (null mA)
                    (CHK-Say "   инстмодель не собралась -- пробу прекращаю")
                    (progn
                      (setq spB (cdr (assoc "space" mA)))
                      (if (and spB (= (type spB) (type "")) (not (KG-StrEq spB lname)))
                        (CHK-Say (strcat "   сценарий A: скан записал space «" spB
                                         "» -- именно это значение ушло бы в пересоздание"))
                        (progn
                          (setq mA (CHK-Put "space" bnm mA))
                          (CHK-Say (strcat "   сценарий B (принудительно): space «" bnm
                                           "» -- старое дефектное значение (проверка защиты)"))
                        )
                      )
                      (setq spB (cdr (assoc "space" mA)))
                      (if (and spB (= (type spB) (type "")))
                        (progn
                          (setq r (CHK-Call '(lambda () (KG-LayoutBlockForSpace spB))))
                          (CHK-Say
                            (strcat "   KG-LayoutBlockForSpace(\"" spB "\") -> "
                                    (if (not (car r))
                                      (strcat "ОШИБКА: " (car (cdr r)))
                                      (if (car (cdr r))
                                        "блок найден"
                                        "nil -- лист не найден; создание будет отклонено (в МОДЕЛЬ не уводим)"))))
                        )
                      )
                      (setq h2 (CHK-Call '(lambda () (KG_EXCreateInstance def mA))))
                      (cond
                        ((not (car h2))
                         (CHK-Say (strcat "3. Создание упало с ошибкой: " (car (cdr h2)))))
                        ((null (car (cdr h2)))
                         (CHK-Say "3. Создание по старому дефектному значению отклонено (nil).")
                         (CHK-Say "   ИТОГ: правильно -- экземпляр не создан, в МОДЕЛЬ не уведён."))
                        (t
                         (setq CHK-H2 (car (cdr h2)))
                         (CHK-Say (strcat "3. Пересоздание (KG_EXCreateInstance): handle "
                            (KG-AsString CHK-H2)))
                         (vl-catch-all-apply '(lambda () (KG-RefreshSpaceMap)))
                         (setq r (KG-SpaceOfHandle CHK-H2))
                         (CHK-Say (strcat "   новый экземпляр лёг в: «"
                            (if r r "не определилось")
                            "», ожидалось «" lname "»"))
                         (cond
                           ((and r (KG-StrEq r "Model") (not (KG-StrEq lname "Model")))
                            (CHK-Say "   СТАРЫЙ ДЕФЕКТ: экземпляр с листа лёг в МОДЕЛЬ"))
                           ((and r (KG-StrEq r lname))
                            (CHK-Say "   ИТОГ: экземпляр остался на своём листе."))
                           (t
                            (CHK-Say "   ИТОГ: пространство определить не удалось -- пришлите вывод целиком."))
                         )
                        )
                      )
                    )
                  )
                  (CHK-LiveCleanup)
                  (CHK-Say "Готово. Пришлите вывод целиком.")
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; Алиасы и баннер
;; ---------------------------------------------------------------------------

(defun C:INTSUBSTR () (C:CHKSUBSTR) (princ))
(defun C:ПДБСУБСТР () (C:CHKSUBSTR) (princ))
(defun C:INTSPACE () (C:CHKSPACE) (princ))
(defun C:ПДБЛИСТ () (C:CHKSPACE) (princ))
(defun C:INTSPACELIVE () (C:CHKSPACELIVE) (princ))
(defun C:ПДБЛИСТПРОБА () (C:CHKSPACELIVE) (princ))

(princ (strcat "\ncheck-substr-space.lsp, версия " CHK-VERSION
               ". Команды: CHKSUBSTR (INTSUBSTR, ПДБСУБСТР), "
               "CHKSPACE (INTSPACE, ПДБЛИСТ), "
               "CHKSPACELIVE (INTSPACELIVE, ПДБЛИСТПРОБА)."))
(princ)
