; ============================================================
; probe-holders.lsp -- разбор держателей мусора «~ИМЯ» и «*U», проба 6.
;
; Проба 1 показала: «Комплект КП50К стойка КП45370 v1.3» держит внутри
; старые «~ИМЯ» (десятки вставок) и часть «*U». Такие держатели не видит
; карта верхнего уровня (KG-EXRefHolders), поэтому очистка считает остаток
; неиспользуемым, а PURGE и стирание его не берут.
;
; Проба 3: перепривязка пробует два способа записи имени вложенной вставки
; (entmod группы 2 и COM vla-put-Name), печатает счёт по каждому держателю,
; умеет обработать сразу все пары возврата (Enter на запросе имени) и
; сообщает остаток служебных имён после PURGE.
;
; Проба 4 добавила ЭКСП-ЦЕПОЧКА: игрушечные блоки в новом пустом чертеже
; сравнивают штатную цепочку возврата ссылок (KG-RewireOnePair,
; KG-RewirePairOf, KG-Step_RewireDefs) с прямой (EXP-FixOne) -- чтобы
; увидеть, на каком звене цепочка глохнет на живом AutoCAD.
;
; Проба 5 делит проверку на пять звеньев (прямой вызов, обёртка, пара,
; полный шаг, контроль) и требует, чтобы в документе был загружен рабочий
; файл: команды зовут его функции. Без него печатается напоминание.
;
; Проба 6: служебные имена троек названы «~база» -- чтобы
; KG-CleanupRestorePairs видел пару (без снятия отметки «~» пара не
; находилась, и звено [Г] давало «пар: 0» из-за моей ошибки в именах).
; Проба проверяет сборку 71.2, где KG-RewirePairOf починена (переменную r
; в лямбде подменяла одноимённая переменная перехвата KG-Safe).
;
; Нужен загруженный RepDblock.lsp (сборка 71.2): вызываются KG-функции.
; Загрузка: (load "D:/ReplaceDBlock/testbed/probe-holders.lsp")
;
; ЭКСП-ДЕРЖАТЕЛИ    -- только чтение, безопасна на рабочем чертеже.
; ЭКСП-ПЕРЕПРИВЯЗКА -- меняет чертёж, запускать ТОЛЬКО на копии.
; ============================================================

(defun EXP-VER () "проба 6 (сборка 71.2)")

;; Имена всех определений запасным способом -- без COM.
(defun EXP-AllNames ( / r out)
  (setq out nil)
  (setq r (KG-Safe '(lambda () (tblnext "BLOCK" t)) nil))
  (while r
    (if (cdr (assoc 2 r)) (setq out (cons (cdr (assoc 2 r)) out)))
    (setq r (KG-Safe '(lambda () (tblnext "BLOCK")) nil))
  )
  (reverse out)
)

;; Первое служебное имя «~ИМЯ» в чертеже (nil, если такого нет).
(defun EXP-FirstTempName ( / names nm out)
  (setq names (KG-Call 'KG_EXAllDefNames nil nil))
  (if (null names) (setq names (EXP-AllNames)))
  (setq out nil)
  (foreach nm names
    (if (and (null out) (KG-Call 'KG-IsTempDefName (list nm) nil))
      (setq out nm))
  )
  out
)

;; Счётчик «ключ -> сколько раз» в списке пар.
(defun EXP-Add (k al / pr)
  (setq pr (KG-AssocCI k al))
  (if pr
    (KG-SetAssoc k (1+ (cdr pr)) al)
    (cons (cons k 1) al)
  )
)

;; Счётчик одной строкой: «INSERT=3, LINE=12».
(defun EXP-Types (al / out)
  (setq out "")
  (foreach pr al
    (setq out (strcat out (if (= out "") "" ", ")
                       (KG-AsString (car pr)) "=" (itoa (cdr pr))))
  )
  out
)

;; Есть ли среди имён списка служебные «~ИМЯ» или «*U»; nil, если нет.
(defun EXP-HasJunk (lst / out)
  (setq out nil)
  (foreach x lst
    (if (or (KG-Call 'KG-IsTempDefName (list x) nil)
            (KG-Call 'KG-IsAnonDynName (list x) nil))
      (setq out t))
  )
  out
)

;; Служебные имена из списка -- одним списком.
(defun EXP-JunkOnly (lst / out)
  (setq out nil)
  (foreach x lst
    (if (or (KG-Call 'KG-IsTempDefName (list x) nil)
            (KG-Call 'KG-IsAnonDynName (list x) nil))
      (setq out (cons x out)))
  )
  (reverse out)
)

;; Пройти содержимое одного определения. Печатает состав и возвращает
;; список пар «имя вставки -> сколько раз».
(defun EXP-ScanDef (defname / e ed ty n hist ins shown)
  (setq n 0 hist nil ins nil)
  (setq e (KG-Call 'KG-DefEnt (list defname) nil))
  (princ (strcat "\n  запись определения: "
                 (if e "прочитана" "НЕ найдена (три пути молчат)")))
  (if e
    (while (and (setq e (KG-Call 'entnext (list e) nil))
                (setq ed (KG-Call 'entget (list e) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (setq ty (KG-AsString (cdr (assoc 0 ed))))
      (setq n (1+ n))
      (setq hist (EXP-Add ty hist))
      (if (KG-StrEq ty "INSERT")
        (setq ins (EXP-Add (KG-AsString (cdr (assoc 2 ed))) ins))
      )
    )
  )
  (princ (strcat "\n  сущностей пройдено: " (itoa n)))
  (if (> n 0) (princ (strcat "\n  типы: " (EXP-Types hist))))
  (setq shown 0)
  (foreach pr ins
    (if (< shown 25)
      (progn
        (princ (strcat "\n    вставка \"" (KG-AsString (car pr)) "\" (шт. "
                       (itoa (cdr pr)) ")"))
        (setq shown (1+ shown))
      )
    )
  )
  (if (> (length ins) 25)
    (princ (strcat "\n    ... и ещё " (itoa (- (length ins) 25)) " имён"))
  )
  (if (and e (= n 0)) (princ "\n  (содержимое пусто)"))
  ins
)

;; Кто из списка allrefs держит имя tgt. allrefs -- пары
;; (имя определения . список имён вставок внутри него).
(defun EXP-HoldersOf (tgt allrefs / out pr)
  (setq out nil)
  (foreach pr allrefs
    (if (KG-Call 'KG-StrInterCI (list (list tgt) (cdr pr)) nil)
      (setq out (cons (car pr) out))
    )
  )
  (reverse out)
)

(defun C:ЭКСП-ДЕРЖАТЕЛИ ( / names nm snm base top hl inner shown cnt cntu allrefs
                            i pr x f1 cr1 cr2 rp)
  (vl-load-com)
  (setq KG-REWHOLD nil KG-REWSKIP nil KG-REWPAIRS nil)
  (princ (strcat "\n=== ЭКСП-ДЕРЖАТЕЛИ (" (EXP-VER) "): кто держит остаток ==="))
  (setq names (KG-Call 'KG_EXAllDefNames nil nil))
  (if (null names)
    (progn
      (princ "\n(COM-список имён не дался -- беру таблицу блоков)")
      (setq names (EXP-AllNames))
    )
  )
  (if (null names)
    (princ "\nСписок определений не получен. Конец.")
    (progn
      (princ (strcat "\nОпределений в таблице: " (itoa (length names))))
      (setq top (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
      (setq f1 nil cnt 0 cntu 0)
      (foreach nm names
        (if (KG-Call 'KG-IsTempDefName (list nm) nil)
          (progn
            (setq cnt (1+ cnt))
            (if (null f1) (setq f1 nm))
          )
        )
        (if (KG-Call 'KG-IsAnonDynName (list nm) nil) (setq cntu (1+ cntu)))
      )
      (princ (strcat "\nСлужебных «~ИМЯ» всего: " (itoa cnt)))
      (princ (strcat "\nАнонимных «*U» всего: " (itoa cntu)))
      (setq snm (getstring T "\nИмя определения для разбора содержимого <Комплект КП50К стойка v1.51>: "))
      (if (= snm "") (setq snm "Комплект КП50К стойка v1.51"))
      ;; --- один общий обход содержимого всех определений ---
      (princ "\nОбхожу содержимое определений (одним проходом)...")
      (setq allrefs nil)
      (setq i 0)
      (foreach nm names
        (setq i (1+ i))
        (if (= (rem i 50) 0)
          (princ (strcat "\n  обойдено " (itoa i) " из " (itoa (length names))))
        )
        (setq inner (KG-Call 'KG_DefEntNames (list nm) nil))
        (if inner (setq allrefs (cons (cons nm (KG-Unique inner)) allrefs)))
      )
      (princ (strcat "\nОпределений со вставками внутри: " (itoa (length allrefs))))
      ;; --- прямые проверки звеньев машины ссылок ---
      (setq inner (KG-Call 'KG_DefEntNames (list snm) nil))
      (setq cnt 0 cntu 0)
      (foreach x inner
        (if (KG-Call 'KG-IsTempDefName (list x) nil) (setq cnt (1+ cnt)))
        (if (KG-Call 'KG-IsAnonDynName (list x) nil) (setq cntu (1+ cntu)))
      )
      (princ (strcat "\nПроверка KG_DefEntNames(\"" snm "\"): имён вставок "
                     (itoa (length inner)) ", из них «~ИМЯ»: " (itoa cnt)
                     ", «*U»: " (itoa cntu)))
      (if f1
        (progn
          (setq hl (KG-Call 'KG-DefRefHolders (list f1) nil))
          (princ (strcat "\nПроверка KG-DefRefHolders(\"" f1 "\"): держателей "
                         (itoa (length hl))
                         (if hl (strcat " (" (KG-JoinNames hl) ")") "")))
        )
        (princ "\nСлужебных имён нет -- проверку KG-DefRefHolders пропускаю.")
      )
      (setq rp (KG-Call 'KG-CleanupRestorePairs (list names) nil))
      (princ (strcat "\nПроверка KG-CleanupRestorePairs: пар " (itoa (length rp))))
      (setq shown 0)
      (foreach pr rp
        (if (< shown 5)
          (progn
            (princ (strcat "\n  \"" (nth 1 pr) "\" -> \"" (nth 0 pr) "\""))
            (setq shown (1+ shown))
          )
        )
      )
      ;; --- кто держит мусор: общий список ---
      (setq cr1 0 cr2 0)
      (foreach pr allrefs
        (foreach x (cdr pr)
          (if (KG-Call 'KG-IsTempDefName (list x) nil) (setq cr1 (1+ cr1)))
          (if (KG-Call 'KG-IsAnonDynName (list x) nil) (setq cr2 (1+ cr2)))
        )
      )
      (princ (strcat "\nСсылок на «~ИМЯ» внутри определений всего: " (itoa cr1)))
      (princ (strcat "\nСсылок на «*U» внутри определений всего: " (itoa cr2)))
      (setq cnt 0)
      (foreach pr allrefs
        (if (EXP-HasJunk (cdr pr)) (setq cnt (1+ cnt)))
      )
      (princ (strcat "\nОпределений, держащих мусор: " (itoa cnt)))
      (setq shown 0)
      (foreach pr allrefs
        (setq x (EXP-JunkOnly (cdr pr)))
        (if (and x (< shown 6))
          (progn
            (princ (strcat "\n  \"" (car pr) "\" держит: " (KG-JoinNames x)))
            (setq shown (1+ shown))
          )
        )
      )
      ;; --- «~ИМЯ»: до 9 строк ---
      (setq shown 0)
      (foreach nm names
        (if (and (< shown 9) (KG-Call 'KG-IsTempDefName (list nm) nil))
          (progn
            (setq base (KG-StripDoSuffix nm))
            (setq hl (EXP-HoldersOf nm allrefs))
            (princ (strcat "\n\"" nm "\" -- база \"" base "\" "
                           (if (KG-Call 'KG-StrInterCI (list (list base) names) nil)
                             "в чертеже есть" "в чертеже НЕТ")))
            (princ (strcat "; сверху вставок: "
                           (itoa (length (KG-Call 'KG-CdrCI
                                                  (list (KG-AsString nm) top) nil)))))
            (princ (strcat "; внутри определений держат: " (itoa (length hl))
                           (if hl (strcat " (" (KG-JoinNames hl) ")") "")))
            (setq shown (1+ shown))
          )
        )
      )
      (if (= shown 0) (princ "\nСлужебных имён «~ИМЯ» в чертеже нет."))
      ;; --- «*U»: до 9 строк ---
      (setq shown 0)
      (foreach nm names
        (if (and (< shown 9) (KG-Call 'KG-IsAnonDynName (list nm) nil))
          (progn
            (setq hl (EXP-HoldersOf nm allrefs))
            (princ (strcat "\n\"" nm "\" -- сверху вставок: "
                           (itoa (length (KG-Call 'KG-CdrCI
                                                  (list (KG-AsString nm) top) nil)))))
            (princ (strcat "; внутри определений держат: " (itoa (length hl))
                           (if hl (strcat " (" (KG-JoinNames hl) ")") "")))
            (setq shown (1+ shown))
          )
        )
      )
      ;; --- словари первых трёх «*U» ---
      (setq shown 0)
      (foreach nm names
        (if (and (< shown 3) (KG-Call 'KG-IsAnonDynName (list nm) nil))
          (progn
            (setq x (KG-Call 'KG-DefDictProbe (list nm) ""))
            (princ (strcat "\n\"" nm "\": словарь -> "
                           (if (= x "") "(словаря не видно)" x)))
            (setq shown (1+ shown))
          )
        )
      )
      ;; --- содержимое названного определения ---
      (princ (strcat "\n--- содержимое \"" snm "\" ---"))
      (setq inner (EXP-ScanDef snm))
      (setq x (EXP-JunkOnly (mapcar 'car inner)))
      (if (null x)
        (princ "\n  служебных/анонимных ссылок внутри: нет")
        (princ (strcat "\n  служебные/анонимные ссылки внутри: " (KG-JoinNames x)))
      )
    )
  )
  (princ (strcat "\n=== конец ЭКСП-ДЕРЖАТЕЛИ (" (EXP-VER) ") ==="))
  (princ)
)

;; Перепривязка держателей: два способа на каждого держателя.
;; Способ 1 -- переписать запись вставки (entmod группы 2).
(defun EXP-RewireOf (h nm base)
  (KG-Safe '(lambda () (KG-RewireDefRefs h nm base)) 0)
)

;; Способ 2 -- свойство Name через COM (vla-put-Name). Считается только
;; то, что подтвердилось повторным чтением группы 2 после записи.
(defun EXP-RewireOfCom (h nm base / e ed n obj)
  (setq n 0)
  (setq e (KG-Call 'KG-DefEnt (list h) nil))
  (if e
    (while (and (setq e (KG-Call 'entnext (list e) nil))
                (setq ed (KG-Call 'entget (list e) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT")
        (if (KG-StrEq (KG-AsString (cdr (assoc 2 ed))) nm)
          (progn
            (setq obj (KG-Call 'vlax-ename->vla-object (list e) nil))
            (if obj
              (progn
                (KG-Call 'vla-put-Name (list obj (KG-AsString base)) nil)
                (KG-Call 'entupd (list e) nil)
                (setq ed (KG-Call 'entget (list e) nil))
                (if (KG-StrEq (KG-AsString (cdr (assoc 2 ed))) base)
                  (setq n (1+ n))
                )
              )
            )
          )
        )
      )
    )
  )
  n
)

;; Починить всех держателей одного служебного имени. Печатает строку на
;; держателя: сколько вставок переписано entmod и сколько через COM.
(defun EXP-FixOne (nm base holders / h n1 n2 tot)
  (princ (strcat "\n\"" nm "\" -> \"" base "\": держателей "
                 (itoa (length holders))))
  (setq tot 0)
  (foreach h holders
    (setq n1 (KG-AsNum (EXP-RewireOf h nm base) 0))
    (setq n2 (KG-AsNum (EXP-RewireOfCom h nm base) 0))
    (princ (strcat "\n  \"" h "\": entmod " (itoa n1) ", COM " (itoa n2)))
    (setq tot (+ tot n1 n2))
  )
  (princ (strcat "\n  итого по имени: " (itoa tot)))
  tot
)

;; Сколько служебных имён «~ИМЯ» в чертеже сейчас.
(defun EXP-CountTemp ( / names nm c)
  (setq names (KG-Call 'KG_EXAllDefNames nil nil))
  (if (null names) (setq names (EXP-AllNames)))
  (setq c 0)
  (foreach nm names
    (if (KG-Call 'KG-IsTempDefName (list nm) nil) (setq c (1+ c)))
  )
  c
)

(defun C:ЭКСП-ПЕРЕПРИВЯЗКА ( / nm nm2 base holders pairs pr tot i names
                              allrefs inner shown x)
  (vl-load-com)
  (setq KG-REWHOLD nil KG-REWSKIP nil KG-REWPAIRS nil)
  (princ (strcat "\n=== ЭКСП-ПЕРЕПРИВЯЗКА (" (EXP-VER) "): вернуть ссылки с «~ИМЯ» ==="))
  (princ "\nВНИМАНИЕ: команда меняет чертёж. Запускать только на КОПИИ.")
  (setq nm (getstring T "\nСлужебное имя (Enter = обработать ВСЕ пары): "))
  (if (= nm "")
    (progn
      (setq names (KG-Call 'KG_EXAllDefNames nil nil))
      (if (null names) (setq names (EXP-AllNames)))
      (setq pairs (KG-Call 'KG-CleanupRestorePairs (list names) nil))
      (princ (strcat "\nПар для обработки: " (itoa (length pairs))))
      (princ (strcat "\nСлужебных «~ИМЯ» до правки: " (itoa (EXP-CountTemp))))
      (princ "\nОбхожу содержимое определений (одним проходом)...")
      (setq allrefs nil i 0)
      (foreach nm2 names
        (setq i (1+ i))
        (if (= (rem i 50) 0)
          (princ (strcat "\n  обойдено " (itoa i) " из " (itoa (length names))))
        )
        (setq inner (KG-Call 'KG_DefEntNames (list nm2) nil))
        (if inner (setq allrefs (cons (cons nm2 (KG-Unique inner)) allrefs)))
      )
      (princ (strcat "\nОпределений со вставками внутри: " (itoa (length allrefs))))
      (if pairs
        (progn
          (setq nm (nth 1 (car pairs)))
          (setq holders (KG-Call 'KG-DefRefHolders (list nm) nil))
          (princ (strcat "\nСверка на \"" nm "\": машина ссылок напрямую "
                         (itoa (length holders)) ", карта обхода "
                         (itoa (length (EXP-HoldersOf nm allrefs)))))
        )
      )
      (setq tot 0 i 0)
      (foreach pr pairs
        (setq i (1+ i))
        (princ (strcat "\n\n[" (itoa i) " из " (itoa (length pairs)) "]"))
        (setq tot (+ tot (EXP-FixOne (nth 1 pr) (nth 0 pr)
                                     (EXP-HoldersOf (nth 1 pr) allrefs))))
      )
      (princ (strcat "\n\nВсего исправлено вставок: " (itoa tot)))
      (princ "\nPURGE блоков (маска *) дважды...")
      (command "_.-PURGE" "_B" "*" "_N")
      (command "_.-PURGE" "_B" "*" "_N")
      (setq x (EXP-CountTemp))
      (princ (strcat "\nСлужебных «~ИМЯ» после PURGE: " (itoa x)))
      (if (> x 0)
        (progn
          (setq shown nil)
          (foreach nm2 names
            (if (and (< (length shown) 6)
                     (KG-Call 'KG-IsTempDefName (list nm2) nil))
              (setq shown (cons nm2 shown))
            )
          )
          (princ (strcat "\n  остались: " (KG-JoinNames (reverse shown))))
        )
      )
      (if (= x 0)
        (princ "\nОстатка нет -- путь верный, пришли этот лог.")
        (princ "\nОстаток есть -- пришли этот лог.")
      )
    )
    (progn
      (setq base (KG-StripDoSuffix nm))
      (if (KG-StrEq base nm)
        (princ (strcat "\n\"" nm "\" не похоже на «~ИМЯ» (снятие отметки ничего не изменило)."))
        (if (not (tblsearch "BLOCK" base))
          (princ (strcat "\nВНИМАНИЕ: определения \"" base "\" в чертеже нет -- перепривязывать некуда."))
          (progn
            (setq holders (KG-Call 'KG-DefRefHolders (list nm) nil))
            (princ (strcat "\nДержателей по машине ссылок: " (itoa (length holders))))
            (setq tot (EXP-FixOne nm base holders))
            (setq KG-REWHOLD nil)
            (princ (strcat "\nПосле перепривязки держателей: "
                           (itoa (length (KG-Call 'KG-DefRefHolders (list nm) nil)))))
            (princ "\nПробую PURGE по служебному имени...")
            (command "_.-PURGE" "_B" nm "_N")
            (if (tblsearch "BLOCK" nm)
              (progn
                (princ "\nПо имени не снято -- пробую -PURGE по маске *.")
                (command "_.-PURGE" "_B" "*" "_N")
              )
            )
            (setq x (not (tblsearch "BLOCK" nm)))
            (princ (strcat "\nПосле PURGE определение \"" nm "\" "
                           (if x "СНЯТО" "осталось")))
            (if (and (not x) (= tot 0))
              (princ "\nОба способа не сработали -- пришли этот вывод.")
            )
          )
        )
      )
    )
  )
  (princ (strcat "\n=== конец ЭКСП-ПЕРЕПРИВЯЗКА (" (EXP-VER) ") ==="))
  (princ)
)
;; Игрушечная тройка: база, её служебная копия «~имя», чужой мастер со
;; вставкой копии. Ставится на новый пустой чертёж -- там видно, на каком
;; звене штатная цепочка возврата ссылок глохнет на живом AutoCAD.
(defun EXP-MkTriple (base tmp mastername)
  (entmake (list '(0 . "BLOCK") (cons 2 base) '(70 . 0) '(10 0.0 0.0 0.0)))
  (entmake '((0 . "LINE") (10 0.0 0.0 0.0) (11 1.0 0.0 0.0)))
  (entmake '((0 . "ENDBLK")))
  (entmake (list '(0 . "BLOCK") (cons 2 tmp) '(70 . 0) '(10 0.0 0.0 0.0)))
  (entmake '((0 . "LINE") (10 0.0 0.0 0.0) (11 1.0 0.0 0.0)))
  (entmake '((0 . "ENDBLK")))
  (entmake (list '(0 . "BLOCK") (cons 2 mastername) '(70 . 0) '(10 0.0 0.0 0.0)))
  (entmake (list '(0 . "INSERT") (cons 2 tmp) '(10 5.0 0.0 0.0)))
  (entmake '((0 . "ENDBLK")))
)

;; Обёртка вокруг поиска держателей -- в точности вид вызова из
;; KG-RewireOnePair: лямбда цитирует аргумент этой самой функции.
(defun EXP-TestHolders (nm)
  (KG-Safe '(lambda () (KG-DefRefHolders nm)) nil)
)

;; Рабочая часть команды; зовётся из C:ЭКСП-ЦЕПОЧКА после проверки загрузки.
(defun EXP-CepochkaRun ( / names mn bad p3)
  (setq KG-REWHOLD nil KG-REWSKIP nil KG-REWPAIRS nil)
  (setq names (KG-Call 'KG_EXAllDefNames nil nil))
  (if (null names) (setq names (EXP-AllNames)))
  (if (> (length names) 30)
    (princ "\nВ чертеже уже много определений -- нужен НОВЫЙ пустой чертёж.")
    (progn
      (EXP-MkTriple "ЭЦ-База1" "~ЭЦ-База1" "ЭЦ-Мастер1")
      (EXP-MkTriple "ЭЦ-База2" "~ЭЦ-База2" "ЭЦ-Мастер2")
      (EXP-MkTriple "ЭЦ-База3" "~ЭЦ-База3" "ЭЦ-Мастер3")
      (EXP-MkTriple "ЭЦ-База4" "~ЭЦ-База4" "ЭЦ-Мастер4")
      (EXP-MkTriple "ЭЦ-База5" "~ЭЦ-База5" "ЭЦ-Мастер5")
      (setq bad nil)
      (foreach mn '("ЭЦ-База1" "~ЭЦ-База1" "ЭЦ-Мастер1"
                    "ЭЦ-База2" "~ЭЦ-База2" "ЭЦ-Мастер2"
                    "ЭЦ-База3" "~ЭЦ-База3" "ЭЦ-Мастер3"
                    "ЭЦ-База4" "~ЭЦ-База4" "ЭЦ-Мастер4"
                    "ЭЦ-База5" "~ЭЦ-База5" "ЭЦ-Мастер5")
        (if (not (KG-Call 'tblsearch (list "BLOCK" mn) nil)) (setq bad t))
      )
      (if bad
        (princ "\nВНИМАНИЕ: не все игрушечные определения построились.")
        (progn
          (princ "\nПостроено 5 троек: база, «~копия», мастер со вставкой «~копии».")
          (setq KG-REWHOLD nil)
          (princ (strcat "\nДержатели \"~ЭЦ-База1\" напрямую: "
                         (itoa (length (KG-Call 'KG-DefRefHolders (list "~ЭЦ-База1") nil)))))
          (setq KG-REWHOLD nil)
          (princ (strcat "\nДержатели \"~ЭЦ-База1\" через обёртку-функцию: "
                         (itoa (length (EXP-TestHolders "~ЭЦ-База1")))))
          (princ "\n--- звенья цепочки ---")
          (princ (strcat "\n[А] KG-RewireOnePair напрямую: "
                         (itoa (KG-AsNum (KG-RewireOnePair "~ЭЦ-База1" "ЭЦ-База1") 0))))
          (princ (strcat "\n[Б] KG-RewireOnePair под обёрткой: "
                         (itoa (KG-AsNum
                                 (KG-Safe '(lambda () (KG-RewireOnePair "~ЭЦ-База2" "ЭЦ-База2")) 0)
                                 0))))
          (princ (strcat "\n[В] KG-RewirePairOf: "
                         (itoa (KG-AsNum (KG-RewirePairOf (list "ЭЦ-База3" "~ЭЦ-База3")) 0))))
          (setq p3 (KG-Call 'KG-CleanupRestorePairs (list (list "ЭЦ-База4" "~ЭЦ-База4")) nil))
          (princ (strcat "\n[Г] пар из KG-CleanupRestorePairs: " (itoa (length p3))))
          (princ (strcat "\n[Г] KG-Step_RewireDefs (как из очистки): "
                         (itoa (KG-AsNum (KG-Call 'KG-Step_RewireDefs (list p3) 0) 0))))
          (princ (strcat "\n[Д] прямая перепривязка (контроль): "
                         (itoa (KG-AsNum
                                 (EXP-FixOne "~ЭЦ-База5" "ЭЦ-База5"
                                             (KG-Call 'KG-DefRefHolders (list "~ЭЦ-База5") nil))
                                 0))))
          (princ "\n--- что теперь внутри мастеров ---")
          (foreach mn '("ЭЦ-Мастер1" "ЭЦ-Мастер2" "ЭЦ-Мастер3" "ЭЦ-Мастер4" "ЭЦ-Мастер5")
            (princ (strcat "\n  " mn ": "
                           (KG-JoinNames (KG-Call 'KG_DefEntNames (list mn) nil))))
          )
          (princ "\nГде звено сработало -- внутри мастера \"ЭЦ-БазаN\"; где отказало -- \"~ЭЦ-БазаN\".")
          (princ "\nЧертёж не сохранять.")
        )
      )
    )
  )
  (princ)
)

(defun C:ЭКСП-ЦЕПОЧКА ( / )
  (vl-load-com)
  (princ (strcat "\n=== ЭКСП-ЦЕПОЧКА (" (EXP-VER) "): штатная цепочка против прямой ==="))
  (princ "\nЗапускать в НОВОМ ПУСТОМ чертеже и не сохранять его.")
  (if (not (boundp 'KG-Call))
    (princ "\nСНАЧАЛА загрузите рабочий файл в ЭТОМ документе: (load \"D:/ReplaceDBlock/RepDblock.lsp\"), затем повторите команду.")
    (EXP-CepochkaRun)
  )
  (princ (strcat "\n=== конец ЭКСП-ЦЕПОЧКА (" (EXP-VER) ") ==="))
  (princ)
)
; конец probe-holders.lsp (проба 5)
