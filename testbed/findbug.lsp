;;;===========================================================================
;;; findbug.lsp -- самостоятельная диагностика чертежа.
;;;
;;; НЕ ЗАВИСИТ от Integration.lsp. Загрузите отдельно:
;;;     (load "D:/findbug.lsp")
;;; и выполните команду FINDBUG.
;;;
;;; Команда проходит по всем определениям и вхождениям чертежа и для каждого
;;; повторяет те же операции чтения, что делает INTEGRATE. На первой же
;;; ошибке печатает ИМЯ ОБЪЕКТА и ШАГ, на котором она произошла.
;;;
;;; Чертеж не изменяется.
;;;===========================================================================

(vl-load-com)

(setq FINDBUG-VERSION "2")

(defun FB-Say (s) (princ (strcat "\n" s)))

(defun FB-SayKV (k v) (princ (strcat "\n" k ": " v)))

;; Выполнить операцию; при ошибке напечатать её и вернуть nil
(defun FB-Try (label fn / r)
  (setq r (vl-catch-all-apply fn))
  (if (vl-catch-all-error-p r)
    (progn
      (FB-Say (strcat "      ОШИБКА [" label "]: " (vl-princ-to-string r)))
      nil)
    r
  )
)

;;; Карта handle -> пространство (модель или имя листа).
;;; Группа 330 в entget экземпляра есть не во всех файлах -- версия 1
;;; полагалась только на неё и на реальном чертеже не определила
;;; пространство ни у одного вхождения.
(defun FB-BuildSpaceMap ( / map blk nm sp lay)
  (setq map nil)
  (vlax-for blk (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
    (if (and (not (vl-catch-all-error-p nm))
             (not (vl-catch-all-apply '(lambda () (vla-get-IsXRef blk)))))
      (progn
        (setq sp
          (cond
            ((wcmatch (strcase nm) "`*MODEL_SPACE*") "Model")
            ((wcmatch (strcase nm) "`*PAPER_SPACE*")
             (setq lay (vl-catch-all-apply '(lambda () (vla-get-Layout blk))))
             (if (vl-catch-all-error-p lay)
               nm
               (vl-catch-all-apply '(lambda () (vla-get-Name lay)))))
            (t nil)))
        (if (and sp (not (vl-catch-all-error-p sp)))
          (vl-catch-all-apply
            '(lambda ()
               (vlax-for e blk
                 (setq map
                   (cons (cons (strcase (vla-get-Handle e)) sp) map)))))
        )
      )
    )
  )
  (reverse map)
)

(defun FB-SpaceOfHandle (handle / p)
  (setq p (assoc (strcase handle) FB-SPACE-MAP))
  (if p (cdr p) "НЕ ОПРЕДЕЛЕНО")
)

;; Чтение одного определения
(defun FB-CheckDef (blk / nm r arr n i)
  (setq nm (FB-Try "vla-get-Name" '(lambda () (vla-get-Name blk))))
  (if (null nm)
    (progn (FB-Say "   определение: имя не читается") nil)
    (progn
      (FB-Say (strcat "   определение \"" nm "\""))
      (FB-Try "IsXRef"        '(lambda () (vla-get-IsXRef blk)))
      (FB-Try "IsDynamicBlock" '(lambda () (vla-get-IsDynamicBlock blk)))
      ;; вложенные вхождения внутри определения
      (FB-Try "обход вложенных"
        '(lambda ()
           (vlax-for e blk
             (vla-get-ObjectName e)
             (vl-catch-all-apply '(lambda () (vla-get-EffectiveName e)))
           )
           t))
      ;; параметры видимости: берём первое вхождение этого определения
      (setq r (FB-Try "поиск вхождения"
                '(lambda ()
                   (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 2 nm))))
                   (if (and ss (> (sslength ss) 0)) (ssname ss 0) nil))))
      (if r (FB-CheckVisibilityOf r nm))
      t
    )
  )
)

;; Динамические свойства и параметр видимости у одного вхождения
(defun FB-CheckVisibilityOf (e who / obj props p nm allowed arr n i v)
  (setq obj (FB-Try "vlax-ename->vla-object"
              '(lambda () (vlax-ename->vla-object e))))
  (if obj
    (progn
      (setq props (FB-Try "GetDynamicBlockProperties"
                    '(lambda () (vlax-invoke obj 'GetDynamicBlockProperties))))
      (if (and props (/= (type props) (type "")))
        (foreach p props
          (setq nm (FB-Try "PropertyName"
                     '(lambda () (vla-get-PropertyName p))))
          ;; ВОТ ЗДЕСЬ чаще всего и падало: AllowedValues у свойства без
          ;; списка значений возвращает ПУСТОЙ variant (тип 8197, VT_EMPTY),
          ;; и vlax-variant-value на нём бросает ошибку
          (setq allowed (FB-Try "AllowedValues"
                          '(lambda () (vlax-get-property p 'AllowedValues))))
          (FB-Say (strcat "      тип AllowedValues: "
                          (if allowed (vl-princ-to-string (type allowed)) "nil")
                          (if allowed
                            (strcat " / " (vl-princ-to-string allowed))
                            "")))
          (setq arr (FB-Try "vlax-variant-value(AllowedValues)"
                      '(lambda () (vlax-variant-value allowed))))
          (if arr
            (progn
              (setq n (FB-Try "safearray-get-u-bound"
                        '(lambda () (vlax-safearray-get-u-bound arr 1))))
              (FB-Say (strcat "      граница массива: "
                              (if n (itoa n) "не читается")))
              (if n
                (progn
                  (setq i 0)
                  (while (<= i n)
                    (setq v (FB-Try "элемент массива"
                              '(lambda ()
                                 (vlax-variant-value
                                   (vlax-safearray-get-element arr i)))))
                    (FB-Say (strcat "        состояние " (itoa i) ": "
                                    (if v (vl-princ-to-string v) "?")))
                    (setq i (1+ i))
                  )
                )
              )
            )
          )
          (setq v (FB-Try "Value" '(lambda () (vlax-get-property p 'Value))))
          (FB-Say (strcat "      свойство \"" (if nm nm "?")
                          "\", тип Value: "
                          (if v (vl-princ-to-string (type v)) "nil")
                          " = " (if v (vl-princ-to-string v) "?")))
        )
      )
    )
  )
)

(defun C:FINDBUG ( / *error* acad doc blks blk ss i e h ok bad firstbad)
  (defun *error* (m)
    (if (and m (/= m "Function cancelled") (/= m "quit / exit abort"))
      (princ (strcat "\nОШИБКА КОМАНДЫ: " m))
    )
    (princ)
  )
  (vl-load-com)

  (FB-Say (strcat "=== FINDBUG, версия " FINDBUG-VERSION " ==="))
  (FB-Say "Чертеж не изменяется.")

  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq blks (vla-get-Blocks doc))

  ;; 1. определения
  (FB-Say "")
  (FB-Say "1. ОПРЕДЕЛЕНИЯ")
  (setq ok 0 bad 0 firstbad nil)
  (vlax-for blk blks
    (if (FB-CheckDef blk)
      (setq ok (1+ ok))
      (progn
        (setq bad (1+ bad))
        (if (not firstbad) (setq firstbad "см. выше"))
      )
    )
  )
  (FB-SayKV "  прочитано" (itoa ok))
  (FB-SayKV "  с ошибками" (itoa bad))

  ;; карта пространств
  (FB-Say "")
  (FB-Say "2. КАРТА ПРОСТРАНСТВ (handle -> модель/лист)")
  (setq FB-SPACE-MAP (FB-Try "построение карты" '(lambda () (FB-BuildSpaceMap))))
  (if FB-SPACE-MAP
    (FB-SayKV "   объектов в карте" (itoa (length FB-SPACE-MAP)))
    (FB-Say "   карту построить не удалось")
  )

  ;; 2. вхождения
  (FB-Say "")
  (FB-Say "3. ВХОЖДЕНИЯ")
  (setq ok 0 bad 0)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      (setq h (cdr (assoc 5 (entget e))))
      (FB-Say (strcat "   вхождение handle " h " определение \""
                      (cdr (assoc 2 (entget e))) "\""))
      ;; пространство: по карте handle -> пространство
      (FB-Say (strcat "      пространство: " (FB-SpaceOfHandle h)))
      ;; группа 330 -- только справка: в этом файле её может не быть
      (setq oh (cdr (assoc 330 (entget e))))
      (FB-Say (strcat "      группа 330: " (if oh oh "ОТСУТСТВУЕТ")))
      (if (FB-CheckVisibilityOf e h)
        (setq ok (1+ ok))
        (setq bad (1+ bad))
      )
    )
  )
  (FB-SayKV "  прочитано" (itoa ok))
  (FB-SayKV "  с ошибками" (itoa bad))

  (FB-Say "")
  (FB-Say "Готово. Пришлите вывод целиком.")
  (princ)
)

(princ (strcat "\nfindbug.lsp, версия " FINDBUG-VERSION
               ". Запустите команду FINDBUG."))
(princ)
