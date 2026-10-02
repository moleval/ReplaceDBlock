;;;===========================================================================
;;; Integration.lsp  --  интеграция новой итерации динамического блока
;;;                        из буфера обмена в текущий DWG
;;;
;;; Команды:
;;;   INTEGRATE       -- полная интеграция
;;;   INTEGRATECHECK  -- скан без изменений (Этап 2)
;;;   INTDUMP         -- диагностика экземпляра (Этап 0.5)
;;;   INTDUMPDEF      -- диагностика определения (Этап 0.5)
;;;   INTPASTETEST    -- диагностика вставки из буфера (Этап 0.6)
;;;   INTTESTBED      -- построение тестового стенда (Этап 0)
;;;   INTCLEANUP      -- очистка определений прошлой интеграции
;;;   INTCOUNT        -- снимок таблицы блоков: вставок на определение
;;;   INTBRIEF        -- короткая выжимка: чем кончилась очистка
;;;   INTERR          -- короткий отчёт об ошибке (для отправки в чат)
;;;
;;; ГЛАВНЫЙ ПРИНЦИП: источник истины -- содержимое буфера обмена.
;;; Имена вида "ABC1.01(15)Стойка" в ТЗ -- только примеры. Программа
;;; берёт семейство и номер итерации из того определения, которое реально
;;; пришло из буфера, а набор вариантов (суффиксов) -- из того, что реально
;;; найдено в текущем файле.
;;;
;;; Архитектура (подробности в docs/README.md):
;;;   Раздел 1  утилиты
;;;   Раздел 2  парсер имени блока                       [чистое ядро]
;;;   Раздел 3  модель чертежа и сканирование            [чистое ядро]
;;;   Раздел 4  планирование интеграции                  [чистое ядро]
;;;   Раздел 5  валидация                                [чистое ядро]
;;;   Раздел 6  контракт адаптера БД
;;;   Раздел 7  шаги интеграции и оркестрация            [чистое ядро + адаптер]
;;;   Раздел 8  исполняющий слой AutoCAD (COM / entget)
;;;
;;; Разделы 2-5 и 7 не обращаются к AutoCAD напрямую: они работают с моделью
;;; чертежа в виде списков и с функциями-адаптерами KG_DB*/KG_EX*. Благодаря
;;; этому вся логика исполняется тестами (tests/run_tests.py) без AutoCAD.
;;;===========================================================================

(if (not (boundp 'KG-TESTING)) (setq KG-TESTING nil))

;; Удалять ли старые определения после перевода ссылок на новые.
;; По умолчанию nil: по п. 8.4 и 14.4 ТЗ неиспользуемые определения
;; автоматически не удаляются.
(if (not (boundp 'KG-DELETE-OLD-DEFS)) (setq KG-DELETE-OLD-DEFS nil))

;; INTCLEANUP по умолчанию работает молча: снимает только то, что нигде
;; не вставлено, поэтому спрашивать не о чем. Если нужно подтверждение --
;; (setq KG-CLEANUP-ASK t) до запуска команды.
(if (not (boundp 'KG-CLEANUP-ASK)) (setq KG-CLEANUP-ASK nil))

;; Очистка определений прошлой интеграции в конце INTEGRATE. По
;; умолчанию включена: снимается только то, что нигде не вставлено, а
;; PURGE вставленное не трогает по построению. Отключается
;; (setq KG-AUTOCLEAN nil) -- тогда остаётся ручной INTCLEANUP.
(if (not (boundp 'KG-AUTOCLEAN)) (setq KG-AUTOCLEAN t))

;; Удалять ли свои определения самим, через vla-Delete. По умолчанию НЕТ:
;; на чертеже со 100 заменёнными экземплярами удаление динамического
;; определения «Комплект КП50 v1.5 стойка КП45302-1», у которого 281
;; анонимное представление, дало 0xC0000005 -- причём не в момент
;; удаления, а на следующем проходе по таблице блоков. PURGE снимает те
;; же определения своим механизмом и проверен на реальном чертеже.
;; Включается для диагностики: (setq KG-CLEANUP-DELETE t).
(if (not (boundp 'KG-CLEANUP-DELETE)) (setq KG-CLEANUP-DELETE nil))

;; Стирать ли сиротские анонимные представления (*U) самим, через ENTD.
;; По умолчанию ДА. Повод -- реальный прогон: PURGE снял 110 определений,
;; но 180 сиротских *U остались на месте, и вместе с ними остались шесть
;; старых вложенных определений «...~до»: PURGE не заглядывает внутрь
;; анонимного представления, поэтому для него «kps 920~до» выглядит
;; использованным, а сами *U он не снимает вообще. Стирание *U с нулём
;; вставок открывает эти определения, и их уносит следующий PURGE.
;; Отключается (setq KG-CLEANUP-ERASE-U nil) -- тогда остаётся только
;; PURGE, как было в сборке 36.
(if (not (boundp 'KG-CLEANUP-ERASE-U)) (setq KG-CLEANUP-ERASE-U t))

;; Куда смотрело чтение состояний вложенных блоков у последнего экземпляра:
;; "*U" -- анонимное представление, "ОПРЕДЕЛЕНИЕ" -- родительское
;; определение (представления у вставки нет), "НЕТ" -- вложенных вставок не
;; найдено. Нужен отчёту: по одному нулю нельзя отличить «состояний не
;; было» от «искали не там».
(if (not (boundp 'KG-LAST-NESTED-SRC)) (setq KG-LAST-NESTED-SRC "НЕТ"))

;; Печатать ли промежуточные шаги. Включается командой INTEGRATE:
;; когда что-то падает внутри AutoCAD, по последнему напечатанному шагу
;; видно, где именно.
(if (not (boundp 'KG-VERBOSE)) (setq KG-VERBOSE nil))

;; Какое имя мастер-версии сейчас переименовано и в какое (пара
;; старое . временное) либо nil. Нужна обработчику ошибок: если команда
;; упадёт между освобождением имени и завершением, определение надо
;; вернуть обратно, иначе в списке блоков останется "ИМЯ~доN".

;; Номер сборки. Печатается при загрузке и в начале INTEGRATE: по нему
;; видно, тот ли файл загружен в AutoCAD.
(if (not (boundp 'KG-MASTER-FREED)) (setq KG-MASTER-FREED nil))

;; Переименования, выполненные командой C:INTEGRATE ДО вставки из буфера.
;; KG-Integrate_Model в режиме вставки берёт их отсюда: освобождать имена
;; после вставки уже поздно, а переименовывать пришедшие определения
;; нельзя -- содержимое снова стало бы старым.
(if (not (boundp 'KG-PRERENAMES)) (setq KG-PRERENAMES nil))

;; Снимок таблицы блоков, сделанный C:INTEGRATE ДО освобождения имён.
(if (not (boundp 'KG-PREDEFS)) (setq KG-PREDEFS nil))

;; Определения, которые реально пришли из буфера.
(if (not (boundp 'KG-ARRIVED)) (setq KG-ARRIVED nil))

;; Сколько имён освобождено ДО вставки и какие из них не удалось
;; переименовать. Без этих чисел отчёт не отличает «имена не
;; освобождались» от «освобождались, но буфер ничего не принёс».
(if (not (boundp 'KG-FREED-N)) (setq KG-FREED-N 0))
(if (not (boundp 'KG-PRE-CANDIDATES)) (setq KG-PRE-CANDIDATES 0))
(if (not (boundp 'KG-PRENAME-FAILED)) (setq KG-PRENAME-FAILED nil))

;; Причины, по которым не удалось получить интерфейс ObjectDBX.
(if (not (boundp 'KG-DBX-ERR)) (setq KG-DBX-ERR nil))

;; Что стало с определением мастер-версии после вставки из буфера.
;; Проверяется по именам и по handle: PASTECLIP молча игнорирует
;; конфликтующее определение, и тогда мастер остался бы СТАРЫМ.
(if (not (boundp 'KG-MASTER-STATE)) (setq KG-MASTER-STATE nil))

;; Сколько экземпляров КАЖДОГО варианта уже было на новой итерации ДО
;; замены: (вариант . число). Без этой поправки отчёт сравнивает «стало»
;; со числом только СТАРЫХ экземпляров и показывает ложное расхождение.
(if (not (boundp 'KG-ALREADYNEW)) (setq KG-ALREADYNEW nil))

;; Кто держит ссылки на переименованные определения. Переименование через
;; vla-put-Name правит группу 2 у вставок сам, но на реальном чертеже после
;; прогона осталось 31 определение «имя~до» с нулём вставок, и PURGE их не
;; снял. Значит, часть ссылок AutoCAD не обновил -- в первую очередь внутри
;; других определений и внутри анонимных представлений. Карта держателей
;; собирается обходом содержимого и кэшируется: пар переименований десятки,
;; а обход один.
(if (not (boundp 'KG-REWHOLD)) (setq KG-REWHOLD nil))
;; Пары «старое имя -> служебное», которые ещё не перепривязаны:
;; ((старое служебное) ...).
(if (not (boundp 'KG-REWPAIRS)) (setq KG-REWPAIRS nil))
;; Имена, чьи вставки оставлены как есть из-за риска зациклить определение.
(if (not (boundp 'KG-REWSKIP)) (setq KG-REWSKIP nil))

;; Состояния видимости вложенных блоков внутри определений, снятые ДО
;; освобождения имён: ((имя . ((вложенный . состояние) ...)) ...).
;; Подменяемое определение теряет состояния вложенных блоков -- они
;; сбрасываются в значения по умолчанию новой версии.
(if (not (boundp 'KG-NESTSAVE)) (setq KG-NESTSAVE nil))
(if (not (boundp 'KG-NESTSAVEN)) (setq KG-NESTSAVEN 0))
;; Сколько вложенных вставок при снятии осталось без состояния видимости.
(if (not (boundp 'KG-NESTDROP)) (setq KG-NESTDROP 0))
(if (not (boundp 'KG-TECH-HANDLE)) (setq KG-TECH-HANDLE nil))

(setq KG-VERSION "70")

(vl-load-com)

;;;===========================================================================
;;; РАЗДЕЛ 1. УТИЛИТЫ
;;;===========================================================================

(defun KG-RemoveAll (x lst)
  (vl-remove-if '(lambda (e) (equal e x)) lst)
)

;; Уникальные элементы с сохранением порядка
(defun KG-Unique (lst / out)
  (setq out nil)
  (foreach e lst
    (if (not (member e out)) (setq out (cons e out)))
  )
  (reverse out)
)

;; Сравнение строк без учёта регистра; nil трактуется как пустая строка
(defun KG-StrEq (a b)
  (= (strcase (KG-AsString a)) (strcase (KG-AsString b)))
)

;;; Число в строку для отчёта.
;;; (itoa nil) в AutoCAD -- это «неверный тип аргумента: fixnump: nil»;
;;; в отчёте такое недопустимо, поэтому все счётчики идут через неё.
(defun KG-NumStr (x)
  (itoa (fix (KG-AsNum x 0)))
)

;;; Приведение к строке.
;;; Из COM иногда приходит не строка, а variant или имя объекта; любая
;;; строковая функция на таком значении падает с "неверный тип аргумента:
;;; stringp". Одна точка приведения убирает этот класс ошибок целиком.
;; Метка шага: запоминается всегда, печатается только при (setq KG-TRACE t).
;; Если команда упадёт, обработчик ошибок назовёт объект, на котором это
;; случилось, -- поэтому метки стоят даже там, где печать не нужна.
(defun KG-Mark (s)
  (setq KG-LAST-STEP (KG-AsString s))
  (if KG-TRACE (princ (strcat "\n[шаг] " KG-LAST-STEP)))
)

;; Подробная печать шагов. KG-VERBOSE включается командами, KG-TRACE
;; пользователь ставит сам перед запуском -- (setq KG-TRACE t) -- когда
;; нужно увидеть каждый шаг. Без отдельного флага подробные метки
;; переполняли командную строку AutoCAD и хвост сообщения терялся.
(if (not (boundp 'KG-TRACE)) (setq KG-TRACE nil))
(defun KG-Trace (s)
  (setq KG-LAST-STEP (KG-AsString s))
  (if (or KG-VERBOSE KG-TRACE) (princ (strcat "\n[шаг] " KG-LAST-STEP)))
)

;;; Второй уровень трассировки: строки, которых на большом чертеже сотни.
;;;
;;; Повод -- переход на реципиента со 100 блоками. Один экземпляр даёт
;;; строку на каждое вложенное определение (их 83), на каждое поле
;;; вхождения (9) и на каждое определение в таблице (их больше 100). Лог
;;; не помещался в сообщение, и присылали только хвост -- а причина всегда
;;; оказывалась в начале.
;;;
;;; Последнее действие запоминается ВСЕГДА, даже когда строка не
;;; печатается: место падения по «Последний шаг» определяется и без
;;; подробной печати. Включается отдельно -- (setq KG-TRACE-DETAIL t).
(if (not (boundp 'KG-TRACE-DETAIL)) (setq KG-TRACE-DETAIL nil))
;;; Определения, которые ActiveX не отдал при чтении модели чертежа.
;;; Список, а не счётчик: имена нужны, чтобы понять, не потеряно ли
;;; среди них нужное вложенное определение.
(if (not (boundp 'KG-SKIP-DEFS)) (setq KG-SKIP-DEFS nil))
;;; Сколько определений прочитано в модель чертежа. Отдельная переменная,
;;; а не часть печатаемой строки: прогон сборки 57 показал, что строка из
;;; четырёх вызовов под одним перехватом не говорит, какой из них отказал.
(if (not (boundp 'KG-DEFCOUNT)) (setq KG-DEFCOUNT nil))
;;; Число прочитанных вхождений. Та же причина, что у KG-DEFCOUNT:
;;; значение хранится отдельно от строки, которая его печатает.
(if (not (boundp 'KG-INSTCOUNT)) (setq KG-INSTCOUNT nil))
;;; Получилось ли напечатать счётчик. Отдельный флаг, потому что условие
;;; по значению в LISP не работает: любое непустое значение истинно, и
;;; (not (KG-Safe ...)) не отличало отказ от успеха. Прогон сборки 58
;;; печатал предупреждение сразу после удачно напечатанного счётчика.
(if (not (boundp 'KG-CNT-OK)) (setq KG-CNT-OK nil))
;;; Обход карты ссылок на крупном чертеже: сколько объектов пройдено и
;;; кэш «handle владельца -> имя определения». Прогон на 1231 вставке
;;; оборвался с 0xC0000005, и без счётчика не было видно, дошёл ли обход
;;; до конца; а handent на каждом объекте -- лишний риск и лишняя работа.
(if (not (boundp 'KG-REFDONE)) (setq KG-REFDONE 0))
(if (not (boundp 'KG-REFCACHE)) (setq KG-REFCACHE nil))

;;; Шаг и причина ПОСЛЕДНЕГО отказа, перехваченного в KG-Safe.
;;; Отдельно от KG-LAST-STEP: тот перезаписывается каждым удачным шагом,
;;; и предупреждение, напечатанное позже, называло место, пройденное ДО
;;; отказа. Прогон сборки 56 читался именно так.
(if (not (boundp 'KG-LAST-STEP)) (setq KG-LAST-STEP nil))
(if (not (boundp 'KG-FAIL-STEP)) (setq KG-FAIL-STEP nil))
(if (not (boundp 'KG-FAIL-MSG)) (setq KG-FAIL-MSG nil))

;;; Текст причины отказа для предупреждения: шаг и сообщение вместе.
(defun KG-FailText ( / s)
  (setq s "")
  (if (and KG-FAIL-STEP (/= KG-FAIL-STEP ""))
    (setq s (strcat "шаг \"" KG-FAIL-STEP "\""))
  )
  (if (and KG-FAIL-MSG (/= KG-FAIL-MSG ""))
    (setq s (if (= s "") KG-FAIL-MSG (strcat s ", " KG-FAIL-MSG)))
  )
  (if (= s "") (setq s "(не записано)"))
  s
)
(defun KG-TraceDetail (s)
  (setq KG-LAST-STEP (KG-AsString s))
  (if KG-TRACE-DETAIL (princ (strcat "\n[шаг] " KG-LAST-STEP)))
)

(defun KG-AsString (x)
  (cond
    ((null x) "")
    ((= (type x) (type "")) x)
    ((= (type x) (type 0)) (itoa x))
    ((= (type x) (type 0.0)) (rtos x 2 6))
    (t (vl-catch-all-apply '(lambda () (vl-princ-to-string x))))
  )
)

;; Число ли значение. Проверяется только «явно число»: type у variant
;; возвращает nil, и любое сравнение с ним должно быть безопасно.
(defun KG-Num (x / t1 t2)
  (setq t1 (KG-Safe '(lambda () (type x)) nil))
  (setq t2 (KG-Safe '(lambda () (type 0)) nil))
  (if (or (and t1 t2 (equal t1 t2))
          (and t1 (equal t1 (KG-Safe '(lambda () (type 0.0)) nil))))
    t
    nil)
)

;;; Числовое приведение.
;;; itoa, rtos и арифметика в AutoCAD на nil падают с «неверный тип
;;; аргумента: fixnump: nil». Отдельная точка приведения убирает класс.
(defun KG-AsNum (x def / n)
  (setq n (if (KG-Num x) x nil))
  (if n n (if (KG-Num def) def 0.0))
)

;;; Список из значения COM: список остаётся списком, variant
;;; разворачивается через vlax-variant-value.
;;; ВАЖНО: (nth i variant) в AutoCAD даёт «неверный тип аргумента:
;;; consp #<variant …>» -- именно так падало чтение каждого вхождения.
;;; Поэтому nth к значениям COM здесь не применяется нигде.
(defun KG-ListValue (v / a n i out x)
  (cond
    ((or (null v) (KG-IsErr v)) nil)
    ((equal (KG-Safe '(lambda () (type v)) nil) (type (list nil))) v)
    (t
     (setq a (vl-catch-all-apply '(lambda () (vlax-variant-value v))))
     (if (KG-IsErr a)
       nil
       (progn
         (setq n (vl-catch-all-apply '(lambda () (vlax-safearray-get-u-bound a 1))))
         (if (not (KG-Num n))
           nil
           (progn
             (setq out nil i 0)
             (while (<= i n)
               (setq x (vl-catch-all-apply
                         '(lambda () (vlax-safearray-get-element a i))))
               (setq out (cons (if (KG-IsErr x) nil x) out))
               (setq i (1+ i))
             )
             (reverse out))))))
  )
)

;;; Точка (X Y Z) из значения COM. Всегда список из трёх чисел.
(defun KG-PointValue (v def / lst dx dy dz)
  (setq lst (KG-ListValue v))
  (if (or (null lst) (< (length lst) 3))
    (progn
      (setq lst (KG-ListValue def))
      (if (or (null lst) (< (length lst) 3))
        (list 0.0 0.0 0.0)
        (progn
          (setq dx (car lst))
          (setq dy (car (KG-Safe '(lambda () (cdr lst)) nil)))
          (setq dz (car (KG-Safe '(lambda () (cdr (cdr lst))) nil)))
          (list (KG-AsNum dx 0.0) (KG-AsNum dy 0.0) (KG-AsNum dz 0.0))))
    )
    (progn
      (setq dx (car lst))
      (setq dy (car (KG-Safe '(lambda () (cdr lst)) nil)))
      (setq dz (car (KG-Safe '(lambda () (cdr (cdr lst))) nil)))
      (list (KG-AsNum dx 0.0) (KG-AsNum dy 0.0) (KG-AsNum dz 0.0))
    )
  )
)

;;; Выполнить операцию; при ошибке -- запасное значение.
;;;
;;; ВАЖНО: сюда годится только КВОТИРОВАННАЯ лямбда '(lambda () ...) --
;;; она видит локальные переменные места вызова. Список-вызов сюда
;;; НЕЛЬЗЯ ни в каком виде: vl-catch-all-apply принимает первый
;;; аргумент как ФУНКЦИЮ, поэтому '(entdel e) и (list 'entdel e) -- это
;;; попытка вызвать список, ошибка, и запасное значение вместо
;;; результата. Если нужен вычисляемый вызов без лямбды -- звать
;;; vl-catch-all-apply напрямую: (vl-catch-all-apply 'entdel (list e)).
;;; Сборка 37 наступила на это: стирание *U отвечало nil при живом entdel.
;;; Последний шаг и причина последнего отказа объявляются ДО KG-Safe:
;;; ветка отказа читает KG-LAST-STEP, и без объявления первый же
;;; перехваченный отказ падал с «не определена переменная: KG-LAST-STEP».
(if (not (boundp 'KG-LAST-STEP)) (setq KG-LAST-STEP nil))
(if (not (boundp 'KG-FAIL-STEP)) (setq KG-FAIL-STEP nil))
(if (not (boundp 'KG-FAIL-MSG)) (setq KG-FAIL-MSG nil))

(defun KG-Safe (fn def / r msg)
  ;; Прошлый отказ сбрасывается ДО операции. Прогон сборки 58: строка
  ;; «[счёт] определений в списке: 57» напечаталась, а следом пошло
  ;; «ВНИМАНИЕ: не напечаталось число определений (шаг "счёт вхождений",
  ;; неверная функция: T), значение: 57» -- причина от одного отказа,
  ;; значение от другого, в одной строке.
  (setq KG-FAIL-STEP nil)
  (setq KG-FAIL-MSG nil)
  (setq r (vl-catch-all-apply fn))
  (if (KG-IsErr r)
    (progn
      ;; Шаг, на котором произошёл отказ, запоминается ОТДЕЛЬНО.
      ;;
      ;; Прогон сборки 56: «ВНИМАНИЕ: снимок чертежа не прочитан (таблица
      ;; блоков прочитана)». Причина отказа печаталась как ПОСЛЕДНИЙ шаг,
      ;; но KG-Trace и KG-Mark перезаписывают KG-LAST-STEP на каждом
      ;; удачном шаге, поэтому к моменту печати там стоял шаг, прошедший
      ;; ДО отказа. Сообщение называло не то место и уводило в сторону.
      (setq KG-FAIL-STEP (KG-AsString KG-LAST-STEP))
      (setq msg (KG-AsString
                  (vl-catch-all-apply
                    '(lambda () (vl-catch-all-error-message r)))))
      (if (or (null msg) (= msg "")) (setq msg "(причина не прочитана)"))
      (setq KG-FAIL-MSG msg)
      def
    )
    r
  )
)

;;; Вызов с ВЫЧИСЛЯЕМЫМИ аргументами под перехватом. Отдельная функция
;;; потому, что KG-Safe принимает только лямбду: vl-catch-all-apply
;;; трактует первый аргумент как функцию, и список-вызов вместо вызова
;;; даёт ошибку и запасное значение.
(defun KG-Call (fn args def / r)
  (setq r (vl-catch-all-apply fn args))
  (if (KG-IsErr r) def r)
)

;;; Результат vl-catch-all-apply -- ошибка?
;;; Определена в разделе 1, а не в слое AutoCAD: утилиты приведения типов
;;; нужны и на тестовом стенде, где раздел 8 не загружается.
(defun KG-IsErr (x) (vl-catch-all-error-p x))

;;; Безопасный вариант проверки результата COM-вызова.
;;;
;;; (vl-catch-all-error-p) от живого vla-объекта на реальном чертеже сам
;;; бросает «неверная функция: T». Поэтому там, где на вход может прийти
;;; что угодно, проверку тоже надо перехватывать: иначе отказ проверки
;;; обрывает команду, и последняя метка указывает на уже пройденное место.
;;; Отказ проверки трактуется как «результат непригоден».
(defun KG-IsErrSafe (x def / r)
  (setq r (KG-Safe '(lambda () (vl-catch-all-error-p x)) def))
  r
)

;;; Логическое значение, пришедшее из COM.
;;;
;;; AutoCAD возвращает :vlax-true / :vlax-false, и ОБА этих символа в
;;; условии истинны: (if :vlax-false t nil) даёт T. Именно так каждое
;;; определение чертежа получало флаг «внешняя ссылка», KG-UserDefNames
;;; возвращал пустой список, и освобождать перед вставкой оказывалось
;;; нечего -- 40 вложенных определений оставались старыми.
(defun KG-ComBool (v / s)
  (cond
    ((null v) nil)
    ((KG-IsErr v) nil)
    (t
     (setq s (strcase (vl-princ-to-string v)))
     (cond
       ((= s ":VLAX-FALSE") nil)
       ((= s ":VLAX-TRUE") t)
       ((= (type v) (type 0)) (/= v 0))
       (t t)
     )
    )
  )
)

;;; Прочитать поле объекта; при ошибке -- запасное значение.
;;; Ни одно поле не должно ронять чтение всего экземпляра.
(defun KG-Field (obj prop def / r)
  (setq r (vl-catch-all-apply '(lambda () (vlax-get-property obj prop))))
  (if (or (KG-IsErr r) (null r)) def r)
)

;; Содержит ли строка подстроку (без учёта регистра)
(defun KG-StrContains (s sub / i n m)
  (setq s (KG-AsString s))
  (setq sub (KG-AsString sub))
  (if (= sub "")
    nil
    (progn
      (setq n (strlen s))
      (setq m (strlen sub))
      (setq i 1)
      (while (and (> i 0) (<= i (1+ (- n m))))
        (if (= (strcase (substr s i m)) (strcase sub))
          (setq i (- -1 i))                    ; нашли: выходим с i < 0
          (setq i (1+ i))
        )
      )
      (< i 0)
    )
  )
)

(defun KG-TrimLeft (s / n i)
  (setq s (KG-AsString s))
  (if (= s "") ""
    (progn
      (setq n (strlen s))
      (setq i 1)
      (while (and (<= i n) (= (substr s i 1) " ")) (setq i (1+ i)))
      (if (> i n) "" (substr s i))
    )
  )
)

(defun KG-TrimRight (s / n)
  (setq s (KG-AsString s))
  (if (= s "") ""
    (progn
      (setq n (strlen s))
      (while (and (> n 0) (= (substr s n 1) " ")) (setq n (1- n)))
      (if (= n 0) "" (substr s 1 n))
    )
  )
)

(defun KG-TrimBoth (s) (KG-TrimLeft (KG-TrimRight s)))

(defun KG-StrKey (s) (strcase (KG-AsString s)))

;; Разность списков строк без учёта регистра: a \ b
(defun KG-StrDiffCI (a b / out)
  (setq out nil)
  (foreach e a
    (if (not (vl-some '(lambda (x) (KG-StrEq e x)) b))
      (setq out (cons e out))
    )
  )
  (reverse out)
)

;; Пересечение списков строк без учёта регистра
(defun KG-StrInterCI (a b / out)
  (setq out nil)
  (foreach e a
    (if (vl-some '(lambda (x) (KG-StrEq e x)) b)
      (setq out (cons e out))
    )
  )
  (reverse out)
)

;; Поиск пары в списке ассоциаций по строковому ключу без учёта регистра
(defun KG-AssocCI (key alst)
  (vl-some '(lambda (p) (if (KG-StrEq (car p) key) p nil)) alst)
)

;; Значение по ключу. ВНИМАНИЕ: nil возвращается и когда ключа нет,
;; и когда значение равно nil. Для проверки наличия ключа -- KG-HasKeyCI.
(defun KG-CdrCI (key alst / p)
  (setq p (KG-AssocCI key alst))
  (if p (cdr p) nil)
)

;; Есть ли ключ в списке ассоциаций (работает и со значением nil)
(defun KG-HasKeyCI (key alst)
  (if (KG-AssocCI key alst) t nil)
)

;; Логический флаг: T только если ключ есть и его значение не nil
(defun KG-FlagCI (key alst / p)
  (setq p (KG-AssocCI key alst))
  (if (and p (cdr p)) t nil)
)

;; Замена или добавление пары в списке ассоциаций
(defun KG-SetAssoc (key val alst / out done)
  (setq out nil done nil)
  (foreach p alst
    (if (KG-StrEq (car p) key)
      (progn (setq out (cons (cons key val) out)) (setq done t))
      (setq out (cons p out))
    )
  )
  (if (not done) (setq out (cons (cons key val) out)))
  (reverse out)
)

;; Свободное имя, не занятое в списке used
(defun KG-UniqueName (prefix used / n cand)
  (setq n 0 cand prefix)
  (while (member (KG-StrKey cand) (mapcar 'KG-StrKey used))
    (setq n (1+ n))
    (setq cand (strcat prefix "$" (itoa n) "$"))
  )
  cand
)

;; Анонимное определение (*U, *D, *X, *E, *A ...)
(defun KG-IsSystemBlockName (nm / s)
  (if (null nm)
    nil
    (progn
      (setq s (KG-AsString nm))
      (or (wcmatch (strcase s) "_*,`$*,*|*")
          (KG-IsAnonymousName s))
    )
  )
)

(defun KG-IsAnonymousName (nm)
  (and nm (= (substr (KG-AsString nm) 1 1) "*"))
)

;; Анонимное представление динамического блока: *U и только цифры.
;; Именно такие определения держат на себе старые вложенные блоки и
;; при этом сами никем не вставлены. *Model_Space, *Paper_Space, *D
;; и *X под это определение не попадают.
(defun KG-IsAnonDynName (nm / s i ok)
  (setq s (KG-AsString nm))
  (setq ok (and (> (strlen s) 2) (KG-StrEq (substr s 1 2) "*U")))
  (setq i 3)
  (while (and ok (<= i (strlen s)))
    (if (KG-StrContains "0123456789" (substr s i 1))
      (setq i (1+ i))
      (setq ok nil)
    )
  )
  ok
)

;; Служебное имя, недопустимое в финальном результате (п. 20 ТЗ).
;; Имена вида "Стойка~~14" сюда НЕ входят: это осмысленное имя, под которым
;; сохраняется старое определение, и оно не маскируется под результат.
(defun KG-IsServiceName (nm)
  (and nm
       (or (KG-IsAnonymousName nm)
           (wcmatch (strcase (KG-AsString nm))
                    "*$0$*,*$1$*,*$2$*,*_NEW,*-КОПИЯ")
       )
  )
)

;; Снять суффикс «~до». Старые определения переименовываются ДО чтения
;; старых экземпляров, поэтому вложенные вставки в них указывают уже на
;; «Стойка КП50~до», а в новом определении вложенный блок называется
;; «Стойка КП50». Без снятия суффикса ни одно состояние вложенного блока
;; не переносилось: «В новой версии такого вложенного блока нет».
(defun KG-StripDoSuffix (nm / s pos)
  (setq s (KG-AsString nm))
  (setq pos (vl-string-search "~до" s))
  (if pos
    (substr s 1 pos)
    s
  )
)

;; Одно и то же вложение до переименования и после. Имя -- то, что стоит
;; в группе 2 вложенной вставки; эталон -- имя вложенного блока в новом
;; определении.
(defun KG-NestedKeyMatch (nm key)
  (or (KG-StrEq nm key)
      (KG-StrEq (KG-StripDoSuffix nm) (KG-StripDoSuffix key)))
)

(defun KG-Say (s) (princ (strcat "\n" s)))
(defun KG-SayKV (k v) (princ (strcat "\n" (KG-AsString k) ": " (KG-AsString v))))

(defun KG-VarLabel (v) (strcat "\"" (if v v "") "\""))

;; Склейка имён в одну строку для сообщений
(defun KG-JoinNames (names / out)
  (setq out "")
  (foreach n names
    (setq out (strcat out (if (= out "") "" ", ") n))
  )
  out
)

;;;===========================================================================
;;; РАЗДЕЛ 2. ПАРСЕР ИМЕНИ БЛОКА
;;;
;;; Поддерживаются две схемы именования.
;;;
;;; 1. Производственная (основная):  БАЗА vВЕРСИЯ ВАРИАНТ
;;;      "Комплект КП50 v1.5"                    -> база "Комплект КП50",
;;;                                                 итерация "1.5", вариант ""
;;;      "Комплект КП50К v1.5"                   -> база "Комплект КП50К"
;;;      "Комплект КП50 v1.21 стойка КП45302-1"  -> вариант "стойка КП45302-1"
;;;    Версия -- всегда "v" + цифры через точку: v1.1, v1.5, v1.21, v2.0.
;;;    Буква v не обязана быть отделена от базы, но ДОЛЖНА отделяться
;;;    пробелом от варианта; берётся ПОСЛЕДНЯЯ подходящая метка версии.
;;;
;;; 2. Из технического задания:  БАЗА(ИТЕРАЦИЯ)ВАРИАНТ
;;;      "ABC1.01(15)Стойка" -> база "ABC1.01", итерация "15", вариант "Стойка"
;;;    Разбирается ПОСЛЕДНЯЯ закрывающая скобка, поэтому
;;;    "A(1)(2)Стойка" -> база "A(1)", итерация "2", вариант "Стойка".
;;;
;;; Возвращает (list BASE ITER VARIANT) либо nil, если имя не подходит.
;;; ITER -- всегда строка ("1.5", "15"). VARIANT = "" для мастер-версии.
;;;===========================================================================

(defun KG-IsDigitChar (ch)
  (member ch '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
)

;; Позиция последнего вхождения подстроки sub в name, либо 0
(defun KG-LastPos (name sub / i n m res)
  (setq name (KG-AsString name))
  (setq n (strlen name))
  (setq m (strlen sub))
  (setq res 0)
  (setq i (1+ (- n m)))
  (while (> i 0)
    (if (and (= res 0) (= (strcase (substr name i m)) (strcase sub)))
      (setq res i)
    )
    (setq i (1- i))
  )
  res
)

;; Схема 1: БАЗА vВЕРСИЯ ВАРИАНТ
(defun KG-ParseVName (name / n i j k ch num base ver var)
  (setq n (strlen name))
  (setq i (KG-LastPos name " v"))
  (if (= i 0)
    nil
    (progn
      ;; i -- позиция пробела, метка версии начинается с i+1
      (setq j (+ i 2))                         ; первая цифра версии
      (if (or (> j n) (not (KG-IsDigitChar (substr name j 1))))
        nil
        (progn
          ;; читаем цифры и точки до конца версии
          (setq k j)
          (while (and (<= k n)
                      (or (KG-IsDigitChar (substr name k 1))
                          (= (substr name k 1) ".")))
            (setq k (1+ k))
          )
          (setq num (substr name j (- k j)))
          ;; версия не должна заканчиваться точкой
          (if (= (substr num (strlen num) 1) ".")
            nil
            (progn
              (setq base (substr name 1 (1- i)))
              (setq ver num)
              ;; вариант отделён от версии пробелом
              (setq var (if (> k n) "" (substr name (1+ k))))
              ;; после версии обязателен пробел, иначе "v1.21стойка"
              ;; было бы прочитано как версия 1.21 и вариант "стойка"
              (if (and (> (strlen var) 0) (= (substr var 1 1) " "))
                nil
                (list (KG-TrimRight base) ver (KG-TrimLeft var))
              )
            )
          )
        )
      )
    )
  )
)

;; Схема 2: БАЗА(ИТЕРАЦИЯ)ВАРИАНТ
(defun KG-ParseParenName (name / i n digits base iter var)
  (if (or (= name nil) (= name ""))
    nil
    (progn
      (setq n (strlen name))
      (setq i n)
      (while (and (> i 0) (/= (substr name i 1) ")"))
        (setq i (1- i))
      )
      (if (= i 0)
        nil                                    ; закрывающей скобки нет
        (progn
          (setq digits nil)
          (setq i (1- i))
          (while (and (> i 0) (KG-IsDigitChar (substr name i 1)))
            (setq digits (cons (substr name i 1) digits))
            (setq i (1- i))
          )
          (if (or (null digits) (/= (substr name i 1) "("))
            nil                                ; нет "(ЦИФРЫ)" перед ')'
            (progn
              (setq base (substr name 1 (1- i)))
              (setq iter (apply 'strcat digits))
              (setq var (substr name (+ i (length digits) 2)))
              (if (= base "")
                nil
                (list base iter var)
              )
            )
          )
        )
      )
    )
  )
)

(defun KG-ParseBlockName (name / p s)
  (if (/= (type name) (type ""))
    nil
    (progn
      (setq s name)
      (setq p (KG-ParseVName s))
      (if (not p) (setq p (KG-ParseParenName s)))
      (if (not p)
        (if (and (/= s "")
                 (not (KG-IsAnonymousName s))
                 (not (KG-IsServiceName s))
                 (not (KG-IsSystemBlockName s))
                 (not (KG-StrContains s "~до")))
          (list s "" "")
          nil
        )
        p
      )
    )
  )
)

(defun KG-ParseBase      (name / p) (setq p (KG-ParseBlockName name)) (if p (nth 0 p) nil))
(defun KG-ParseIteration (name / p) (setq p (KG-ParseBlockName name)) (if p (nth 1 p) nil))
(defun KG-ParseVariant   (name / p) (setq p (KG-ParseBlockName name)) (if p (nth 2 p) nil))

(defun KG-IsFamilyName (name base / clean p)
  (setq clean (KG-StripDoSuffix name))
  (setq p (KG-ParseBlockName clean))
  (and p (KG-StrEq (nth 0 p) base))
)

(defun KG-IsIntegrationName (name base iter / p)
  (setq p (KG-ParseBlockName name))
  (and p (KG-StrEq (nth 0 p) base) (KG-IterEq (nth 1 p) iter))
)

;;;--- Итерации ------------------------------------------------------------
;;; Итерация хранится строкой: "1.5" для производственной схемы, "15" для
;;; схемы ТЗ. Сравнивается по частям, поэтому v1.21 старше v1.5, а не наоборот.

;; Ключ для сравнения: части версии дополняются нулями до 6 разрядов,
;; поэтому обычное сравнение строк даёт правильный порядок.
(defun KG-IterKey (iter / s parts out p k i n first)
  ;; Номер итерации пишется без нуля на конце, когда он круглый:
  ;; v1.5 -- это итерация 50, v1.51 -- 51, v1.2 -- 20, v1.21 -- 21.
  ;; Значит, одноразрядная часть после точки означает десятки. Без
  ;; этого v1.5 (50) оказывалась старше v1.21 (21), потому что 5 < 21.
  ;; Первой части правило не касается: это номер версии, не итерация.
  ;; Побочное следствие: v1.5 и v1.50 -- одна и та же итерация 50.
  (setq s (if iter (KG-TrimBoth (strcase (KG-IterToStr iter))) ""))
  (if (= (substr s 1 1) "V") (setq s (substr s 2)))
  (setq parts (KG-SplitByDot s))
  (setq out "")
  (setq first t)
  (foreach p parts
    (setq i (atoi p))
    (if (and (not first) (= (strlen (KG-TrimBoth p)) 1))
      (setq i (* i 10))
    )
    (setq first nil)
    (setq k (itoa i))
    (setq n (strlen k))
    (while (< n 6) (setq k (strcat "0" k)) (setq n (1+ n)))
    (setq out (strcat out "." k))
  )
  out
)

(defun KG-SplitByDot (s / out cur i n ch)
  (setq out nil cur "" n (strlen s))
  (setq i 1)
  (while (<= i n)
    (setq ch (substr s i 1))
    (if (= ch ".")
      (progn (setq out (cons cur out)) (setq cur ""))
      (setq cur (strcat cur ch))
    )
    (setq i (1+ i))
  )
  (reverse (cons cur out))
)

;; Равенство итераций. Для схемы ТЗ сравниваем как числа, чтобы 05 и 5
;; считались одной итерацией; для версий -- по ключу.
;; Итерация в строку: может прийти и числом (схема ТЗ), и строкой версии
(defun KG-IterToStr (iter)
  (cond
    ((null iter) "")
    ((= (type iter) (type "")) iter)
    (t (itoa iter))
  )
)

(defun KG-IterEq (a b / ka kb)
  (setq ka (KG-IterKey a))
  (setq kb (KG-IterKey b))
  (if (and ka kb (= ka kb))
    t
    nil
  )
)

;; a строго старше b. Ключ -- строка, поэтому сравниваем посимвольно:
;; (< "…" "…") в AutoCAD падает с "неверный тип аргумента: fixnump".
(defun KG-IterOlder (a b)
  (KG-StrLt (KG-IterKey a) (KG-IterKey b))
)

;; Посимвольное сравнение строк: a строго меньше b.
;; Нужна потому, что операторы < > <= >= в AutoLISP числовые.
(defun KG-StrLt (a b / sa sb na nb i res found)
  (setq sa (KG-AsString a))
  (setq sb (KG-AsString b))
  (setq na (strlen sa))
  (setq nb (strlen sb))
  (setq i 1)
  (setq res nil)
  ;; found отдельным флагом: KG-CharLt может вернуть nil (a не меньше b),
  ;; и по одному res цикл не отличил бы «разница найдена» от «ещё идем»
  (setq found nil)
  (while (and (not found) (<= i na) (<= i nb))
    (if (= (substr sa i 1) (substr sb i 1))
      (setq i (1+ i))
      (progn
        (setq res (KG-CharLt (substr sa i 1) (substr sb i 1)))
        (setq found t)
      )
    )
  )
  ;; разница не найдена -- решает длина (префикс меньше)
  (if found res (< na nb))
)

;; Один символ строго меньше другого (по коду)
(defun KG-CharLt (a b)
  (< (KG-CharCode a) (KG-CharCode b))
)

;; Код символа (первый символ строки)
(defun KG-CharCode (s / n)
  (setq s (KG-AsString s))
  (setq n 1)
  (while (and (< n 65536) (/= (chr n) (substr s 1 1)))
    (setq n (1+ n))
  )
  (if (>= n 65536) 0 n)
)

;; a не совпадает с b -- этого достаточно, чтобы считать итерацию старой
(defun KG-IterOther (a b)
  (not (KG-IterEq a b))
)

;;; Сборка имени. Схема выбирается по виду итерации: с точкой -- значит
;;; производственная "БАЗА vВЕРСИЯ ВАРИАНТ", иначе -- "БАЗА(ИТЕРАЦИЯ)ВАРИАНТ".
(defun KG-MakeName (base iter var / s hasv hasi)
  (setq s (if iter (KG-TrimBoth (KG-IterToStr iter)) ""))
  (setq hasv (and var (/= var "")))
  (setq hasi (and iter (/= s "")))
  (cond
    ((and (not hasi) (not hasv)) base)
    ((and (not hasi) hasv) (strcat base " " var))
    ((KG-StrContains s ".")
     (strcat base " v" s (if hasv (strcat " " var) "")))
    (t
     (strcat base "(" s ")" (if hasv var "")))
  )
)

;;;===========================================================================
;;; РАЗДЕЛ 3. МОДЕЛЬ ЧЕРТЕЖА И СКАНИРОВАНИЕ
;;;
;;; model = (("defs" . (def ...)) ("insts" . (inst ...)) ("layouts" ...))
;;;
;;; def  = (("name" . "Имя") ("is-xref" . T/nil) ("is-layout" . T/nil)
;;;          ("is-dynamic" . T/nil) ("nested" . ("Имя1" ...))
;;;          ("vis-states" . ("A" "B")) ("vis-param" . "Видимость"))
;;;
;;; inst = (("handle" . "1F2") ("def" . "ИмяОпределения")
;;;          ("eff" . "ЭффективноеИмя") ("space" . "Model" | "Лист 1")
;;;          ("layer" . "0") ("pos" . (x y z)) ("rot" . 0.0)
;;;          ("sx" . 1.0) ("sy" . 1.0) ("sz" . 1.0) ("normal" . (0 0 1))
;;;          ("nested-vis" . (("Стойка" . "A"))) ("dyn-props" . (...)))
;;;===========================================================================

(defun KG-ModelDefs (model)    (cdr (assoc "defs" model)))
(defun KG-ModelInsts (model)   (cdr (assoc "insts" model)))
(defun KG-ModelLayouts (model) (cdr (assoc "layouts" model)))

(defun KG-FindDef (model name / hit)
  (setq hit nil)
  (foreach d (KG-ModelDefs model)
    (if (and (not hit) (KG-StrEq (KG-CdrCI "name" d) name)) (setq hit d))
  )
  hit
)

(defun KG-AllDefNames (model)
  (mapcar '(lambda (d) (KG-CdrCI "name" d)) (KG-ModelDefs model))
)

;; Пользовательские определения: не анонимные, не внешние ссылки, не листы
(defun KG-UserDefNames (model / out)
  (setq out nil)
  (foreach d (KG-ModelDefs model)
    (if (and (not (KG-FlagCI "is-xref" d))
             (not (KG-FlagCI "is-layout" d))
             (not (KG-IsAnonymousName (KG-CdrCI "name" d)))
             (not (KG-IsSystemBlockName (KG-CdrCI "name" d)))
             (not (KG-IsServiceName (KG-CdrCI "name" d))))
      (setq out (cons (KG-CdrCI "name" d) out))
    )
  )
  (reverse out)
)

;; Рекурсивно собрать вложенные определения (Этап 5).
;; Возвращает имена без дублей, не включая само root.
;; Анонимные определения пропускаются, внешние ссылки -- тоже (п. 3.3 ТЗ).
(defun KG-GetNestedBlocks (model root / seen out)
  (setq seen (list (KG-StrKey root)))
  (setq out nil)
  (defun KG--Walk (mdl nm / def)
    (setq def (KG-FindDef mdl nm))
    (if def
      (foreach c (KG-CdrCI "nested" def)
        (if (and (not (KG-IsAnonymousName c))
                 (not (member (KG-StrKey c) seen)))
          (progn
            (setq seen (cons (KG-StrKey c) seen))
            (setq out (cons c out))
            (KG--Walk mdl c)
          )
        )
      )
    )
  )
  (KG--Walk model root)
  (reverse out)
)

(defun KG-GetMasterDependencies (model mastername)
  (cons mastername (KG-GetNestedBlocks model mastername))
)

;; Все экземпляры семейства base во всём файле: (cons inst parsed)
(defun KG-FindFamilyInstances (model base / out p)
  (setq out nil)
  (foreach ins (KG-ModelInsts model)
    (setq p (KG-ParseBlockName (KG-CdrCI "eff" ins)))
    (if (and p (KG-StrEq (nth 0 p) base))
      (setq out (cons (cons ins p) out))
    )
  )
  (reverse out)
)

;; Старые экземпляры: любое вхождение семейства с итерацией, отличной от
;;; новой (п. 5 ТЗ). Сравнение «меньше» здесь неприменимо: в чертеже может
;;; оказаться и более поздняя итерация, и её трогать нельзя.
(defun KG-FindOldIterations (model base newiter / out it eff h th)
  (setq out nil)
  (setq th (if (boundp 'techhandle) techhandle KG-TECH-HANDLE))
  (foreach pr (KG-FindFamilyInstances model base)
    (setq it (nth 1 (cdr pr)))
    (setq h (KG-CdrCI "handle" (car pr)))
    (if (and (= (KG-AsString newiter) "") (= (KG-AsString it) ""))
      (if (not (and th (KG-StrEq h th)))
        (setq out (cons pr out))
      )
      (if (KG-IterOther it newiter)
        (setq out (cons pr out))
      )
    )
  )
  out
)

;; Группировка по варианту: (("Стойка" . (pr ...)) ("" . (pr ...)) ...)
(defun KG-GroupByVariant (pairs / keys out k)
  (setq keys nil)
  (foreach pr pairs
    (setq k (KG-StrKey (nth 2 (cdr pr))))
    (if (not (member k keys)) (setq keys (cons k keys)))
  )
  (setq keys (reverse keys))
  (setq out nil)
  (foreach k keys
    (setq out
      (cons
        (cons
          (vl-some '(lambda (pr)
                      (if (KG-StrEq (KG-StrKey (nth 2 (cdr pr))) k)
                        (nth 2 (cdr pr)) nil))
                   pairs)
          (vl-remove-if-not
            '(lambda (pr) (KG-StrEq (KG-StrKey (nth 2 (cdr pr))) k))
            pairs))
        out))
  )
  (reverse out)
)

(defun KG-FindOldVariants (groups) (mapcar 'car groups))

;; Сводка по пространствам: (("Model" . 13) ("Лист 1" . 6) ...)
(defun KG-CountBySpace (pairs / keys out k)
  (setq keys nil)
  (foreach pr pairs
    (setq k (KG-CdrCI "space" (car pr)))
    (if (not (member k keys)) (setq keys (cons k keys)))
  )
  (setq keys (reverse keys))
  (setq out nil)
  (foreach k keys
    (setq out
      (cons (cons k (length (vl-remove-if-not
                              '(lambda (pr) (equal (KG-CdrCI "space" (car pr)) k))
                              pairs)))
            out))
  )
  (reverse out)
)

;; Результат сканирования:
;;  family, newiter, groups, variants, spaces, total, iterations
(defun KG-ScanFamily (model base newiter / pairs groups its clean_its)
  (setq pairs (KG-FindOldIterations model base newiter))
  (setq groups (KG-GroupByVariant pairs))
  (setq its (KG-Unique (mapcar '(lambda (pr) (nth 1 (cdr pr))) pairs)))
  (setq clean_its (vl-remove-if '(lambda (x) (or (null x) (= (KG-AsString x) ""))) its))
  (list
    (cons "family" base)
    (cons "newiter" newiter)
    (cons "groups" groups)
    (cons "variants" (KG-FindOldVariants groups))
    (cons "spaces" (KG-CountBySpace pairs))
    (cons "total" (length pairs))
    (cons "iterations" (if clean_its (vl-sort clean_its 'KG-IterOlder) nil))
  )
)

;;;===========================================================================
;;; РАЗДЕЛ 4. ПЛАНИРОВАНИЕ ИНТЕГРАЦИИ
;;;===========================================================================

;; Карта вложенных определений (Этап 6):
;;   to-update -- имя есть и в мастер-версии, и в файле
;;   to-add    -- имя есть только в мастер-версии
(defun KG-MapNestedBlocks (model mastername / newnested existing)
  (setq newnested (KG-GetNestedBlocks model mastername))
  (setq existing (KG-UserDefNames model))
  (list
    (cons "to-update" (KG-StrInterCI newnested existing))
    (cons "to-add"    (KG-StrDiffCI  newnested existing))
  )
)

;;; Интерфейс ObjectDBX: невидимый документ в памяти, без файла на диске.
;; Одна попытка получить ObjectDBX двумя способами. Причина отказа
;; запоминается: без неё «ObjectDBX недоступен» -- это guesswork, а не
;; диагноз.
(defun KG-ObjectDbxTry (progid / app r)
  (setq app (vlax-get-acad-object))
  (setq r (vl-catch-all-apply 'vla-GetInterfaceObject (list app progid)))
  (if (KG-IsErr r)
    (progn
      (setq KG-DBX-ERR
        (cons (strcat progid " / GetInterfaceObject: "
                      (KG-AsString (vl-catch-all-error-message r)))
              KG-DBX-ERR))
      (setq r (vl-catch-all-apply 'vlax-create-object (list progid)))
      (if (KG-IsErr r)
        (progn
          (setq KG-DBX-ERR
            (cons (strcat progid " / create-object: "
                          (KG-AsString (vl-catch-all-error-message r)))
                  KG-DBX-ERR))
          nil
        )
        r
      )
    )
    r
  )
)

(defun KG-ObjectDbx ( / vrs dbx)
  (setq KG-DBX-ERR nil)
  (setq vrs (atoi (getvar "ACADVER")))
  (setq dbx (KG-ObjectDbxTry (strcat "ObjectDBX.AxDbDocument." (itoa vrs))))
  (if (not dbx)
    (setq dbx (KG-ObjectDbxTry "ObjectDBX.AxDbDocument"))
  )
  dbx
)

;; Что стало с определением мастер-версии после вставки из буфера.
;;
;; PASTECLIP молча игнорирует конфликтующее определение («Duplicate
;; definition of block ... ignored») и оставляет в чертеже СТАРОЕ. Тогда
;; вся интеграция идёт от устаревшего мастера, а по числу «пришло N»
;; этого не видно. Различаются три исхода:
;;   имени не было в чертеже      -- определение пришло новым;
;;   имя было и определение пришло -- старое перезаписано;
;;   имя было, но не пришло       -- мастер ОСТАЛСЯ СТАРЫМ.
;; Записать причину отказа в KG-DBX-ERR. Без этого «ObjectDBX недоступен»
;; не отличить от «интерфейс получен, но CopyObjects отказал».
;; Определения, которые можно удалить: ссылок нет, имя либо несёт наш
;; маркер «~до», либо разбирается схемой семейства и не является новейшей
;; итерацией.
;; Число ссылок по карте держателей: длина списка = количество вставок.
(defun KG-CleanupCounts (holders / out p)
  ;; Первый элемент может быть маркером успешной выборки ("OK"): карта
  ;; ссылок возвращается парой (маркер . карта), а пересчёт делается в
  ;; четырёх местах KG-CleanupRun, и в трёх из них маркер в сборке 39 не
  ;; снимался -- (length "OK") дал «неверный тип аргумента: consp "OK"»
  ;; сразу после стирания сиротских *U. Поэтому маркер отсекается здесь,
  ;; а не в каждом месте вызова.
  (if (KG-StrEq (KG-AsString (car holders)) "OK") (setq holders (cdr holders)))
  (setq out nil)
  (foreach p holders
    (setq out (KG-SetAssoc (car p) (length (cdr p)) out))
  )
  out
)

(defun KG-CleanupCandidates (counts names keep / cand nm c p)
  (setq cand nil)
  (foreach nm names
    (setq c (KG-AsNum (KG-CdrCI nm counts) 0))
    (setq p (KG-ParseBlockName nm))
    (if (and (< c 1)
             (not (KG-IsAnonymousName nm))
             (not (KG-IsSystemBlockName nm))
             (or (KG-StrContains nm "~до")
                 (and p (/= (nth 1 p) "") (not (KG-StrInterCI (list nm) keep)))))
      (setq cand (cons nm cand))
    )
  )
  (reverse cand)
)

;; Сиротские анонимные представления: их никто не вставляет, но они
;; держат на себе старые вложенные определения. Программа их не удаляет,
;; только показывает -- их снимает PURGE.
(defun KG-CleanupOrphans (counts names / out nm)
  ;; то же, что в KG-CleanupCounts: список может прийти с маркером
  (if (KG-StrEq (KG-AsString (car counts)) "OK") (setq counts (cdr counts)))
  (setq out nil)
  (foreach nm names
    (if (and (KG-IsAnonDynName nm)
             (< (KG-AsNum (KG-CdrCI nm counts) 0) 1))
      (setq out (cons nm out))
    )
  )
  (reverse out)
)

;; Ключ -- семейство И вариант, а не одно семейство: «Комплект КП50 v1.51»
;; и «Комплект КП50 v1.51 КП45387» принадлежат одному семейству и одной
;; итерации, но это два разных определения, и защищать надо оба.
(defun KG-CleanupKeepNewest (names / keep p k hit)
  (setq keep nil)
  (foreach nm names
    (setq p (KG-ParseBlockName nm))
    (if (and p (/= (nth 1 p) ""))
      (progn
        (setq k (strcat (nth 0 p) "|" (nth 2 p)))
        (setq hit (KG-AssocCI k keep))
        (if (not hit)
          (setq keep (cons (cons k nm) keep))
          (if (KG-IterOlder (nth 1 (KG-ParseBlockName (cdr hit))) (nth 1 p))
            (setq keep (KG-SetAssoc k nm keep))
          )
        )
      )
    )
  )
  (mapcar 'cdr keep)
)

;;;--- Сиротские анонимные представления ------------------------------------
;;;
;;; PURGE их не снимает: на реальном чертеже три прохода PURGE унесли 110
;;; определений, а 180 сиротских *U остались. Вместе с ними остались шесть
;;; старых вложенных определений «...~до»: PURGE внутрь анонимного
;;; представления не заглядывает, поэтому «kps 920~до» для него выглядит
;;; использованным. Стираем *U сами (ENTD по записи таблицы блоков) --
;;; вставок у сироты нет по построению, стирать нечего, -- и следующий
;;; PURGE уносит освободившиеся определения.

;; Имя определения, на которое указывает вставка. Пустая строка, если
;; ename не читается.
(defun KG_DefNameOfEnt (e / ed)
  (setq ed (KG-Safe '(lambda () (entget e)) nil))
  (if ed (KG-AsString (cdr (assoc 2 ed))) "")
)

;; Имена вставок внутри определения (вложенные блоки, в том числе
;; вложенные анонимные представления). Только entget: COM-обход
;; содержимого определений на реальном чертеже дал 0xC0000005.
(defun KG_DefEntNames (defname / e ed out)
  (setq out nil)
  (setq e (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
  (if e
    (while (and (setq e (KG-Safe '(lambda () (entnext e)) nil))
                (setq ed (KG-Safe '(lambda () (entget e)) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT")
        (setq out (cons (KG_DefNameOfEnt e) out))
      )
    )
  )
  (reverse out)
)

;; Карта «сиротское *U -> что внутри». Нужна для отчёта: по ней видно,
;; какие именно определения держат сироты и что освободится.
(defun KG-OrphanContents (orph / out nm inner)
  (setq out nil)
  (foreach nm orph
    (setq inner (KG-Safe '(lambda () (KG_DefEntNames nm)) nil))
    (setq out (cons (cons nm (KG-Unique inner)) out))
  )
  out
)

;;;--- Перепривязка ссылок на переименованные определения --------------------
;;
;; Задача: вносимый мастер-блок приносит более свежие версии блоков. Одноимённое
;; определение чертежа должно получить новое содержимое ПО ВСЕМУ чертежу -- не
;; только там, где его вставляют в модель, но и внутри чужих определений.
;;
;; Как это устроено. До вставки из буфера имя освобождается: определение
;; переименовывается в «имя~до<итерация>», и AutoCAD уводит за ним все свои
;; ссылки. Из буфера приходит определение под прежним именем -- с новым
;; содержимым. Дальше ссылки надо вернуть на прежнее имя: тогда и чужие
;; определения, и анонимные представления начинают указывать на свежую версию,
;; а «имя~до» остаётся без ссылок и его снимает PURGE.
;;
;; Без этого шага на реальном чертеже после прогона осталось 31 определение
;; «имя~до» с нулём вставок, и PURGE их не взял: значит, ссылки на них ещё были.

;; Есть ли внутри определения вставка указанного определения.
(defun KG-DefRefersTo (defname tgt / out)
  (setq out nil)
  (foreach nm (KG-Safe '(lambda () (KG_DefEntNames defname)) nil)
    (if (KG-StrEq nm tgt) (setq out t))
  )
  (if out t nil)
)

;; Кто держит определения с суффиксом «~до». Нужна отчёту: «осталось N
;; определений с ~до» без имён держателей не отличить от «ссылок нет, PURGE
;; их просто не берёт». Держатели берутся из готовой карты ссылок, поэтому
;; отдельного обхода таблицы блоков здесь нет.
(defun KG-DoHolderLines (holders names / out nm hh h x lines shown)
  (setq out nil)
  (setq lines 0)
  (foreach nm names
    (if (and (< lines 8) (KG-StrContains (KG-AsString nm) "~до"))
      (progn
        (setq hh (KG-CdrCI (KG-AsString nm) (KG-Unmark holders)))
        (if hh
          (progn
            ;; Три держателя на определение и восемь определений: на реальном
            ;; чертеже их десятки, а строка нужна как диагноз, а не как дамп.
            (setq shown "")
            (setq h 0)
            (foreach x hh
              (if (< h 3)
                (progn
                  (setq shown (strcat shown (if (= shown "") "" ", ")
                                      (KG-AsString x)))
                  (setq h (1+ h))
                )
              )
            )
            (setq out (cons (strcat "  \"" nm "\" держат: " shown) out))
            (setq lines (1+ lines))
          )
        )
      )
    )
  )
  (reverse out)
)

;; Какие определения вставляют данное определение. Обход идёт по ВСЕМ именам
;; таблицы, включая анонимные представления: именно там ссылки на «~до»
;; и остаются. Результат кэшируется в KG-REWHOLD по имени-ключу.
(defun KG-DefRefHolders (tgt / pr out nm names)
  (setq pr (assoc (KG-AsString tgt) KG-REWHOLD))
  (if pr
    (cdr pr)
    (progn
      (setq out nil)
      (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
      (foreach nm names
        ;; Исключается только само старое определение: вставка блока самого в
        ;; себя была бы петлёй. Новое определение держателем быть МОЖЕТ --
        ;; именно там лежит ссылка, которая возвращается на следующем проходе:
        ;; «Гайка» из буфера вставляет «Шайба~до», а «Шайба» становится
        ;; прежней только вместе с «Гайкой».
        (if (and nm (/= (KG-AsString nm) "")
                 (not (KG-StrEq nm (KG-AsString tgt))))
          (if (KG-Safe '(lambda () (KG-DefRefersTo nm tgt)) nil)
            (setq out (cons nm out))
          )
        )
      )
      (setq out (KG-Unique (reverse out)))
      (setq KG-REWHOLD
        (cons (cons (KG-AsString tgt) out) KG-REWHOLD))
      out
    )
  )
)

;; Как будет называться определение после перепривязки. Ищется в списке
;; пар текущего прохода (KG-REWPAIRS); без пары имя остаётся как есть.
;; К моменту, когда чертёж откроют, служебные ссылки уже возвращены на
;; прежние имена, поэтому проверка замыкания обязана смотреть на граф ПОСЛЕ
;; перепривязки, а не на текущий.
;;
;; Суффикс «~до» здесь НАМЕРЕННО не снимается по написанию: имя может
;; само по себе заканчиваться на «до», и обрезание дало бы другое
;; определение.
(defun KG-RewResolve (nm / k pr)
  (setq k (KG-AsString nm))
  (foreach pr KG-REWPAIRS
    (if (KG-StrEq (nth 1 pr) k) (setq k (KG-AsString (nth 0 pr))))
  )
  k
)

;; Замкнётся ли определение, если внутри holder вернуть ссылку на oldname.
;; Проверяется достижимость holder из newname по ссылкам вложенных блоков:
;; если holder достижим, ссылка «holder -> newname» замыкает цепочку.
;;
;; Обход в ширину, а не по первой вложенной ссылке: на реальном чертеже у
;; определения их десятки, и нужная почти никогда не первая. Прежняя версия
;; брала только первую -- на стенде она отвечала «не замкнётся» там, где
;; замыкание было, и шаг переписывал группу 2 у определения, которое после
;; этого ссылалось само на себя.
;;
;; Имена приводятся к виду после перепривязки (KG-RewResolve): иначе обход
;; упирался в островок старых определений, которые ссылаются друг на друга,
;; и до holder не доходил никогда.
;; Предел -- 400 определений: дальше это уже не проверка, а обход чертежа.
(defun KG-WouldCycle (holder oldname newname / front seen add cur key res
                                             inner x hit n)
  (setq hit nil)
  (setq res (KG-RewResolve holder))
  (setq front (list (KG-AsString newname)))
  (setq seen nil)
  (setq n 0)
  (while (and (not hit) front (< n 400))
    (setq cur (car front))
    (setq front (cdr front))
    (setq key (KG-RewResolve cur))
    (if (KG-StrEq key res)
      (setq hit t)
      (if (KG-StrInterCI (list key) seen)
        nil
        (progn
          (setq seen (cons key seen))
          (setq n (1+ n))
          (setq inner (KG-Safe '(lambda () (KG_DefEntNames cur)) nil))
          ;; Найденное собирается в ОТДЕЛЬНЫЙ список и добавляется к очереди
          ;; после обхода. Дописывать в саму очередь внутри foreach нельзя:
          ;; обход идёт по тому списку, который был передан, и добавленные
          ;; элементы теряются. Именно так первая версия проверки отвечала
          ;; «не замкнётся» там, где замыкание было.
          (setq add nil)
          (foreach x inner
            (if (and x
                     (not (KG-StrInterCI (list (KG-RewResolve x)) seen))
                     (not (KG-StrInterCI (list (KG-RewResolve x)) add)))
              (setq add (cons x add))
            )
          )
          (setq front (append front (reverse add)))
        )
      )
    )
  )
  (if hit t nil)
)

;; Вернуть ссылку внутри определения holder с oldname на newname.
;; Отдаёт число исправленных вставок.
(defun KG-RewireDefRefs (holder oldname newname / e ed n hit x newed)
  (setq n 0)
  (setq e (KG-Safe '(lambda () (KG-DefEnt holder)) nil))
  (if e
    (while (and (setq e (KG-Safe '(lambda () (entnext e)) nil))
                (setq ed (KG-Safe '(lambda () (entget e)) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT")
        (if (KG-StrEq (KG-AsString (cdr (assoc 2 ed))) oldname)
          (progn
            ;; Правится группа 2 -- имя вставляемого определения. Прочие
            ;; группы передаются обратно без изменений: entmod переписывает
            ;; сущность целиком. Список пересобирается в лоб, а не через
            ;; KG-SetAssoc: тот сравнивает ключи через KG-AsString, и число 2
            ;; у него не равно строке "2", которой assoc ищет группу в
            ;; AutoCAD. Ключ записывается числом, каким он и пришёл.
            (setq newed nil)
            (foreach x ed
              ;; Код группы сравнивается через KG-StrEq, а не через «=»:
              ;; у списка сущности могут встретиться записи, где в car не
              ;; число, и «=» на смешанных типах в AutoCAD поднимает ошибку.
              (if (KG-StrEq (car x) 2)
                (setq newed (cons (cons 2 (KG-AsString newname)) newed))
                (setq newed (cons x newed))
              )
            )
            (setq newed (reverse newed))
            (setq hit (KG-Safe '(lambda () (entmod newed)) nil))
            (if hit
              (progn
                (setq n (1+ n))
                (KG-Safe '(lambda () (entupd e)) nil)
              )
            )
          )
        )
      )
    )
  )
  n
)

;; Одно переименование: вернуть все найденные ссылки на прежнее имя.
(defun KG-RewireOnePair (oldname newname / holders h n skipped)
  (setq n 0)
  (setq skipped 0)
  (if (and oldname newname
           (/= (KG-AsString oldname) "")
           (not (KG-StrEq oldname newname)))
    (progn
      (KG-TraceDetail (strcat "перепривязка ссылок \"" oldname "\""))
      (setq holders (KG-Safe '(lambda () (KG-DefRefHolders oldname)) nil))
      (foreach h holders
        ;; Определения, которые входят в состав нового, не трогаются: ссылка
        ;; «новое -> старое» внутри цепочки замкнула бы определение.
        (if (KG-Safe '(lambda () (KG-WouldCycle h oldname newname)) nil)
          (progn
            (setq skipped (1+ skipped))
            (if (not (KG-StrInterCI (list oldname) KG-REWSKIP))
              (setq KG-REWSKIP (cons oldname KG-REWSKIP))
            )
          )
          (setq n (+ n (KG-Safe
                         '(lambda () (KG-RewireDefRefs h oldname newname)) 0)))
        )
      )
      (if (> skipped 0)
        (princ (strcat "\nВНИМАНИЕ: вставка \"" oldname "\" внутри \""
                       newname "\" оставлена как есть -- иначе определение "
                       "замкнулось бы само на себя."))
      )
    )
  )
  n
)

;; Перепривязать одну пару из списка. Отдельная функция, а не лямбда внутри
;; KG-Safe в теле цикла: лямбда, цитирующая локальную переменную цикла,
;; подставляется ненадёжно -- на стенде такой вызов молча отдавал запасное
;; значение, и шаг не перепривязывал ничего, не сообщая об отказе. Здесь
;; переменная r -- аргумент функции, и перехват стоит вокруг вызова, которому
;; она передана обычным образом.
(defun KG-RewirePairOf (r)
  ;; Порядок аргументов: сперва служебное имя «~до», затем прежнее. Пара в
  ;; renames лежит наоборот -- (прежнее служебное), и перестановка здесь
  ;; обязательна: первая версия передавала их как есть, держатели искались
  ;; по прежнему имени, не находились, и шаг молча не перепривязывал ничего.
  (KG-Safe '(lambda () (KG-RewireOnePair (nth 1 r) (nth 0 r))) 0)
)

;; Шаг 8б. Вернуть ссылки на прежние имена по всем парам переименования.
;; Проходит пары по кругу: за один проход часть ссылок ещё указывает на
;; служебное имя, которое само станет прежним только на следующем. Круг
;; прерывается, когда за проход не исправлено ни одной вставки, -- иначе две
;; пары, ссылающиеся друг на друга, крутились бы вечно.
(defun KG-Step_RewireDefs (renames / todo r done total n pairs guard)
  (setq KG-REWHOLD nil)
  (setq KG-REWSKIP nil)
  (setq total 0)
  (setq todo nil)
  (foreach r renames
    (if (and (nth 0 r) (nth 1 r)
             (/= (KG-AsString (nth 0 r)) "")
             (not (KG-StrEq (nth 0 r) (nth 1 r))))
      (setq todo (cons (list (nth 0 r) (nth 1 r)) todo))
    )
  )
  (setq todo (reverse todo))
  (setq guard 0)
  (while (and todo (< guard 12))
    (setq pairs todo)
    (setq KG-REWPAIRS todo)
    (setq todo nil)
    (setq done 0)
    (foreach r pairs
      (setq n (KG-AsNum (KG-RewirePairOf r) 0))
      (setq done (+ done n))
      (if (< n 1) (setq todo (cons r todo)))
    )
    (setq todo (reverse todo))
    (setq total (+ total done))
    (if (> done 0) (setq KG-REWHOLD nil))
    (setq guard (1+ guard))
  )
  ;; Остаток в списке -- это пары, которые за 12 проходов так ничего и не
  ;; исправили. Чаще всего это не отказ: переименование через vla-put-Name
  ;; уже уводит за именем ссылки AutoCAD, и возвращать нечего. Поэтому
  ;; предупреждение печатается только тогда, когда есть пропуски из-за риска
  ;; зациклить определение -- про них пользователь должен знать. В прогоне
  ;; на 43 парах здесь печатался список из 43 имён при нуле пропусков, и
  ;; читался он как авария там, где её не было.
  (if KG-REWSKIP
    (princ (strcat "\nОставлены как есть, иначе определение замкнулось бы: "
                   (KG-JoinNames (reverse KG-REWSKIP)))))
  (if todo
    (KG-TraceDetail (strcat "пар без найденных ссылок: "
                            (KG-NumStr (length todo)))))
  (setq KG-REWPAIRS nil)
  (KG-Mark (strcat "перепривязано ссылок на прежние имена: "
                   (KG-NumStr total)))
  total
)

;;;--- Видимость вложенных блоков внутри определений -------------------------
;;
;; Определение без итерации в имени («Стойка КП50», «Закладная в полость
;; стойки») подменяется целиком: старое переименовывается в «имя~до», из
;; буфера приходит новое под прежним именем. Содержимое при этом обновляется
;; верно, а вот состояния видимости вложенных блоков сбрасываются в значения
;; по умолчанию новой версии -- прогон это подтвердил: блоки с итерацией
;; видимость сохраняют (у них состояния переносятся по экземплярам), а блоки
;; без итерации теряют.
;;
;; Поэтому состояния читаются ДО освобождения имён и записываются обратно
;; после перепривязки.

;; Состояния видимости вложенных вставок одного определения. Определения без
;; вложенных вставок в список не попадают: на крупном чертеже их большинство,
;; а COM-чтение свойств -- самая дорогая часть обхода.
;;
;; Второй элемент результата -- сколько вложенных вставок осталось БЕЗ
;; состояния: у них либо нет параметра видимости, либо имя не прочиталось.
;; Без этого числа «снято 27 состояний» и «в определении 9 вложенных вставок,
;; состояний прочитано 9» нельзя свести с «вернулось 5», и непонятно, где
;; именно состояния потерялись -- при снятии или при возврате.
(defun KG-DefNestedVisOf (defname / inner states p out dropped)
  (setq inner (KG-Safe '(lambda () (KG_DefEntNames defname)) nil))
  (setq out nil)
  (setq dropped 0)
  (if inner
    (progn
      (setq states (KG-Safe '(lambda () (KG-DefNestedVisibility defname)) nil))
      (foreach p states
        (if (and (car p) (/= (KG-AsString (car p)) "") (cdr p)
                 (/= (KG-AsString (cdr p)) ""))
          (setq out (cons p out))
        )
      )
      ;; Считается от числа вложенных вставок, а не от числа пар без
      ;; состояния: чтение свойств может вообще не отдать пару для вставки,
      ;; у которой нет параметра видимости, и тогда пропуск был бы не виден.
      (setq dropped (- (length inner) (length out)))
      (if (< dropped 0) (setq dropped 0))
    )
  )
  (list (reverse out) dropped)
)

;; Снимок состояний по всем определениям чертежа. Делается ДО освобождения
;; имён: после переименования определение живёт под служебным именем, а
;; состояние нужно привязать к прежнему.
(defun KG-SaveNestedVis ( / names nm got states dropped n)
  (setq KG-NESTSAVE nil)
  (setq KG-NESTSAVEN 0)
  (setq KG-NESTDROP 0)
  (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
  (setq n 0)
  (foreach nm names
    (if (and (> (length names) 100) (= (rem n 100) 0))
      (KG-Mark (strcat "снимок состояний: пройдено " (itoa n) " из "
                       (itoa (length names))))
    )
    (setq n (1+ n))
    (if (and nm (/= (KG-AsString nm) ""))
      (progn
        (setq got (KG-DefNestedVisOf nm))
        (setq states (nth 0 got))
        (setq dropped (KG-AsNum (nth 1 got) 0))
        (setq KG-NESTDROP (+ KG-NESTDROP dropped))
        (if states
          (progn
            (setq KG-NESTSAVE (cons (cons (KG-AsString nm) states) KG-NESTSAVE))
            (setq KG-NESTSAVEN (+ KG-NESTSAVEN (length states)))
          )
        )
      )
    )
  )
  (setq KG-NESTSAVE (reverse KG-NESTSAVE))
  (KG-Mark (strcat "снимок состояний вложенных блоков: определений "
                   (KG-NumStr (length KG-NESTSAVE)) ", состояний "
                   (KG-NumStr KG-NESTSAVEN)
                   (if (> KG-NESTDROP 0)
                     (strcat ", без параметра видимости "
                             (KG-NumStr KG-NESTDROP))
                     "")))
  KG-NESTSAVE
)

;; Вернуть состояния одному определению. Отдаёт список из трёх чисел:
;; возвращено, не найдено в новом определении, не записалось.
;; Прочитать состояние обратно. Нужна отдельно: запись свойства через
;; vlax-put-property может вернуть успех и ничего не изменить -- вложение
;; лежит внутри определения, и AutoCAD не обязан применять к нему
;; динамическое свойство так же, как применяет к экземпляру в модели.
;; «Вернулось 5» без этой проверки означало «вызов прошёл», а не «состояние
;; стоит».
(defun KG-VerifyNestedVis (sub expected / v)
  (setq v (KG-Safe '(lambda () (KG-GetVisibilityState sub)) nil))
  (and v (KG-StrEq v expected))
)

;; Состояние одной вложенной вставки одним значением -- для подробной печати.
(defun KG-VisDetailOne (sub expect / v)
  (setq v (KG-AsString (KG-Safe '(lambda () (KG-GetVisibilityState sub)) "?")))
  (strcat "=\"" (KG-AsString expect) "\" -> \"" v "\"")
)

;; Вернуть состояния одному определению. Отдаёт список:
;;   0 -- возвращено и подтверждено чтением
;;   1 -- не найдено в новом определении
;;   2 -- записать не удалось
;;   3 -- записано, но чтение показало другое
;;   4 -- подробности: ("вложенный=\"прежнее\" -> \"стало\"" ...)
(defun KG-RestoreNestedVisOne (defname states / sub r ok nf bad bad2 st det)
  (setq ok 0 nf 0 bad 0 bad2 0 det nil)
  (foreach st states
    ;; Лямбды здесь ссылаются на АРГУМЕНТЫ функции, а не на переменные цикла
    ;; вызывающей функции: такие перехват подставляет.
    (setq sub (KG-Safe '(lambda () (KG-DefInsertByName defname (car st))) nil))
    (if (null sub)
      (setq nf (1+ nf))
      (progn
        (setq r (KG-Safe '(lambda () (KG-SetVisibilityState sub (cdr st))) nil))
        (if (null r)
          (setq bad (1+ bad))
          (if (KG-VerifyNestedVis sub (cdr st))
            (progn
              (setq ok (1+ ok))
              (setq det
                (cons (strcat (KG-AsString (car st))
                              (KG-VisDetailOne sub (cdr st)))
                      det))
            )
            (progn
              (setq bad2 (1+ bad2))
              (setq det
                (cons (strcat (KG-AsString (car st))
                              (KG-VisDetailOne sub (cdr st)) " НЕ ПРИНЯТО")
                      det))
            )
          )
        )
      )
    )
  )
  (list ok nf bad bad2 (reverse det))
)

;; Одно определение из списка переименований. Отдельная функция по той же
;; причине, что KG-RewirePairOf: лямбда, цитирующая локальную переменную
;; цикла, подставляется ненадёжно, и вызов молча отдаёт запасное значение.
;;
;; Шестой элемент результата -- имя определения, когда в него хоть что-то
;; возвращено: отчёт должен называть определения, а не только количество.
(defun KG-RestoreVisOfPair (r / nm st res p target plan_newiter)
  (setq nm (KG-AsString (nth 0 r)))
  (setq st (KG-CdrCI nm KG-NESTSAVE))
  (setq target nm)
  (if (not (KG_EXDefExists target))
    (progn
      (setq p (KG-ParseBlockName nm))
      (if (and p (boundp 'plan) plan)
        (progn
          (setq plan_newiter (cdr (assoc "newiter" plan)))
          (if (and plan_newiter (/= (KG-AsString plan_newiter) ""))
            (setq target (KG-MakeName (nth 0 p) plan_newiter (nth 2 p)))
          )
        )
      )
    )
  )
  (if (and st (KG_EXDefExists target))
    (progn
      (setq res (KG-RestoreNestedVisOne target st))
      (if (> (KG-AsNum (nth 0 res) 0) 0)
        (append res (list target))
        res
      )
    )
    (list 0 0 0 0 nil)
  )
)

(defun KG-FindVariantNestedStates (family variant plan / scan groups g pairs pr inst nv)
  (setq scan (cdr (assoc "scan" plan)))
  (setq groups (if scan (cdr (assoc "groups" scan)) nil))
  (setq g (if groups (assoc variant groups) nil))
  (if (not g)
    (if groups
      (foreach item groups
        (if (and (null g) (KG-StrEq (car item) variant)) (setq g item))
      )
    )
  )
  (setq pairs (if g (cdr g) nil))
  (setq nv nil)
  (foreach pr pairs
    (if (not nv)
      (progn
        (setq inst (car pr))
        (setq nv (KG-CdrCI "nested-vis" inst))
      )
    )
  )
  nv
)

;; Шаг 8в. Вернуть состояния видимости вложенных блоков определениям, которые
;; были подменены пришедшими из буфера.
;;
;; Обрабатываются только переименованные пары: определение, которого буфер не
;; принёс, вернулось на место нетронутым, писать в него нечего.
(defun KG-Step_RestoreNestedVis (renames / r res restored notfound failed
                                         unconf ndefs defs dets n)
  (setq restored 0 notfound 0 failed 0 unconf 0 ndefs 0)
  (setq defs nil dets nil)
  (setq n 0)
  (foreach r renames
    (if (and (> (length renames) 100) (= (rem n 100) 0))
      (KG-Mark (strcat "возврат состояний: пройдено " (itoa n) " из "
                       (itoa (length renames))))
    )
    (setq n (1+ n))
    (setq res (KG-RestoreVisOfPair r))
    (setq restored (+ restored (KG-AsNum (nth 0 res) 0)))
    (setq notfound (+ notfound (KG-AsNum (nth 1 res) 0)))
    (setq failed (+ failed (KG-AsNum (nth 2 res) 0)))
    (setq unconf (+ unconf (KG-AsNum (nth 3 res) 0)))
    (if (nth 5 res)
      (progn
        (setq ndefs (1+ ndefs))
        (setq defs (cons (nth 5 res) defs))
      )
    )
    (if (nth 4 res) (setq dets (append dets (nth 4 res))))
  )
  (KG-Mark (strcat "возвращено состояний вложенных блоков: "
                   (KG-NumStr restored) " в определениях "
                   (KG-NumStr ndefs)))
  (if (> notfound 0)
    (princ (strcat "\nВНИМАНИЕ: " (itoa notfound)
                   " состояний вложенных блоков не возвращены -- такого "
                   "вложенного блока в новой версии нет.")))
  (if (> failed 0)
    (princ (strcat "\nВНИМАНИЕ: " (itoa failed)
                   " состояний вложенных блоков записать не удалось.")))
  ;; Запись не подтвердилась чтением. Это главный признак того, что AutoCAD
  ;; принял вызов, но состояние не применил: без этой строки «вернулось 5»
  ;; означало бы «вызов прошёл», а на чертеже осталось значение мастер-блока.
  (if (> unconf 0)
    (princ (strcat "\nВНИМАНИЕ: " (itoa unconf)
                   " состояний вложенных блоков записаны, но чтение "
                   "показало другое значение -- состояние НЕ применено.")))
  (if defs
    (princ (strcat "\nСостояния возвращены в определения: "
                   (KG-JoinNames (reverse defs)))))
  (foreach x dets
    (KG-TraceDetail (strcat "состояние " x)))
  ;; 0 возвращено, 1 не найдено, 2 не записалось, 3 не подтвердилось,
  ;; 4 число определений, 5 определения, 6 подробности.
  (list restored notfound failed unconf ndefs (reverse defs) dets)
)

;; Есть ли в чертеже многострочная вставка (MINSERT) этого определения.
;; MINSERT тоже держит определение, а в подсчёт ссылок он не входит,
;; поэтому при его наличии *U не стираем.
(defun KG_HasMInsert (defname / e ed hit)
  ;; Есть ли ВНУТРИ определения многострочная вставка (MINSERT): её
  ;; нельзя расчленить, и определение с ней стирать не следует.
  ;;
  ;; ПРЕЖНЯЯ ПРОВЕРКА БЛОКИРОВАЛА ВСЁ. Она искала ssget'ом вставки с
  ;; признаком атрибутов ((66 . 1)) и именем определения. Анонимное
  ;; представление динамического блока почти всегда несёт атрибуты --
  ;; тег представления (AcDbBlockRepBTag) хранится именно так. Поэтому
  ;; проверка отвечала «да» на все 43 сироты, ENTD не вызывался ни разу,
  ;; и очистка печатала «Стёрто 0, не удалось 43». Многострочная вставка
  ;; -- это другое: группа 0 равна "MINSERT", и лежит она внутри самого
  ;; определения, поэтому искать её нужно обходом содержимого.
  (setq hit nil)
  (setq e (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
  (while (and (not hit)
              e
              (setq e (KG-Safe '(lambda () (entnext e)) nil))
              (setq ed (KG-Safe '(lambda () (entget e)) nil))
              (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
    (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "MINSERT") (setq hit t))
  )
  hit
)

;; Есть ли в чертеже определения, пришедшие из внешней ссылки. Пока они
;; есть, содержимое чертежа связано с внешним файлом и *U не стираются
;; (п. «XREF не обрабатываем»).
(defun KG_HasXrefDefs ( / tbl out)
  (setq out nil)
  (setq tbl (KG-Safe '(lambda () (tblnext "BLOCK" t)) nil))
  (while tbl
    (if (KG-IsXrefDepName (KG-AsString (cdr (assoc 2 tbl))))
      (setq out t tbl nil)
      (setq tbl (KG-Safe '(lambda () (tblnext "BLOCK")) nil))
    )
  )
  out
)

;; Стирание одного сиротского *U: ENTD по записи таблицы блоков.
;; Возвращает T, если определение исчезло.
(defun KG_EXEraseAnonDef (defname / e r ok n)
  (setq ok nil n 0)
  (if (KG-Safe '(lambda () (KG_HasMInsert defname)) nil)
    ;; без этой строки отказ выглядел как «не удалось: 43» без причины
    (KG-TraceDetail (strcat "стирание \"" (KG-AsString defname)
                            "\": внутри есть многострочная вставка"))
    (progn
      (setq e (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
      (if e
        (progn
          (setq r (KG-Call 'entdel (list e) nil))
          (if r
            (progn
              ;; определение должно исчезнуть; если ename ещё читается,
              ;; стирание не состоялось
              (while (and (< n 3)
                          (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
                (setq n (1+ n))
              )
              (setq ok (not (KG-Safe '(lambda () (KG-DefEnt defname)) nil)))
            )
            ;; Текст прежней метки («BLOCK не найден») вводил в заблуждение:
            ;; ename к этому моменту найден, не отдался именно ENTD. В
            ;; прогоне сборки 54 строка «нет BLOCK для ...» не появилась ни
            ;; разу -- значит, поиск определения работал, а читался отказ
            ;; как «определения нет».
            (KG-TraceDetail (strcat "стирание \"" (KG-AsString defname)
                                    "\": ENTD не отдал определение"))
          )
        )
        (KG-TraceDetail (strcat "стирание \"" (KG-AsString defname)
                                "\": не найден ename определения"))
      )
      (if (and r (not ok))
        (KG-TraceDetail (strcat "стирание \"" (KG-AsString defname)
                                "\": определение читается и после ENTD"))
      )
      ok
    )
  )
)

;; Стирать ли сиротские *U. Отдельный предикат, а не условие внутри
;; KG-CleanupRun: KG-CleanupRun исполняется только в AutoCAD, а решение
;; должно быть проверяемым на стенде. Три причины не стирать: выключен
;; переключатель, стирать нечего, в чертеже есть определения из внешней
;; ссылки (их содержимое связано с внешним файлом, п. «XREF не
;; обрабатываем»).
(defun KG-OwnerBlockName (e / ed oh)
  (if (or (null e) (= (type e) (type "")))
    nil
    (progn
      (setq ed (entget e))
      (setq oh (if ed (cdr (assoc 330 ed)) nil))
      (if oh (KG-RecordNameOf oh) nil)
    )
  )
)

;; Имя записи блока по её handle (группа 2)
(defun KG-RecordNameOf (oh / oe ed)
  (setq oe (if oh (handent (KG-AsString oh)) nil))
  (setq ed (if oe (entget oe) nil))
  (if ed (KG-AsString (cdr (assoc 2 ed))) nil)
)

;;;--- Чтение модели чертежа -------------------------------------------------

;; Список динамических свойств экземпляра: (("Имя" . значение) ...)

(defun KG-EXRefHolders ( / out ss i n e ed tgt defnm flt oh pr v)
  ;; Карта ссылок «определение -> кто его вставляет».
  ;;
  ;; ПРЕЖНИЙ СПОСОБ УБИВАЛ КОМАНДУ: обход таблицы блоков с
  ;; tblobjname + entnext + entget по каждому определению на реальном
  ;; чертеже дал 0xC0000005 после того, как из буфера пришли 109
  ;; определений, а 48 были переименованы. Перехват исключение не ловит.
  ;;
  ;; Теперь -- выборка вставок через ssget "_X" и группа 330 у каждой:
  ;; она указывает на запись блока, внутри которой вставка лежит. Ни COM,
  ;; ни обхода таблицы блоков.
  ;;
  ;; Результат -- ("OK" . карта): маркер в car, карта в cdr, nil --
  ;; выборка не сработала. Маркер ЭЛЕМЕНТОМ списка быть не должен:
  ;; KG-CdrCI ищет по списку и на строке «OK» отвечал «нашёл», из-за чего
  ;; любое имя считалось присутствующим в карте, а сама карта при этом
  ;; терялась -- в сборке 38 на диск ушла именно такая версия.
  (KG-Mark "карта ссылок: выборка всех вставок")
  (setq out nil)
  (setq ss (KG-Call 'ssget (list "_X" (list (cons 0 "INSERT"))) nil))
  (if (KG-IsErr ss)
    (KG-Mark (strcat "карта ссылок: выборка с фильтром отказала: "
                     (KG-AsString
                       (KG-Safe '(lambda () (vl-catch-all-error-message ss))
                                "?"))))
    (KG-Mark (strcat "карта ссылок: выборка с фильтром дала "
                     (if ss "набор" "nil")))
  )
  (if (KG-IsErr ss) (setq ss nil))
  (setq flt t)
  (if (null ss)
    ;; На реальном чертеже выборка с фильтром ((0 . "INSERT")) вернула
    ;; nil при трёх живых вставках, и очистка остановилась. Запасной
    ;; путь -- выборка без фильтра и своя проверка группы 0: дороже, но
    ;; не зависит от того, как AutoCAD обработал фильтр.
    (progn
      (KG-Mark "карта ссылок: запасная выборка без фильтра")
      ;; вторым аргументом nil, а не «без аргумента»: в AutoCAD
      ;; отсутствующий аргумент принимает nil, но полагаться на это не
      ;; стоит -- функция, читающая flt, спотыкается о неопределённую
      ;; переменную.
      (setq ss (KG-Call 'ssget (list "_X" nil) nil))
      (if (KG-IsErr ss) (setq ss nil))
      (setq flt nil)
    )
  )
  (if ss
    (progn
      (setq n (KG-AsNum (KG-Call 'sslength (list ss) 0) 0))
      (KG-Mark (strcat "карта ссылок: объектов в наборе " (itoa n)))
      ;; Промежуточный счёт каждые 200 объектов. Прогон на крупном
      ;; реципиенте (1231 вставка) оборвался с 0xC0000005 сразу после
      ;; строки «объектов в наборе 1231» -- без промежуточных меток
      ;; неизвестно, дошёл ли обход до конца и на каком объекте встал.
      (setq KG-REFDONE 0)
      (setq KG-REFCACHE nil)
      (setq i 0)
      (while (< i n)
        (if (and (> n 200) (= (rem i 200) 0))
          (KG-Mark (strcat "карта ссылок: пройдено " (itoa i) " из "
                           (itoa n)))
        )
        (setq e (KG-Call 'ssname (list ss i) nil))
        (if e
          (progn
            (setq ed (KG-Call 'entget (list e) nil))
            (if ed
              (progn
                ;; без фильтра в наборе все примитивы -- берём вставки
                (if (not flt)
                  (if (not (KG-StrEq (KG-AsString (cdr (assoc 0 ed)))
                                     "INSERT"))
                    (setq ed nil)
                  )
                )
                (if ed
                  (progn
                    (setq tgt (KG-AsString (cdr (assoc 2 ed))))
                    ;; Имя владельца -- из кэша по handle. handent на
                    ;; крупном чертеже после PURGE даёт 0xC0000005, а
                    ;; перехват его не ловит: в прогоне на 1231 вставке
                    ;; INTBRIEF оборвалась именно здесь. Повторных
                    ;; обращений к одному владельцу при этом в десятки
                    ;; раз больше, чем самих владельцев.
                    (setq defnm
                      (KG-Safe
                        '(lambda ()
                           (setq oh (cdr (assoc 330 ed)))
                           (setq oh (if oh (KG-AsString oh) ""))
                           (setq pr (assoc oh KG-REFCACHE))
                           (if pr
                             (cdr pr)
                             (progn
                               (setq v (KG-OwnerBlockName e))
                               (setq KG-REFCACHE
                                 (cons (cons oh v) KG-REFCACHE))
                               v)))
                        nil))
                    (setq KG-REFDONE (1+ KG-REFDONE))
                    ;; Владелец не прочитался -- ссылку всё равно
                    ;; считаем: лучше держатель «неизвестно», чем объявить
                    ;; определение свободным и снять его.
                    (if (/= tgt "")
                      (setq out
                        (KG-SetAssoc tgt
                          (cons (if defnm defnm "неизвестно")
                                (KG-CdrCI tgt out))
                          out))
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (KG-Mark (strcat "карта ссылок: пройдено объектов "
                       (KG-NumStr KG-REFDONE) " из " (itoa n)))
      (setq out (cons "OK" out))
    )
  )
  out
)

(defun KG-CleanupEraseAllowed (flag orph noxref)
  (and flag (if orph t nil) noxref)
)

(defun KG-DBX-Fail (msg)
  (setq KG-DBX-ERR (cons msg KG-DBX-ERR))
  nil
)

;; Выполнить под перехватом; при неудаче записать шаг и текст ошибки.
;; Лямбда КВОТИРОВАННАЯ: вычисленная в AutoCAD превращается в SUBR и
;; vl-catch-all-apply её отвергает.
(defun KG-DBX-Try (step fn / r)
  (setq r (vl-catch-all-apply fn))
  (if (KG-IsErr r)
    (KG-DBX-Fail (strcat step ": "
                         (KG-AsString (vl-catch-all-error-message r))))
    r
  )
)

(defun KG-MasterState (mastername beforedefs arrived)
  (cond
    ((not mastername) "не определена")
    ((not (KG-StrInterCI (list mastername) beforedefs))
     "пришла из буфера как новое определение")
    ((KG-StrInterCI (list mastername) arrived)
     "перезаписана из буфера (имя уже было в чертеже)")
    (t "ОСТАЛАСЬ СТАРОЙ: имя было в чертеже, буфер его не принёс")
  )
)

;; Определения, которые нужно переименовать ДО вставки из буфера.
;;
;; Причина: PASTECLIP НЕ переименовывает конфликтующие определения --
;; он печатает "Duplicate definition of block ... ignored" и оставляет
;; в чертеже СТАРОЕ определение. Поэтому имена освобождаются заранее.
;;
;; Кандидаты -- все неанонимные определения, достижимые из определений
;; семейства base любых итераций (то есть всё, что может прийти вместе
;; с новой мастер-версией под тем же именем).
(defun KG-ConflictCandidates (model base / out nm)
  (setq out nil)
  (foreach nm (KG-UserDefNames model)
    (if (KG-IsFamilyName nm base)
      (foreach c (KG-GetNestedBlocks model nm)
        (setq out (cons c out))
      )
    )
  )
  (KG-Unique
    (vl-remove-if
      '(lambda (x) (or (null x) (= x "")
                       (KG-IsSystemBlockName x)
                       (KG-IsServiceName x)
                       (KG-IsAnonymousName x)
                       (KG-IsFamilyName x base)))
      out))
)

;;; Освободить имя мастер-версии до вставки из буфера.
;;; Возвращает пару (старое-имя . временное-имя) или nil, если имя и так
;;; свободно. Пара нужна, чтобы при неудачной вставке вернуть имя на место.
(defun KG-FreeMasterName (mastername newiter / tmp used)
  (if (not (KG_EXDefExists mastername))
    nil
    (progn
      (setq used (KG_EXAllDefNames))
      (setq tmp (KG-TempDefName mastername newiter used))
      (if (KG_EXRenameDef mastername tmp)
        (progn
          (KG-Mark (strcat "имя мастер-версии освобождено: " mastername
                            " -> " tmp))
          (princ (strcat "\nОпределение \"" mastername
                         "\" временно переименовано в \"" tmp
                         "\", чтобы освободить имя для вставки из буфера."))
          (cons mastername tmp))
        (progn
          (princ (strcat "\nОШИБКА: не удалось освободить имя \"" mastername
                         "\". AutoCAD отбросит пришедшее определение."))
          nil))
    )
  )
)

;; Вернуть имя мастер-версии, если вставка из буфера не удалась
(defun KG-RestoreMasterName (freed / old tmp)
  (setq old (car freed))
  (setq tmp (cdr freed))
  (if (and old tmp (KG_EXDefExists tmp))
    (KG_EXRenameDef tmp old)
    nil
  )
)

;; Определение семейства base, ссылающееся на вложенное nm.
;; Нужно только для осмысленного имени сохраняемого старого определения.
;; Приоритет: вариант семейства с тем же суффиксом (у него «своя» итерация),
;; затем любое определение семейства со ссылкой на nm.
(defun KG-SourceOfNested (model base nm / out p)
  (setq out nil)
  (foreach d (KG-UserDefNames model)
    (if (and (not out)
             (KG-IsFamilyName d base)
             (setq p (KG-ParseBlockName d))
             (KG-StrEq (nth 2 p) nm)
             (vl-some '(lambda (c) (KG-StrEq c nm))
                      (KG-GetNestedBlocks model d)))
      (setq out d)
    )
  )
  (if (not out)
    (foreach d (KG-UserDefNames model)
      (if (and (not out)
               (KG-IsFamilyName d base)
               (vl-some '(lambda (c) (KG-StrEq c nm))
                        (KG-GetNestedBlocks model d)))
        (setq out d)
      )
    )
  )
  out
)

;; Правило переноса состояния видимости (п. 9.4 ТЗ, вариант A).
;; Возвращает (list значение признак-совпадения предупреждение)
;; Возвращает (установленное-состояние найдено-ли-старое текст-предупреждения).
;; Если у определения вообще нет состояний видимости (вложенный блок без
;; параметра видимости), восстанавливать нечего: состояние считается
;; восстановленным и предупреждение не выдаётся.
(defun KG-ResolveVisibility (old avail / hit first)
  (cond
    ((or (null avail) (= (length avail) 0))
     (list old t nil))
    (t
     (setq first (nth 0 avail))
     (setq hit (vl-some '(lambda (a) (if (KG-StrEq a old) a nil)) avail))
     (cond
       (hit (list hit t nil))
       ((or (null old) (= old "")) (list first nil nil))
       (t
        (list first nil
              (strcat "Старое состояние \"" (KG-AsString old)
                      "\" отсутствует в новой версии. Установлено состояние \""
                      (KG-AsString first) "\".")))
     ))
  )
)

;; План восстановления видимости одного экземпляра.
;; Возвращает (("Стойка" (новое-значение совпало предупреждение)) ...)
(defun KG-PlanNestedVisibility (model inst newdefname / def states out r)
  (setq def (KG-FindDef model newdefname))
  (setq out nil)
  (foreach pair (KG-CdrCI "nested-vis" inst)
    (setq states (KG-CdrCI "vis-states" (KG-FindDef model (car pair))))
    (if (null states) (setq states (KG-CdrCI "vis-states" def)))
    (setq r (KG-ResolveVisibility (cdr pair) states))
    (setq out (cons (cons (car pair) (list (nth 0 r) (nth 1 r) (nth 2 r))) out))
  )
  (reverse out)
)

;; Карта интеграции (Этап 4).
(defun KG-BuildIntegrationMap (model mastername beforedefs / p base newiter
                                     scan groups nested newnested existing
                                     tocreate toexist torename out)
  (setq p (KG-ParseBlockName mastername))
  (if (null p)
    nil
    (progn
      (setq base (nth 0 p))
      (setq newiter (nth 1 p))
      (setq scan (KG-ScanFamily model base newiter))
      (setq groups (cdr (assoc "groups" scan)))
      ;; ВАЖНО: классификация «обновить / добавить» считается по снимку
          ;; таблицы блоков ДО вставки. После вставки новые определения уже
          ;; есть в файле, и по текущему состоянию их не отличить от старых.
      (setq newnested (KG-GetNestedBlocks model mastername))
      (if beforedefs
        (setq nested
          (list
            (cons "to-update" (KG-StrInterCI newnested beforedefs))
            (cons "to-add" (KG-StrDiffCI newnested beforedefs))))
        (setq nested (KG-MapNestedBlocks model mastername))
      )
      (setq existing (KG-UserDefNames model))
      (setq tocreate nil toexist nil)
      (foreach g groups
        (if (KG-FindDef model (KG-MakeName base newiter (car g)))
          (setq toexist (cons (car g) toexist))
          (setq tocreate (cons (car g) tocreate))
        )
      )
      (setq torename (KG-ConflictCandidates model base))
      (setq out
        (list
          (cons "family" base)
          (cons "newiter" newiter)
          (cons "master" mastername)
          (cons "scan" scan)
          (cons "variants-to-create" (reverse tocreate))
          (cons "variants-existing" (reverse toexist))
          (cons "nested-to-update" (cdr (assoc "to-update" nested)))
          (cons "nested-to-add" (cdr (assoc "to-add" nested)))
          (cons "nested-all" newnested)
          (cons "pre-rename" torename)
          (cons "instances" (KG-FindOldIterations model base newiter))
          (cons "warnings" nil)
        ))
      (if (null (KG-FindDef model mastername))
        (setq out (KG-SetAssoc "warnings"
                       (cons (strcat "Определение мастер-версии \""
                                     mastername "\" не найдено в файле.")
                             (cdr (assoc "warnings" out)))
                       out))
      )
      out
    )
  )
)

;;;===========================================================================
;;; РАЗДЕЛ 5. ВАЛИДАЦИЯ И ОТЧЁТНОСТЬ (Этап 12)
;;;===========================================================================

(defun KG-ValidateIntegration (model base newiter expected-groups / pairs left
                                     newpairs newservice out lost per v exp got
                                     defnm g exp_total)
  (if (= (KG-AsString newiter) "")
    (progn
      (setq exp_total 0)
      (foreach g expected-groups
        (setq exp_total (+ exp_total (if (vl-consp (cdr g)) (length (cdr g)) (KG-AsNum (cdr g) 0))))
      )
      (setq left (max 0 (- exp_total (length (vl-remove-if-not
                                               '(lambda (ins)
                                                  (and (KG-IsIntegrationName (KG-CdrCI "eff" ins) base newiter)
                                                       (not (and (boundp 'techhandle) techhandle
                                                                 (KG-StrEq (KG-CdrCI "handle" ins) techhandle)))))
                                               (KG-ModelInsts model))))))
    )
    (progn
      (setq pairs (KG-FindOldIterations model base newiter))
      (setq left (length pairs))
    )
  )
  (setq newpairs nil)
  (foreach ins (KG-ModelInsts model)
    (if (and (KG-IsIntegrationName (KG-CdrCI "eff" ins) base newiter)
             (not (and (boundp 'techhandle) techhandle
                       (KG-StrEq (KG-CdrCI "handle" ins) techhandle))))
      (setq newpairs (cons (cons ins (KG-ParseBlockName (KG-CdrCI "eff" ins)))
                           newpairs))
    )
  )
  (setq newservice nil)
  (foreach pr newpairs
    (setq defnm (KG-CdrCI "def" (car pr)))
    (if (and defnm
             (not (KG-IsAnonymousName defnm))
             (KG-IsServiceName defnm)
             (not (member defnm newservice)))
      (setq newservice (cons defnm newservice))
    )
  )
  (setq per nil lost 0)
  (foreach g (KG-ExpectedWithExisting expected-groups KG-ALREADYNEW)
    (setq v (car g))
    (setq exp (cdr g))
    (setq got (length (vl-remove-if-not
                        '(lambda (pr) (KG-StrEq (nth 2 (cdr pr)) v))
                        newpairs)))
    (if (/= exp got) (setq lost (+ lost (abs (- exp got)))))
    (setq per (cons (list v exp got (= exp got)) per))
  )
  (list
    (cons "old-left" left)
    (cons "new-total" (length newpairs))
    (cons "per-variant" (reverse per))
    (cons "service-names" (reverse newservice))
    (cons "lost" lost)
    (cons "ok" (and (= left 0) (= lost 0) (null newservice)))
  )
)

;; Ожидаемое число экземпляров по вариантам с учётом тех, что УЖЕ были
;; на новой итерации до замены. Без этой поправки отчёт сравнивает
;; фактическое число со счётчиком только СТАРЫХ экземпляров: лишний блок
;; новой версии (например, копия, оставшаяся от диагностической вставки)
;; даёт ложное «РАСХОЖДЕНИЕ» и ложную потерю.
;; groups = ((вариант . список-экземпляров) ...), already = ((вариант . n) ...)
(defun KG-ExpectedWithExisting (groups already / out v hit)
  (setq out nil)
  (foreach g groups
    (setq v (car g))
    (setq hit (KG-AssocCI v already))
    (setq out (cons (cons v (+ (length (cdr g)) (if hit (cdr hit) 0))) out))
  )
  (foreach a already
    (if (not (KG-AssocCI (car a) groups))
      (setq out (cons (cons (car a) (cdr a)) out))
    )
  )
  (reverse out)
)

;; Семейство, к которому относился отчёт.
(defun KG-ReportBase (rep) (KG-AsString (cdr (assoc "base" rep))))


;; Диагноз «экземпляры есть, а план пуст».
;;
;; Прогон сборки 52: строка «Экземпляров семейства в чертеже: 2» с чистыми
;; именами «Комплект КП50 v1.1» и «Комплект КП50 v1.2 КП45387» -- и тут же
;; «Заменено экземпляров: 0», «Создано вариантов: 0». Значит, вставка
;; находится одним путём (ssget + эффективное имя через COM) и НЕ
;; находится другим (модель, по которой строится план). Два этих числа и
;; есть диагноз: если они расходятся, дело не в именах и не в плане, а в
;; чтении модели. Возвращает текст строки отчёта.
(defun KG-ModelScanLine (base)
  (strcat "Экземпляров по модели (по ним строится план): "
          (KG-NumStr (length (KG-Safe '(lambda ()
                                         (KG-FindFamilyInstances
                                           (KG_DBGetModel) base))
                                      nil)))
          ", по вставкам напрямую: "
          (KG-NumStr (KG-AsNum
                       (car (KG-Safe '(lambda () (KG-FamilyInstances base))
                                     (list -1 nil)))
                       -1)))
)

;; Итоговый отчёт интеграции (п. 16 ТЗ)
(defun KG-PrintIntegrationReport (rep / plan val created replaced v s
                                      nerr nwarn bad)
  (setq plan (cdr (assoc "plan" rep)))
  (setq val (cdr (assoc "validation" rep)))
  (setq created (cdr (assoc "created" rep)))
  (setq replaced (cdr (assoc "replaced" rep)))

  ;; Списки свернуты: на реципиенте со 100 блоками вложенных
  ;; определений больше сорока, и три списка по сорок строк вытесняли из
  ;; сообщения всё остальное. Полностью прятать их нельзя, поэтому
  ;; печатается счётчик и начало. Отдельно и ПОЛНОСТЬЮ печатаются те, что
  ;; остались старыми, -- это отказ, и его имена нужны все.
  (if created
    (progn
      (KG-Say (strcat "Созданы определения: " (KG-NumStr (length created))))
      (KG-SayList created 10)
    )
  )
  (if (cdr (assoc "nested-to-update" plan))
    (progn
      ;; это СПИСОК ЗАПЛАНИРОВАННЫХ: часть из них AutoCAD мог оставить
      ;; старыми, фактическое число печатается ниже в итоговом блоке
      (KG-Say (strcat "Вложенные определения, которые должны обновиться: "
                      (KG-NumStr
                        (length (cdr (assoc "nested-to-update" plan))))))
      (KG-SayList (cdr (assoc "nested-to-update" plan)) 10)
    )
  )
  (if (cdr (assoc "nested-stale" rep))
    (progn
      (KG-Say (strcat "ОСТАЛИСЬ СТАРЫМИ: "
                      (KG-NumStr (length (cdr (assoc "nested-stale" rep))))))
      (foreach v (cdr (assoc "nested-stale" rep))
        (KG-Say (strcat "  " v "   [ОСТАЛОСЬ СТАРЫМ]")))
    )
  )
  (if (cdr (assoc "nested-to-add" plan))
    (progn
      (KG-Say (strcat "Добавлены новые вложенные определения: "
                      (KG-NumStr (length (cdr (assoc "nested-to-add" plan))))))
      (KG-SayList (cdr (assoc "nested-to-add" plan)) 10)
    )
  )

  ;; Без этих двух чисел непонятно, почему вложенные определения
  ;; обновились или не обновились: освобождение имён -- условие того,
  ;; что PASTECLIP вообще принесёт новые определения.
  ;; Три числа вместо одного: по ним видно, где именно оборвалась цепочка.
  ;; «Освобождено 0» -- имена не освобождались, PASTECLIP подставил старое.
  ;; «Освобождено N, пришло 0» -- имена освободились, но буфер пуст.
  ;; «Освобождено N, вернулось N» -- пришло, но под теми же именами.
  ;; Кандидаты -- сколько имён вообще намечено освободить. Если их мало,
  ;; значит состав мастер-версии определён неверно, и вставка физически
  ;; не может принести новые определения.
  (KG-SayKV "Кандидатов на освобождение"
            (KG-NumStr (cdr (assoc "candidates" rep))))
  (KG-SayKV "Освобождено имён до вставки"
            (KG-NumStr (cdr (assoc "freed" rep))))
  (KG-SayKV "Из них занято пришедшими определениями"
            (KG-NumStr (length (cdr (assoc "renames" rep)))))
  (KG-SayKV "Из них вернулось на место (буфер не принёс)"
            (KG-NumStr (- (KG-AsNum (cdr (assoc "freed" rep)) 0)
                          (length (cdr (assoc "renames" rep))))))
  (if (cdr (assoc "rename-failed" rep))
    (progn
      (KG-SayKV "Не удалось переименовать"
                (KG-NumStr (length (cdr (assoc "rename-failed" rep)))))
      (KG-Say (strcat "  " (KG-JoinNames (cdr (assoc "rename-failed" rep)))))
    )
  )
  (KG-SayKV "Пришло определений из буфера" (KG-NumStr (length (cdr (assoc "arrived" rep)))))
  ;; Отдельно -- судьба самого определения мастер-версии. По числу
  ;; «пришло N» этого не видно: имя могло остаться за старым
  ;; определением, и тогда вся интеграция шла бы от устаревшего мастера.
  (KG-SayKV "Мастер-версия" (KG-AsString KG-MASTER-STATE))
  (if (and KG-MASTER-STATE (KG-StrContains KG-MASTER-STATE "ОСТАЛАСЬ СТАРОЙ"))
    (KG-Say (strcat "ВНИМАНИЕ: определение мастер-версии НЕ обновилось -- "
                    "интеграция шла от устаревшего мастера."))
  )

  (KG-Say "=== Результат ===")
  ;; Сколько экземпляров семейства реально стоит в чертеже и какими
  ;; именами они видны. Без этой строки «Заменено 0» невозможно отличить
  ;; от «экземпляров и не было», а на реальном чертеже вышло именно так.
  (setq bad (KG-Safe '(lambda ()
                        (KG-FamilyInstances (KG-ReportBase rep)))
                     (list -1 nil)))
  (if (>= (KG-AsNum (car bad) 0) 0)
    (progn
      (KG-SayKV "Экземпляров семейства в чертеже" (KG-NumStr (car bad)))
      (if (car (cdr bad))
        (progn
          (KG-Say "  какими именами видны:")
          (foreach v (car (cdr bad)) (KG-Say (strcat "    " (KG-AsString v))))
        )
      )
    )
  )
  ;; Оба числа обязаны совпадать. Если нет -- план строился не по тем
  ;; данным, которые реально лежат в чертеже.
  (KG-Say (KG-Safe '(lambda () (KG-ModelScanLine (KG-ReportBase rep)))
                   "Диагноз по модели не собрался."))
  (KG-SayKV "Заменено экземпляров" (KG-NumStr (cdr (assoc "replaced" replaced))))
  ;; экземпляры внутри определений блоков: у них нет пространства,
  ;; пересоздать их на месте нельзя, поэтому они остаются как есть
  (if (cdr (assoc "space-unknown" replaced))
    (KG-SayKV "Не заменено (лежат внутри определений блоков)"
              (KG-NumStr (cdr (assoc "space-unknown" replaced))))
  )
  (KG-SayKV "Создано вариантов" (KG-NumStr (length created)))
  ;; Печатается ФАКТИЧЕСКОЕ число обновлённых: план может насчитать 42,
  ;; а AutoCAD подставить старые определения -- тогда обновлено 0, и
  ;; отчёт не должен этого скрывать.
  (KG-SayKV "Обновлено вложенных определений"
            (KG-NumStr (- (length (cdr (assoc "nested-to-update" plan)))
                          (length (cdr (assoc "nested-stale" rep))))))
  (if (cdr (assoc "nested-stale" rep))
    (KG-SayKV "Из них остались старыми"
              (KG-NumStr (length (cdr (assoc "nested-stale" rep))))))
  ;; Сколько ссылок внутри определений возвращено на прежнее имя. Без этой
  ;; строки «осталось N определений с суффиксом ~до» не отличить от
  ;; «определения обновлены, но чужие блоки всё ещё вставляют старое».
  (KG-SayKV "Перепривязано ссылок на прежние имена"
            (KG-NumStr (KG-AsNum (cdr (assoc "rewired" rep)) 0)))
  ;; Определения без итерации в имени подменяются целиком, и состояния
  ;; видимости их вложенных блоков сбрасываются в значения новой версии.
  ;; Три числа, а не одно: по нулям не отличить «состояний и не было» от
  ;; «вложенного блока в новой версии нет» и от «записать не удалось».
  (KG-SayKV "Возвращено состояний вложенных блоков в определениях"
            (KG-NumStr (KG-AsNum
                         (nth 0 (cdr (assoc "vis-nested-restored" rep))) 0)))
  (KG-SayKV "  из них не найдено в новой версии"
            (KG-NumStr (KG-AsNum
                         (nth 1 (cdr (assoc "vis-nested-restored" rep))) 0)))
  (KG-SayKV "  из них записать не удалось"
            (KG-NumStr (KG-AsNum
                         (nth 2 (cdr (assoc "vis-nested-restored" rep))) 0)))
  ;; «Записано, но чтение показало другое» -- отдельное число. Без него
  ;; «вернулось 5, отказов 0» выглядело успехом, а на чертеже оставалось
  ;; значение мастер-блока: AutoCAD принял вызов записи и ничего не изменил.
  (KG-SayKV "  из них запись не подтвердилась чтением"
            (KG-NumStr (KG-AsNum
                         (nth 3 (cdr (assoc "vis-nested-restored" rep))) 0)))
  (if (nth 5 (cdr (assoc "vis-nested-restored" rep)))
    (progn
      (KG-SayKV "Определений, в которые возвращены состояния"
                (KG-NumStr (KG-AsNum
                             (nth 4 (cdr (assoc "vis-nested-restored" rep))) 0)))
      (KG-SayList (nth 5 (cdr (assoc "vis-nested-restored" rep))) 10)
    )
  )
  (if (cdr (assoc "rewired-skip" rep))
    (progn
      (KG-SayKV "Оставлены как есть (риск зациклить определение)"
                (KG-NumStr (length (cdr (assoc "rewired-skip" rep)))))
      (KG-SayList (cdr (assoc "rewired-skip" rep)) 10)
    )
  )
  (KG-SayKV "Восстановлено состояний видимости"
            (KG-NumStr (cdr (assoc "vis-restored" replaced))))
  ;; Три строки рядом с «восстановлено»: по одному нулю нельзя отличить
  ;; «у старых экземпляров не было состояний вложенных блоков» от
  ;; «состояния не прочитались» и от «прочитались, но записать некуда».
  ;; Все три нуля при ненулевом числе замен -- это диагноз, а не норма.
  (KG-SayKV "Прочитано состояний вложенных блоков"
            (KG-NumStr (cdr (assoc "nested-vis-n" replaced))))
  ;; Откуда прочитано: у представления экземпляра или из родительского
  ;; определения. «Из определения» означает, что у экземпляра свойства
  ;; вложенных блоков не менялись, -- переносить их некуда, состояние и
  ;; так останется состоянием по умолчанию.
  (KG-SayKV "  из них у представлений экземпляров (*U)"
            (KG-NumStr (cdr (assoc "nested-from-u" replaced))))
  (KG-SayKV "  из них состояний по умолчанию из определения"
            (KG-NumStr (cdr (assoc "nested-from-def" replaced))))
  (KG-SayKV "Вложенных блоков не найдено в новой версии"
            (KG-NumStr (cdr (assoc "vis-notfound" replaced))))
  (KG-SayKV "Экземпляров без анонимного представления"
            (KG-NumStr (cdr (assoc "vis-noanon" replaced))))
  (if (and (> (KG-AsNum (cdr (assoc "replaced" replaced)) 0) 0)
           (< (KG-AsNum (cdr (assoc "nested-vis-n" replaced)) 0) 1))
    (KG-Say (strcat "ВНИМАНИЕ: у заменённых экземпляров не прочитано ни "
                    "одного состояния вложенных блоков. Либо в старых "
                    "экземплярах свойства вложенных блоков не менялись, "
                    "либо чтение не сработало -- проверьте трассировку "
                    "(setq KG-TRACE t): метки \"вложенная вставка ...\"."))
  )
  ;; Отдельно -- свойства самого блока (его собственный параметр
  ;; видимости). Без этой строки «Восстановлено состояний: 0» выглядело
  ;; как потерянное состояние мастера, хотя оно переносилось.
  (KG-SayKV "Прочитано свойств у старых экземпляров"
            (KG-NumStr (cdr (assoc "dyn-read" replaced))))
  (KG-SayKV "Перенесено свойств самого блока"
            (KG-NumStr (cdr (assoc "dyn-restored" replaced))))
  ;; Нуль прочитанного при ненулевом числе замен -- это не «состояний не
  ;; было», а «состояния не прочитались»: переносить тогда нечего, и
  ;; экземпляры получат состояния по умолчанию.
  (if (and (> (KG-AsNum (cdr (assoc "replaced" replaced)) 0) 0)
           (< (KG-AsNum (cdr (assoc "dyn-read" replaced)) 0) 1))
    (KG-Say (strcat "ВНИМАНИЕ: у заменённых экземпляров не прочитано ни "
                    "одного динамического свойства -- состояния видимости "
                    "перенести невозможно, экземпляры получат состояния по "
                    "умолчанию."))
  )
  (KG-SayKV "Состояний не найдено в новой версии"
            (KG-NumStr (cdr (assoc "vis-missing" replaced))))
  (KG-SayKV "Старых экземпляров осталось" (KG-NumStr (cdr (assoc "old-left" val))))
  (KG-SayKV "Новых экземпляров" (KG-NumStr (cdr (assoc "new-total" val))))
  (KG-SayKV "Потерь экземпляров" (KG-NumStr (cdr (assoc "lost" val))))
  (KG-SayKV "Предупреждений" (KG-NumStr (length (cdr (assoc "warnings" rep)))))
  (KG-SayKV "Ошибок" (KG-NumStr (length (cdr (assoc "errors" rep)))))

  (foreach s (cdr (assoc "per-variant" val))
    (princ (strcat "\n  Вариант " (KG-VarLabel (nth 0 s)) ": ожидалось "
                   (KG-NumStr (nth 1 s)) ", стало " (KG-NumStr (nth 2 s))
                   (if (nth 3 s) "  [OK]" "  [РАСХОЖДЕНИЕ]")))
  )
  (foreach w (cdr (assoc "warnings" rep))
    (princ (strcat "\nВНИМАНИЕ:\n" w))
  )
  (foreach s (cdr (assoc "errors" rep))
    (princ (strcat "\nОШИБКА: " s))
  )
  (if (cdr (assoc "service-names" val))
    (progn
      (KG-Say "ВНИМАНИЕ: у созданных экземпляров служебные суффиксы в именах:")
      (foreach s (cdr (assoc "service-names" val))
        (KG-Say (strcat "  " s)))
    )
  )

  ;; Итог одной строкой. Отчёт длинный, и по нему не сразу видно,
  ;; состоялся прогон или нет: нули ошибок и предупреждений теряются
  ;; среди счётчиков. Расхождение по варианту считается ошибкой -- это
  ;; нарушение п. «количество экземпляров сохраняется».
  (setq nerr (length (cdr (assoc "errors" rep))))
  (setq nwarn (+ (length (cdr (assoc "warnings" rep)))
                 (KG-AsNum (cdr (assoc "lost" val)) 0)
                 (KG-AsNum (cdr (assoc "old-left" val)) 0)))
  (setq bad nil)
  (foreach v (cdr (assoc "per-variant" val))
    (if (not (nth 3 v)) (setq bad (1+ (if bad bad 0))))
  )
  (princ (strcat "\n=== ИТОГ: "
                 (cond
                   ((> nerr 0)
                    (strcat "ОШИБКИ (" (itoa nerr) ") -- см. строки выше"))
                   ((> (if bad bad 0) 0)
                    (strcat "РАСХОЖДЕНИЕ ПО КОЛИЧЕСТВУ (" (itoa bad) ")"))
                   ((> nwarn 0)
                    (strcat "выполнено, есть предупреждения ("
                            (itoa nwarn) ")"))
                   (t "УСПЕШНО, замечаний нет"))
                 " ==="))
)


;; Добить строку пробелами до нужной ширины. В AutoCAD НЕТ функции
;; make-string -- на реальном чертеже это дало бы
;; "no function definition: MAKE-STRING".
(defun KG-Pad (s width / out)
  (setq out (KG-AsString s))
  (while (< (strlen out) width) (setq out (strcat out " ")))
  out
)

(defun KG-PrintScanReport (scan / g)
  (KG-Say "Найдены старые варианты:")
  (foreach g (cdr (assoc "groups" scan))
    (princ (strcat "\nВариант " (KG-VarLabel (car g))
                   (KG-Pad "" (max 0 (- 10 (strlen (if (car g) (car g) "")))))
                   ": " (itoa (length (cdr g)))))
  )
  (KG-SayKV "Всего" (KG-NumStr (cdr (assoc "total" scan))))
)

(defun KG-PrintSpaceReport (scan)
  (KG-Say "Пространства:")
  (foreach s (cdr (assoc "spaces" scan))
    (princ (strcat "\n  " (KG-AsString (car s)) " : " (KG-NumStr (cdr s))))
  )
)

;;;===========================================================================
;;; РАЗДЕЛ 6. КОНТРАКТ АДАПТЕРА БД
;;;
;;; Чтение:
;;;   KG_DBGetModel          ()                 -> модель чертежа
;;;
;;; Исполнение (все должны возвращать nil при неудаче, а не ронять команду):
;;;   KG_EXAllDefNames       ()                 -> имена определений
;;;   KG_EXDefExists         (name)             -> T/nil
;;;   KG_EXRenameDef         (old new)          -> T/nil
;;;   KG_EXDeleteDef         (name)             -> T/nil
;;;   KG_EXPasteAtOrigin     ()                 -> T/nil   техническая вставка
;;;   KG_SnapshotDrawing     ()                 -> снимок handle'ов вхождений
;;;   KG_EXNewInstancesSince (snapshot)         -> новые handle'ы
;;;   KG_EXInstanceHandle    (handle)           -> модель экземпляра | nil
;;;   KG_EXCreateInstance    (defname instmodel)-> handle | nil
;;;   KG_EXSetEffectiveName  (handle defname)   -> T/nil
;;;   KG_EXDeleteInstance    (handle)           -> T/nil
;;;   KG_EXRestoreDynProps   (handle dynprops)  -> число восстановленных
;;;   KG_EXSetNestedVisibility (handle nm value)-> T/nil
;;;   KG_EXCreateDefFromMaster (newname master) -> T/nil
;;;
;;;   KG-RefreshSpaceMap     ()                 -> перестроить карту
;;;       handle -> пространство. Нужна после вставки: карта, построенная
;;;       до вставки, не знает о новых объектах.
;;;===========================================================================

;;;===========================================================================
;;; РАЗДЕЛ 7. ШАГИ ИНТЕГРАЦИИ И ОРКЕСТРАЦИЯ
;;;===========================================================================

;; Имя, под которым сохраняется старое определение после перевода ссылок
;; на новое. Осмысленное -- видно, что оно предшествовало итерации iter, --
;; и без служебных маркеров вроде $0$, _new, "копия" (п. 20 ТЗ).
;; Определение, пришедшее из внешней ссылки: имя вида "чертёж|блок".
;; Переименовывать такие нельзя -- на реальном чертеже с 264 кандидатами
;; попытка дала 0xC0000005. Флаг is-xref у них не выставлен: ссылка --
;; сам внешний файл, а это его зависимое определение.
(defun KG-IsXrefDepName (nm)
  (KG-StrContains (KG-AsString nm) "|")
)

(defun KG-TempDefName (base iter used)
  (KG-UniqueName (strcat base "~до" (KG-IterToStr iter)) used)
)

;; Шаг 1. Освободить имена ДО вставки из буфера.
;; Возвращает список троек (исходное-имя временное-имя определение-источник).
(defun KG-Step_PreRename (names iter base model / used out tmp nm)
  ;; Список занятых имён читается ОДИН раз. Раньше он перечитывался в
  ;; каждой итерации: на чертеже с 264 кандидатами это 264 полных прохода
  ;; по таблице блоков. Меняется список только нашими же
  ;; переименованиями, поэтому новое имя достаточно в него добавить.
  (setq used (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
  (setq out nil)
  (foreach nm names
    (if (KG-IsXrefDepName nm)
      (princ (strcat "\nПропущено, определение из внешней ссылки: \""
                     nm "\""))
      (if (KG_EXDefExists nm)
        (progn
          ;; Метка на каждое имя: если переименование уронит AutoCAD,
          ;; последняя назовёт виновника. Печатается только на подробном
          ;; уровне -- имён больше сотни, и KG-LAST-STEP запоминается
          ;; в любом случае.
          (KG-TraceDetail (strcat "переименование \"" nm "\""))
          (setq tmp (KG-TempDefName nm iter used))
          (if (KG_EXRenameDef nm tmp)
            (progn
              (setq used (cons tmp used))
              (setq out (cons (list nm tmp
                                    (KG-SourceOfNested model base nm))
                              out))
            )
            (progn
              (setq KG-PRENAME-FAILED (cons nm KG-PRENAME-FAILED))
              (princ (strcat "\nВНИМАНИЕ: не удалось временно переименовать \""
                             nm "\" -- новое определение с этим именем"
                             " не придёт."))
            )
          )
        )
      )
    )
  )
  (reverse out)
)

;; Из переименованных имён оставить только те, что буфер реально принёс.
;; Остальные возвращаются на место: освобождать имя имеет смысл только
;; тогда, когда на него что-то пришло. Иначе определение, которого в
;; новом мастере нет (п. 8.4 ТЗ: не удалять), осталось бы под служебным
;; именем «имя~до<итерация>», а итоговые имена обязаны быть чистыми.
(defun KG-Step_RollbackNotArrived (renames arrived / out)
  (setq out nil)
  (foreach r renames
    (if (KG-StrInterCI (list (nth 0 r)) arrived)
      (setq out (cons r out))
      (KG_EXRenameDef (nth 1 r) (nth 0 r))
    )
  )
  (reverse out)
)

;; Откат временных переименований (когда буфер не «пришёл»)
(defun KG-Step_RollbackRename (renames / out)
  (setq out nil)
  (foreach r renames
    (if (KG_EXRenameDef (nth 1 r) (nth 0 r))
      (setq out (cons (nth 0 r) out))
    )
  )
  (if out
    (princ (strcat "\nПереименования откатаны, чертёж не изменён: "
                   (KG-JoinNames out)))
  )
  (reverse out)
)

;; Что реально пришло из буфера
(defun KG-Step_DetectArrived (beforedefs afterdefs)
  (KG-StrDiffCI afterdefs beforedefs)
)

;; Найти имя мастер-версии среди пришедших определений
(defun KG-DetectMasterName (newdefs / out p cand root mdl all_nested)
  (setq out nil)
  (foreach nm newdefs
    (if (not out)
      (progn
        (setq p (KG-ParseBlockName nm))
        (if (and p (/= (nth 1 p) "") (= (nth 2 p) "")) (setq out nm))
      )
    )
  )
  (if (and (null out) (> (length newdefs) 0))
    (progn
      (setq cand nil)
      (foreach nm newdefs
        (setq p (KG-ParseBlockName nm))
        (if (and p (not (KG-IsAnonymousName nm)) (not (KG-IsServiceName nm)) (not (KG-IsSystemBlockName nm)))
          (setq cand (cons nm cand))
        )
      )
      (setq cand (reverse cand))
      (if cand
        (progn
          (setq mdl (KG-Safe '(lambda () (KG_DBGetModel)) nil))
          (setq all_nested nil)
          (if mdl
            (foreach d (KG-ModelDefs mdl)
              (foreach n (KG-CdrCI "nested" d)
                (if (not (member n all_nested)) (setq all_nested (cons n all_nested)))
              )
            )
          )
          (setq root nil)
          (foreach c cand
            (if (and (null root) (not (member c all_nested)))
              (setq root c)
            )
          )
          (setq out (if root root (car cand)))
        )
      )
    )
  )
  out
)

;; Из новых экземпляров выбрать мастер-версию (имя без суффикса)
(defun KG-PickMasterHandle (handles / best m p)
  (setq best nil)
  (foreach h handles
    (if (not best)
      (progn
        (setq m (KG_EXInstanceHandle h))
        (if m
          (progn
            (setq p (KG-ParseBlockName (KG-CdrCI "eff" m)))
            (if (and p (= (nth 2 p) "")) (setq best h))
          )
        )
      )
    )
  )
  best
)

;; Шаг 2. Создать недостающие варианты из мастер-версии (Этап 9)
;;; Вариант создаётся глубоким клонированием, чтобы он остался
;;; ДИНАМИЧЕСКИМ, как мастер-версия. Если ObjectDBX недоступен --
;;; запасной путь без динамики, с явным предупреждением.
(defun KG_EXCreateDefFromMaster (newname mastername / ok cur curdyn srcdyn)
  (setq cur (KG_EXDefExists newname))
  ;; Определение уже есть. Если мастер-версия динамическая, а оно нет --
  ;; это остаток прошлой попытки, когда вариант строился копированием
  ;; объектов. Такое определение пересоздаётся, иначе оно навсегда
  ;; останется статическим.
  (if cur
    (progn
      (setq curdyn (KG-Safe '(lambda () (KG-EXDefIsDynamic newname)) nil))
      (setq srcdyn (KG-Safe '(lambda () (KG-EXDefIsDynamic mastername)) nil))
      (if (and srcdyn (not curdyn))
        (progn
          (princ (strcat "\nВНИМАНИЕ: \"" newname "\" уже есть и оно "
                         "статическое -- пересоздаю динамическим."))
          (if (KG-Safe '(lambda () (KG_EXDeleteDef newname)) nil)
            (setq cur nil)
            (princ (strcat "\nВНИМАНИЕ: \"" newname "\" используется "
                           "экземплярами, удалить нельзя -- оно останется "
                           "статическим. Удалите его экземпляры и повторите."))
          )
        )
      )
    )
  )
  (if cur
    t
    (progn
      ;; Обёртка ОБЯЗАНА записывать причину: KG-Safe проглатывал исключение
      ;; из KG_EXCopyDefDeep, KG-DBX-ERR оставался пустым, и отчёт печатал
      ;; бесполезное «скопировать определение не удалось».
      (setq ok
        (KG-DBX-Try
          (strcat "глубокое копирование \"" (KG-AsString mastername)
                  "\" -> \"" (KG-AsString newname) "\"")
          '(lambda () (KG_EXCopyDefDeep mastername newname))))
      (if (not ok)
        (progn
          (princ (strcat "\nВНИМАНИЕ: вариант \"" newname "\" создан БЕЗ "
                         "динамических свойств -- он будет статическим блоком."
                         "\nПричина: "
                         (if KG-DBX-ERR
                           (KG-JoinNames (reverse KG-DBX-ERR))
                           "не сообщена (KG_EXCopyDefDeep вернула nil, ни один шаг не отказал)")
                         "\nПроверьте подробнее командой INTDBXTEST."))
          (setq ok (KG-EXCreateDefStatic newname mastername))
        )
      )
      ok
    )
  )
)

(defun KG-Step_CreateVariants (plan / base newiter out nm)
  (setq base (cdr (assoc "family" plan)))
  (setq newiter (cdr (assoc "newiter" plan)))
  (setq out nil)
  (foreach v (cdr (assoc "variants-to-create" plan))
    (setq nm (KG-MakeName base newiter v))
    (if (KG_EXCreateDefFromMaster nm (cdr (assoc "master" plan)))
      (setq out (cons nm out))
      (princ (strcat "\nОШИБКА: невозможно создать вариант \"" nm "\"."))
    )
  )
  (reverse out)
)

;; Шаг 3. Заменить старые экземпляры (Этап 10) и восстановить видимости
;; (Этап 11). Экземпляр пересоздаётся, поэтому его Handle меняется --
;; это отражается в отчёте (п. 15 ТЗ).
(defun KG-Step_ReplaceInstances (model plan oldpairs / base newiter nh tgt
                                       plan-vis old restored missing warns
                                       out cnt nospace pr inst v pn pstate
                                       phit pwarn doone dynrestored dynread
                                       nvread notfound noanon retry pv r
                                       nfromu nfromdef src nv th)
  (setq base (cdr (assoc "family" plan)))
  (setq newiter (cdr (assoc "newiter" plan)))
  (setq restored 0 missing 0 warns nil out nil cnt 0 nospace 0
        dynrestored 0 dynread 0 nvread 0 notfound 0 noanon 0
        nfromu 0 nfromdef 0)
  (setq th (if (boundp 'techhandle) techhandle KG-TECH-HANDLE))
  (foreach pr oldpairs
    (setq inst (car pr))
    (if (and th (KG-StrEq (KG-CdrCI "handle" inst) th))
      nil
      (progn
        (setq doone (KG-CdrCI "space" inst))
        (if (not doone)
          (progn
            (setq nospace (1+ nospace))
            (princ (strcat "\nВНИМАНИЕ: экземпляр \""
                           (KG-AsString (KG-CdrCI "eff" inst))
                           "\" (handle " (KG-AsString (KG-CdrCI "handle" inst))
                           ") лежит внутри определения блока и не заменён."))
          )
        )
        (setq v (nth 2 (cdr pr)))
        (setq tgt (KG-MakeName base newiter v))
        (setq plan-vis (KG-PlanNestedVisibility model inst tgt))
        (setq old (KG-CdrCI "handle" inst))
        (setq nh (if doone (KG_EXCreateInstance tgt inst) nil))
        (if (and doone nh)
          (progn
            (setq dynread
              (+ dynread (length (KG-CdrCI "dyn-props" inst))))
            (setq dynrestored
              (+ dynrestored
                 (KG-AsNum
                   (KG_EXRestoreDynProps nh (KG-CdrCI "dyn-props" inst)) 0)))
            (setq nv (KG-AsNum (KG-CdrCI "nested-vis-n" inst) 0))
            (setq nvread (+ nvread nv))
            (setq src (KG-CdrCI "nested-vis-src" inst))
            (cond
              ((KG-StrEq src "*U") (setq nfromu (+ nfromu nv)))
              ((KG-StrEq src "ОПРЕДЕЛЕНИЕ") (setq nfromdef (+ nfromdef nv))))
            (setq retry nil)
            (foreach pv plan-vis
              (setq pn (car pv))
              (setq pstate (nth 0 (cdr pv)))
              (setq phit (nth 1 (cdr pv)))
              (setq pwarn (nth 2 (cdr pv)))
              (setq r (KG_EXSetNestedVisibility nh pn pstate))
              (cond
                ((= r t)
                 (if phit (setq restored (1+ restored)) (setq missing (1+ missing))))
                ((KG-StrEq r "НЕ НАЙДЕН") (setq retry (cons pv retry)))
                ((KG-StrEq r "НЕТ ПРЕДСТАВЛЕНИЯ")
                 (setq noanon (1+ noanon))
                 (if (< noanon 6)
                   (setq warns
                     (cons (strcat "Блок: " (KG-CdrCI "eff" inst)
                                   "\nВложенный блок: " pn
                                   "\nУ экземпляра нет анонимного "
                                   "представления, состояние вложенного блока \""
                                   (KG-AsString pstate)
                                   "\" установить только для него нельзя.")
                           warns))))
                (r
                 (if phit
                   (setq restored (1+ restored))
                   (setq missing (1+ missing))))
                (t
                 (princ (strcat "\nОШИБКА: состояние видимости \""
                                (KG-AsString pstate)
                                "\" не установлено во вложенном блоке \"" pn
                                "\" экземпляра \"" (KG-CdrCI "eff" inst)
                                "\" (handle " (KG-AsString nh) ")."))))
              (if pwarn
                (setq warns
                  (cons (strcat "Блок: " (KG-CdrCI "eff" inst)
                                "\nВложенный блок: " pn
                                "\n" pwarn)
                        warns))
              )
            )
            (foreach pv retry
              (setq pn (car pv))
              (setq pstate (nth 0 (cdr pv)))
              (setq phit (nth 1 (cdr pv)))
              (setq r (KG_EXSetNestedVisibility nh pn pstate))
              (cond
                ((= r t)
                 (if phit (setq restored (1+ restored)) (setq missing (1+ missing))))
                ((KG-StrEq r "НЕТ ПРЕДСТАВЛЕНИЯ") (setq noanon (1+ noanon)))
                (t
                 (setq notfound (1+ notfound))
                 (setq warns
                   (cons (strcat "Блок: " (KG-CdrCI "eff" inst)
                                 "\nВложенный блок: " pn
                                 "\nВ новой версии такого вложенного блока нет, "
                                 "состояние \"" (KG-AsString pstate)
                                 "\" не перенесено.")
                         warns))))
            )
            (KG_EXDeleteInstance old)
            (setq cnt (1+ cnt))
            (setq out (cons nh out))
          )
          (if doone
            (princ (strcat "\nОШИБКА: не удалось создать экземпляр \"" tgt
                           "\" вместо handle " (if old old "?")
                           " -- старый экземпляр оставлен без изменений."))
          )
        )
      )
    )
  )
  (list
    (cons "replaced" cnt)
    (cons "space-unknown" nospace)
    (cons "vis-restored" restored)
    (cons "vis-missing" missing)
    (cons "vis-notfound" notfound)
    (cons "vis-noanon" noanon)
    (cons "nested-vis-n" nvread)
    (cons "nested-from-u" nfromu)
    (cons "nested-from-def" nfromdef)
    (cons "dyn-read" dynread)
    (cons "dyn-restored" dynrestored)
    (cons "warnings" (reverse warns))
    (cons "newhandles" (reverse out))
  )
)

;; Шаг 4. Перепривязать оставшиеся вложенные вхождения (те, что живут
;; внутри сторонних определений) на новые определения.
(defun KG-Step_RewireNested (model renames / tgt n eff)
  (setq n 0)
  (foreach r renames
    (setq tgt (nth 0 r))
    (foreach ins (KG-ModelInsts model)
      (setq eff (KG-CdrCI "eff" ins))
      (if (KG-StrEq eff (nth 1 r))
        (if (KG_EXSetEffectiveName (KG-CdrCI "handle" ins) tgt)
          (setq n (1+ n))
        )
      )
    )
  )
  n
)

;; Шаг 5. Убрать технический экземпляр и решить судьбу старых определений.
;; По п. 8.4 и 14.4 ТЗ неиспользуемые определения автоматически НЕ удаляются,
;; поэтому по умолчанию они остаются под именем "имя~~<итерация-источника>".
(defun KG-Step_Cleanup (renames techhandle / out kept nm)
  (setq out nil kept nil)
  (if techhandle (KG_EXDeleteInstance techhandle))
  (foreach r renames
    (setq nm (nth 1 r))
    (if (and KG-DELETE-OLD-DEFS (KG_EXDeleteDef nm))
      (setq out (cons nm out))
      (setq kept (cons (strcat (nth 0 r) " -> " nm) kept))
    )
  )
  (if kept
    (princ (strcat "\nСтарые определения сохранены и больше не используются "
                   "(удаляются через PURGE): " (KG-JoinNames kept)))
  )
  (reverse out)
)

;;;; Запасной путь режима A: из буфера не пришло ни одного определения, но
;;; пользователь мог вставить мастер-блок руками ДО запуска команды.
;;;
;;; Это важный случай: при ручной вставке AutoCAD отбрасывает вложенные
;;; определения с уже занятыми именами и подставляет существующие, поэтому
;;; мастер-версия оказывается собранной из СТАРОГО содержимого. Интегрировать
;;; по ней нельзя -- нужно отменить вставку и дать команде вставить самой.
;;;
;;; snap0      -- снимок экземпляров до запроса «Мастер-блок:»
;;; beforedefs -- снимок таблицы блоков до любых изменений
;;; model      -- текущая модель чертежа
;;;
;;; Возврат:
;;;   (T mastername handle base iter) -- мастер-версия найдена и пригодна;
;;;   (nil текст-сообщения)           -- продолжать нельзя.
(defun KG-FallbackMasterFromDrawing (snap0 beforedefs model
                                     / newinst cand m nm p names nested stale)
  (setq newinst (KG_EXNewInstancesSince snap0))
  (setq names nil)
  (foreach h newinst
    (setq m (KG_EXInstanceHandle h))
    (if m (setq names (cons (KG-CdrCI "eff" m) names)))
  )
  (setq names (reverse names))
  (setq cand (KG-PickMasterHandle newinst))
  (setq nm (if cand (KG-CdrCI "eff" (KG_EXInstanceHandle cand)) nil))
  (setq p (if nm (KG-ParseBlockName nm) nil))
  (cond
    ;; мастер-версия не найдена вовсе
    ((not (and p (= (nth 2 p) "")))
     (list nil
       (strcat
         "\nОШИБКА: не удалось определить мастер-версию."
         ;; Факты, а не догадка: сколько определений было и стало.
         ;; Если числа равны -- вставка не принесла ничего, и причину
         ;; надо искать в буфере обмена, а не в именах.
         (strcat "\nОпределений в таблице блоков было: "
                 (itoa (length beforedefs)) ", стало: "
                 (itoa (length (KG-Safe '(lambda () (KG_EXAllDefNames))
                                        nil))))
         "\nИз буфера не пришло ни одного нового определения. Обычно это"
         "\nзначит, что AutoCAD отбросил их как дубликаты (\"Повторное"
         "\nопределение блока ... пропущено\") -- такие имена в файле"
         "\nуже есть."
         (if names
           (strcat "\nНовые экземпляры в чертеже: " (KG-JoinNames names) ".")
           "\nНовых экземпляров в чертеже нет -- вставка не выполнена.")
         "\nЧто делать:"
         "\n  1. имя мастер-версии должно содержать номер итерации,"
         "\n     вида \"Комплект КП50 v1.5\" или \"КП50(16)\", без суффикса"
         "\n     варианта;"
         "\n  2. если блок с таким именем в файле уже есть, удалите или"
         "\n     переименуйте его ДО вставки из буфера;"
         "\n  3. после этого скопируйте мастер-блок заново и повторите"
         "\n     INTEGRATE.")))
    ;; определение мастер-версии было в файле ещё до команды
    ((member nm beforedefs)
     (list nil
       (strcat
         "\nОШИБКА: определение \"" nm "\" уже было в файле до запуска"
         "\nкоманды, из буфера не пришло ничего нового."
         "\nAutoCAD отбросил пришедшее определение как дубликат."
         "\nУдалите или переименуйте \"" nm "\" и повторите вставку.")))
    ;; определение пришло, но вложенные -- нет: содержимое старое
    ((progn
       (setq nested (KG-GetNestedBlocks model nm))
       (setq stale (KG-StrInterCI nested beforedefs))
       stale)
     (list nil
       (strcat
         "\nОШИБКА: определение \"" nm "\" пришло, но вложенные"
         "\nопределения " (KG-JoinNames stale) " остались СТАРЫМИ:"
         "\nAutoCAD отбросил их как дубликаты и подставил существующие."
         "\nИнтегрировать по такому мастеру нельзя -- получится старое"
         "\nсодержимое под новым именем."
         "\nЧто делать:"
         "\n  1. отмените свою вставку: UNDO (или Ctrl+Z) до её отмены;"
         "\n  2. запустите INTEGRATE заново и на запросе \"Мастер-блок:\""
         "\n     нажмите ENTER, ничего не выбирая;"
         "\n  3. команда сама освободит конфликтующие имена и сама вставит"
         "\n     блок из буфера -- тогда содержимое придёт новое.")))
    ;; всё в порядке: мастер пришёл, вложенные тоже новые
    (t
     (KG-Say "")
     (KG-Say "Из буфера не пришло новых определений, но в чертеже есть")
     (KG-Say "новый экземпляр мастер-версии с новым содержимым.")
     (KG-Say "Продолжаем по нему.")
     (list t nm cand (nth 0 p) (nth 1 p)))
  )
)


;; Печать длинного списка имён: первые несколько и счётчик.
;;
;; На реципиенте со 100 блоками списки «пришло из буфера» (111 имён),
;; «освобождено» (105) и «кандидаты на очистку» (43) занимали больше места,
;; чем весь остальной отчёт, и лог не помещался в сообщение. Полностью
;; прятать их нельзя: по именам видно, не тронуто ли чужое семейство.
;; Поэтому печатается начало списка и остаток числом.
(defun KG-PrincList (lst limit / i)
  (setq i 0)
  (foreach nm lst
    (if (< i limit)
      (progn
        (princ (strcat "\n  " (KG-AsString nm)))
        (setq i (1+ i)))
    )
  )
  (if (> (length lst) limit)
    (princ (strcat "\n  ... и ещё " (KG-NumStr (- (length lst) limit))))
  )
  (length lst)
)

(defun KG-SayList (lst limit / i)
  (setq i 0)
  (if (null lst)
    (KG-Say "  (ни одного)")
    (foreach nm lst
      (if (< i limit)
        (progn
          (KG-Say (strcat "  " (KG-AsString nm)))
          (setq i (1+ i)))
      )
    )
  )
  (if (> (length lst) limit)
    (KG-Say (strcat "  ... и ещё " (KG-NumStr (- (length lst) limit)))))
  (length lst)
)

(defun KG-ReportArrived (beforedefs afterdefs / newdefs gone nm)
  (setq newdefs (KG-Step_DetectArrived beforedefs afterdefs))
  (setq gone (KG-StrDiffCI beforedefs afterdefs))
  (KG-Say (strcat "Из буфера пришли определения: "
                  (KG-NumStr (length newdefs))))
  (if newdefs
    (KG-SayList newdefs 5)
    (progn
      (KG-Say "  ВНИМАНИЕ: скорее всего сработало \"Duplicate definition of")
      (KG-Say "  block ... ignored\" — AutoCAD оставил СТАРЫЕ определения.")
      (KG-Say "  Переименуйте конфликтующие определения и повторите вставку.")
    )
  )
  (if gone
    (progn
      (KG-Say (strcat "Освобождённые имена: " (KG-NumStr (length gone))))
      (KG-SayList gone 5)
    )
  )
)


;;;--- ОРКЕСТРАЦИЯ -----------------------------------------------------------
;;; Полный цикл интеграции. Работает только через адаптер KG_DB*/KG_EX*,
;;; поэтому одинаково исполняется и в AutoCAD, и на тестовом стенде.
;;;
;;; base       -- семейство, например "ABC1.01"
;;; newiter    -- новая итерация, например 15
;;; techhandle -- handle технического экземпляра мастер-версии (или nil)
;;; do-paste   -- T, если нужна техническая вставка из буфера (режим A)
;;;
;;; Порядок операций принципиален:
;;;   1. снимается снимок таблицы блоков ДО изменений -- по нему потом
;;;      различаются «обновить существующее вложенное» и «добавить новое»;
;;;      второй снимок делается после освобождения имён и до вставки --
;;;      только по нему видно, что пришло именно из буфера;
;;;   2. освобождаются имена, которые должно занять содержимое буфера --
;;;      иначе PASTECLIP отбросит новые определения;
;;;   3. проверяется, что мастер-версия ДЕЙСТВИТЕЛЬНО пришла из буфера;
;;;      если нет -- переименования откатываются и операция не начинается;
;;;   4. только теперь читается модель и строится план: на этом шаге
;;;      мастер-версия уже ссылается на новые вложенные определения;
;;;   5. создаются варианты, заменяются экземпляры, перепривязываются
;;;      вложенные вхождения внутри сторонних определений.
(defun KG-Integrate_Model (base newiter techhandle do-paste / model mastername
                                                pre renames plan created rep
                                                deleted val warns errs arrived
                                                missing v snap beforedefs
                                                pastesnap arriveddefs stale
                                                masterfreed handlesnap
                                                eff a rewired visrest)
  (setq errs nil)
  (setq warns nil)
  (setq mastername (KG-MakeName base newiter ""))

  ;; 1. снимок таблицы блоков ДО любых изменений. Именно по нему потом
  ;;    различаются «вложенное определение уже было в файле» (обновляем)
  ;;    и «его в файле не было» (добавляем). Снимок, сделанный после
  ;;    переименования, этого различия не даёт.
  (KG-Mark "снимок чертежа до изменений")
  ;; Прогон сборки 55: снимок не читался, и команда обрывалась на
  ;; «неверная функция: T» уже после вставки из буфера, с освобождёнными
  ;; именами. Отказ снимка -- неполные данные, а не отказ интеграции.
  (setq model (KG-Safe '(lambda () (KG_DBGetModel)) nil))
  (if (null model)
    (progn
      (princ (strcat "\nВНИМАНИЕ: снимок чертежа не прочитан ("
                     (KG-FailText)
                     "). Отчёт будет неполным, интеграция продолжена."))
      (setq model (list (cons "defs" nil) (cons "insts" nil)
                        (cons "layouts" nil)))
    )
  )
  ;; В режиме вставки из буфера снимок обязан быть сделан ДО того, как
  ;; C:INTEGRATE освободил имена: по нему «вложенное определение уже
  ;; было в файле» (обновляем) отличается от «его не было» (добавляем).
  ;; Снимок, снятый после переименования, путает их местами.
  (setq beforedefs (if KG-PREDEFS KG-PREDEFS (KG_EXAllDefNames)))

  ;; 2. освобождаем имена до вставки.
  ;;    В режиме вставки из буфера (do-paste) вставку выполняет команда
  ;;    C:INTEGRATE ещё до вызова этой функции, и имена освобождает тоже
  ;;    она -- она передаёт их списком prerenames. Освобождать имена здесь
  ;;    уже поздно, а переименовывать пришедшие определения нельзя:
  ;;    содержимое снова стало бы старым.
  ;; Имена уже освобождены ДО вставки (это делает C:INTEGRATE). Если
  ;; переименовать их здесь ещё раз, освобождённое имя мастер-версии
  ;; снова уйдёт в "~до" и определение окажется пустым -- именно так
  ;; появлялся мусорный вариант "v1.5 до1.5".
  (if KG-PRERENAMES
    (setq renames KG-PRERENAMES)
    (if do-paste
      (progn
        (KG-Mark "освобождение имён до вставки")
        (setq pre (KG-ConflictCandidates model base))
        (setq renames (KG-Step_PreRename pre newiter base model))
      )
      ;; Режим B: мастер-блок вставлен руками ДО запуска команды.
      ;; Переименовывать определения здесь поздно и опасно: вставка уже
      ;; выполнена, пришедшего из буфера ждать нечего, а переименование
      ;; оторвало бы мастер-версию от её собственных вложенных
      ;; определений -- в чертеже остался бы блок со ссылками на
      ;; «Стойка~до8» вместо «Стойка».
      (progn
        (setq renames nil)
        (princ (strcat "\nВНИМАНИЕ: мастер-блок вставлен вручную, вставка из "
                       "буфера внутри команды не выполнялась."
                       "\nВложенные определения в этом режиме обновить нельзя:"
                       " при ручной вставке AutoCAD подставляет уже "
                       "существующие определения с теми же именами"
                       " (\"Duplicate definition of block ... ignored\")."
                       "\nЧтобы вложенные обновились, запустите INTEGRATE и на "
                       "запросе «Мастер-блок:» нажмите Enter -- команда сама "
                       "освободит имена и вставит содержимое буфера."))
      )
    )
  )

  ;; 3. техническая вставка из буфера
  (if do-paste
    (progn
      (setq snap (KG-SnapshotDrawing))
      ;; второй снимок -- ПОСЛЕ переименования и ДО вставки: только по нему
      ;; видно, что пришло именно из буфера (иначе «пришедшими» окажутся
      ;; наши же переименованные имена)
      (setq pastesnap (KG_EXAllDefNames))
      (setq handlesnap
        (KG-Safe '(lambda () (KG_EXDefHandleSnapshot)) nil))
      ;; Имя самой мастер-версии тоже обязано быть свободным ДО вставки,
      ;; иначе AutoCAD отбрасывает пришедшее определение
      ;; ("Duplicate definition of block ... ignored") -- именно так
      ;; команда получила «НЕ получена из буфера» на реальном чертеже.
      (setq masterfreed
        (KG-Safe '(lambda () (KG-FreeMasterName mastername newiter)) nil))
      (setq KG-MASTER-FREED masterfreed)
      (if (not (KG_EXPasteAtOrigin))
        (setq errs (cons "Не удалось вставить содержимое буфера обмена." errs))
      )
      (setq arriveddefs (KG-Step_DetectArrived pastesnap (KG_EXAllDefNames)))
      ;; Сравнение имён ненадёжно: на реальном чертеже оно сказало, что
      ;; мастер-версия не пришла, хотя определение в файле было. Handle
      ;; определения меняется, когда AutoCAD заменяет его пришедшим,
      ;; поэтому список дополняется по снимку handle.
      (if handlesnap
        (foreach nm (KG-Safe '(lambda () (KG-ArrivedByHandle handlesnap)) nil)
          (if (not (member nm arriveddefs))
            (setq arriveddefs (cons nm arriveddefs))
          )
        )
      )
      ;; Имена, которых буфер не принёс, возвращаются на место --
      ;; иначе сторонние семейства и старые вложенные определения
      ;; остались бы под служебными именами «имя~до<итерация>».
      (setq renames (KG-Step_RollbackNotArrived renames arriveddefs))
      ;; карта handle -> пространство построена до вставки и не знает о
      ;; техническом экземпляре; без перестроения он не читается
      (KG-Mark "перестроение карты пространств после вставки")
      (KG-RefreshSpaceMap)
      ;; поиск технического экземпляра не должен ронять команду:
      ;; без него INTEGRATE просто не найдёт вставку и скажет об этом
      (KG-Mark "поиск технического экземпляра среди новых вхождений")
      (setq techhandle
        (KG-Safe '(lambda () (KG-PickMasterHandle (KG_EXNewInstancesSince snap))) nil))
      (if (null techhandle)
        (KG-Mark "технический экземпляр не найден")
        (KG-Mark (strcat "технический экземпляр: " (KG-AsString techhandle)))
      )
      ;; Мастер-версия пришла из буфера тогда и только тогда, когда её имя
      ;; было освобождено нами и после вставки определение снова существует.
      ;; Сверять это по снимку таблицы блоков нельзя: снимок делается до
      ;; освобождения имени и в реальном AutoCAD на таком сравнении
      ;; команда уже получала ложное «НЕ получена из буфера».
      (setq arrived
        (if masterfreed
          (KG_EXDefExists mastername)
          (and (member mastername arriveddefs) t)))
      (if (and (null arrived) masterfreed)
        (progn
          ;; вставка не дала мастер-версию -- возвращаем имя на место,
          ;; иначе определение осталось бы переименованным
          (KG-Safe '(lambda () (KG-RestoreMasterName masterfreed)) nil)
          (setq KG-MASTER-FREED nil)
          (princ (strcat "\nВНИМАНИЕ: мастер-версия из буфера не пришла, "
                         "определение \"" mastername "\" возвращено как было."))
        )
      )
    )
    (progn
      ;; режим B: мастер уже в чертеже. Список пришедших определений
      ;; берётся у вызывающей стороны -- без него проверка «осталось
      ;; старым» сочла бы старым вообще всё.
      (if KG-ARRIVED (setq arriveddefs KG-ARRIVED))
      (setq arrived t)
    )
  )

  ;; 4. модель ПОСЛЕ вставки
  (KG-Mark "чтение модели после вставки")
  ;; Прогон сборки 55: снимок не читался, и команда обрывалась на
  ;; «неверная функция: T» уже после вставки из буфера, с освобождёнными
  ;; именами. Отказ снимка -- неполные данные, а не отказ интеграции.
  (setq model (KG-Safe '(lambda () (KG_DBGetModel)) nil))
  (if (null model)
    (progn
      (princ (strcat "\nВНИМАНИЕ: снимок чертежа не прочитан ("
                     (KG-FailText)
                     "). Отчёт будет неполным, интеграция продолжена."))
      (setq model (list (cons "defs" nil) (cons "insts" nil)
                        (cons "layouts" nil)))
    )
  )

  (if techhandle (setq KG-TECH-HANDLE techhandle))
  ;; Сколько экземпляров каждого варианта уже стоит на новой итерации.
  ;; Считается ДО замены и без технического экземпляра: иначе отчёт
  ;; сравнит «стало» со счётчиком только старых экземпляров и покажет
  ;; ложное расхождение и ложную потерю.
  (setq KG-ALREADYNEW nil)
  (if (/= (KG-AsString newiter) "")
    (foreach ins (KG-ModelInsts model)
      (setq eff (KG-CdrCI "eff" ins))
      (if (and (KG-IsIntegrationName eff base newiter)
               (not (KG-StrEq (KG-CdrCI "handle" ins) techhandle)))
        (progn
          (setq p (KG-ParseBlockName eff))
          (setq v (if p (nth 2 p) ""))
          (setq a (KG-AssocCI v KG-ALREADYNEW))
          (if a
            (setq KG-ALREADYNEW (KG-SetAssoc v (1+ (cdr a)) KG-ALREADYNEW))
            (setq KG-ALREADYNEW (cons (cons v 1) KG-ALREADYNEW))
          )
        )
      )
    )
  )

  (if (not (and arrived (KG-FindDef model mastername)))
    (progn
      ;; имя мастер-версии возвращается ПЕРЕД откатом вложенных имён:
      ;; иначе откат мог бы переименовать определение обратно в имя,
      ;; которое всё ещё занято нашей временной копией мастера
      (if masterfreed
        (KG-Safe '(lambda () (KG-RestoreMasterName masterfreed)) nil))
      (KG-Step_RollbackRename renames)
      (list
        (cons "errors"
          (cons (if arrived
                  (strcat "Определение мастер-версии \"" mastername
                          "\" не найдено в чертеже.")
                  (strcat "Мастер-версия \"" mastername "\" НЕ получена из "
                          "буфера: AutoCAD отбросил пришедшее определение, "
                          "потому что такое имя уже занято в текущем файле "
                          "(\"Duplicate definition of block ... ignored\"). "
                          "Переименуйте или удалите конфликтующее определение, "
                          "скопируйте мастер-блок заново и повторите команду."))
                errs))
        (cons "renames" nil)
        (cons "rolled-back" renames)
      )
    )
    (progn
      ;; 5. план
      (KG-Mark "построение плана интеграции")
      ;; Снимок таблицы блоков ДО вставки передаётся ВСЕГДА, когда он есть,
      ;; а не только когда вставку делает сама эта функция. Иначе
      ;; классификация «обновить / добавить» считается по таблице ПОСЛЕ
      ;; вставки: все вложенные определения к тому моменту уже в файле,
      ;; и план решает, что обновлять нечего. Именно так на реальном
      ;; чертеже получилось «Обновлено вложенных: 0» при 42 «новых».
      (setq plan (KG-BuildIntegrationMap model mastername beforedefs))
      ;; 6. что из вложенных определений реально пришло
      (setq missing nil)
      (foreach v (cdr (assoc "nested-all" plan))
        (if (not (KG-FindDef model v)) (setq missing (cons v missing)))
      )
      (if missing
        (setq warns
          (cons (strcat "Не получены из буфера вложенные определения: "
                        (KG-JoinNames missing)
                        ". Их содержимое осталось прежним.")
                warns))
      )
      ;; вложенные, которые AutoCAD подставил старыми (имя было занято
      ;; и определение не освободилось) -- содержимое НЕ обновилось
      (if beforedefs
        (progn
          (setq stale nil)
          (foreach v (cdr (assoc "nested-to-update" plan))
            (if (not (member v arriveddefs)) (setq stale (cons v stale)))
          )
          (if stale
            (setq warns
              (cons (strcat "ВНИМАНИЕ: вложенные определения "
                            (KG-JoinNames (reverse stale))
                            " вставлены СТАРЫМИ (AutoCAD подставил "
                            "существующие вместо пришедших). Обновите их "
                            "вручную или освободите имена и повторите.")
                    warns))
          )
        )
      )
      ;; 6. варианты
      (KG-Mark "создание определений вариантов")
      (setq created (KG-Step_CreateVariants plan))
      ;; 7. экземпляры
      (KG-Mark "замена экземпляров и перенос состояний видимости")
      (setq rep (KG-Step_ReplaceInstances model plan
                     (cdr (assoc "instances" plan))))
      ;; 8. перепривязка вложенных вхождений внутри сторонних определений
      (KG-Mark "перепривязка вложенных вхождений")
      (KG-Step_RewireNested (KG-Safe '(lambda () (KG_DBGetModel)) nil) renames)
      ;; 8б. перепривязка ссылок внутри определений на прежние имена.
      ;; Идёт ДО очистки: освобождённые здесь служебные определения «~до»
      ;; очистка снимет тем же прогоном.
      (KG-Mark "перепривязка ссылок на прежние имена")
      (setq rewired (KG-Safe '(lambda () (KG-Step_RewireDefs renames)) 0))
      ;; 8в. возврат состояний видимости вложенных блоков подменённым
      ;; определениям. Идёт после перепривязки: к этому моменту вложенные
      ;; вставки уже указывают на прежние имена.
      (KG-Mark "возврат состояний вложенных блоков")
      (setq visrest (KG-Safe '(lambda () (KG-Step_RestoreNestedVis renames))
                             (list 0 0 0)))
      ;; 9. технический экземпляр и старые определения
      (KG-Mark "удаление технического экземпляра")
      (setq deleted (KG-Step_Cleanup renames techhandle))
      ;; 10. контроль
      (KG-Mark "контроль результата")
      (setq val (KG-ValidateIntegration (KG-Safe '(lambda () (KG_DBGetModel)) nil) base newiter
                   (cdr (assoc "groups" (cdr (assoc "scan" plan))))))
      (list
         (cons "plan" plan)
         (cons "renames" renames)
         (cons "created" created)
         (cons "replaced" rep)
         (cons "deleted-defs" deleted)
         (cons "rewired" (KG-AsNum rewired 0))
         (cons "vis-nested-restored" visrest)
         (cons "rewired-skip" (reverse KG-REWSKIP))
        ;; что из запланированного обновления вложенных НЕ обновилось:
        ;; AutoCAD подставил старое определение вместо пришедшего
        (cons "nested-stale" (reverse stale))
        (cons "arrived" arriveddefs)
        (cons "freed" KG-FREED-N)
        (cons "candidates" KG-PRE-CANDIDATES)
        (cons "rename-failed" KG-PRENAME-FAILED)
        (cons "validation" val)
        (cons "warnings" (append warns (cdr (assoc "warnings" rep))))
        (cons "errors" errs)
        ;; семейство нужно отчёту: по нему считается, сколько экземпляров
        ;; реально стоит в чертеже
        (cons "base" base)
      )
    )
  )
)

;; Почему указанное вхождение не годится в мастер-версию.
;; Два принципиально разных случая, и текст должен их различать:
;;   * в имени вообще нет номера итерации -- имя не по схеме;
;;   * номер есть, но есть и суффикс варианта -- это вариант, а не мастер.
(defun KG-MasterRejectMsg (eff / p)
  (setq eff (KG-AsString eff))
  (setq p (KG-ParseBlockName eff))
  (cond
    ((null p)
     (strcat "\nВНИМАНИЕ: \"" eff "\" не подходит под схему именования."
             "\nВ имени нет номера итерации."
             "\nГодятся имена вида \"Комплект КП50 v1.5\" или \"КП50(16)\"."
             "\nНомер новой итерации берётся только из имени мастер-версии."))
    (t
     (strcat "\nВНИМАНИЕ: \"" eff "\" -- это вариант \"" (nth 2 p)
             "\" итерации " (KG-IterToStr (nth 1 p)) ", а не мастер-версия."
             "\nМастер-версия называется \""
             (KG-MakeName (nth 0 p) (nth 1 p) "") "\"."))
  )
)

;;; РАЗДЕЛ 8. ИСПОЛНЯЮЩИЙ СЛОЙ AUTOCAD
;;;
;;; Загружается только внутри AutoCAD. На тестовом стенде эти функции
;;; подставляются стендом (см. tests/run_tests.py).
;;;---------------------------------------------------------------------------

;; Вложенная вставка внутри определения по её эффективному имени.
;; Возвращает vla-объект или nil. Обход только через entget: COM-обход
;; содержимого определений на реальном чертеже дал 0xC0000005.
(defun KG-DefInsertByName (defname nestedname / e ed sub nm out)
  (setq out nil)
  (setq e (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
  (if e
    (while (and (not out)
                (setq e (KG-Safe '(lambda () (entnext e)) nil))
                (setq ed (KG-Safe '(lambda () (entget e)) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT")
        (progn
          (setq sub (KG-Safe '(lambda () (vlax-ename->vla-object e)) nil))
          (if sub
            (progn
              (setq nm (KG-Safe '(lambda () (KG-EffectiveNameOf sub)) nil))
              ;; Сравнение с учётом суффикса «~до»: старое определение
              ;; переименовано ДО чтения экземпляра, поэтому состояние
              ;; прочитано под именем «Стойка КП50~до», а в новом
              ;; определении вложенный блок называется «Стойка КП50».
              ;; В сборке 39 функция KG-NestedKeyMatch была написана, но
              ;; НЕ ВЫЗВАНА ни разу: здесь осталось прежнее KG-StrEq, и
              ;; все шесть состояний снова «не нашлись».
              (if (KG-NestedKeyMatch nm nestedname) (setq out sub))
            )
          )
        )
      )
    )
  )
  out
)

;; Эффективное имя объекта-вхождения. Всегда строка: у некоторых
;; объектов не читается ни EffectiveName, ни Name, и тогда ошибка
;; перехвата уезжала дальше в strcat как «неверный тип аргумента: stringp».
(defun KG-EffectiveNameOf (obj / n)
  (setq n (vl-catch-all-apply '(lambda () (vla-get-EffectiveName obj))))
  (if (KG-IsErr n)
    (setq n (vl-catch-all-apply '(lambda () (vla-get-Name obj)))))
  (if (KG-IsErr n)
    (KG-AsString (cdr (assoc 2 (entget (vl-catch-all-apply
                                         '(lambda () (vlax-vla-object->ename obj)))))))
    (KG-AsString n)
  )
)

;;; Снять маркер успешной выборки с карты ссылок.
;;;
;;; KG-EXRefHolders возвращает ("OK" . карта), чтобы «выборка не
;;; сработала» (nil) отличалась от «вставок нет» (("OK")). Маркер живёт
;;; только в car: любой обход карты -- assoc, vl-some, foreach -- на
;;; строке "OK" спотыкается. В сборке 40 маркер снимался внутри
;;; KG-CleanupCounts и KG-CleanupOrphans, но в контроле после PURGE карта
;;; шла в KG-CdrCI напрямую -- и команда снова падала с «неверный тип
;;; аргумента: consp "OK"». Снимать надо там, где карта получена.
(defun KG-Unmark (x / )
  (if (and x (KG-StrEq (KG-AsString (car x)) "OK")) (cdr x) x)
)

;;; Текст «кто держит определение» для контроля после очистки.
;;; Отдельная функция, чтобы печатающий код не обходил карту сам:
;;; обход с маркером в car -- это и есть падение consp "OK".
(defun KG-CleanupHolderText (holders nm / hh)
  (setq hh (KG-CdrCI (KG-AsString nm) (KG-Unmark holders)))
  (if hh (KG-JoinNames hh) "(прямых вставок нет)")
)

;;; Ename записи блока (самого BLOCK-примитива определения).
;;;
;;; tblobjname на реальном чертеже ответил nil на ВСЕ 37 сиротских *U, и
;;; очистка напечатала «BLOCK не найден» 37 раз подряд: имена приходят из
;;; COM-коллекции Blocks (KG_EXAllDefNames), а ename программа брала из
;;; таблицы блоков. Запасной путь -- tblsearch: у записи таблицы группа -2
;;; и есть ename нужного примитива. В отличие от обхода tblnext + entnext
;;; по всей таблице (на нём чертёж падал с 0xC0000005) tblsearch читает
;;; одну запись и содержимое определений не обходит.
(defun KG-DefEntViaCom (defname / b e)
  ;; Третий путь к ename определения: COM-коллекция Blocks. Она же --
  ;; источник имён (KG_EXAllDefNames), поэтому имя из неё здесь
  ;; находится всегда; остаётся превратить объект в ename.
  (setq b (KG-Safe '(lambda () (KG-BlockObj (KG-AsString defname))) nil))
  (if b
    (setq e (KG-Safe '(lambda () (vlax-vla-object->ename b)) nil))
  )
  e
)

(defun KG-DefEnt (defname / e r src)
  (setq e (KG-Safe '(lambda () (tblobjname "BLOCK" (KG-AsString defname)))
                   nil))
  (if e (setq src "tblobjname"))
  (if (null e)
    (progn
      (setq r (KG-Call 'tblsearch (list "BLOCK" (KG-AsString defname)) nil))
      ;; Прогон сборки 50 закрыл вопрос, который две сборки оставался
      ;; гипотезой. Строка «tblsearch -> запись без -2» печаталась при
      ;; tblobjname -> nil и COM -> nil, то есть все три пути молчали, а
      ;; очистка 19 раз подряд отвечала «BLOCK не найден». Запись при этом
      ;; возвращается. Имя сущности BLOCK в ней лежит в группе -1; группа
      ;; -2 -- это ename первого объекта ВНУТРИ определения, и у пустого
      ;; или анонимного представления её может не быть вовсе. Код брал
      ;; только -2, поэтому ename не находился никогда.
      (if r (setq e (cdr (assoc -2 r))))
      (if e
        (setq src "tblsearch (-2)")
        (progn
          (if r (setq e (cdr (assoc -1 r))))
          (if e (setq src "tblsearch (-1)"))
        )
      )
      (if (null e)
        (progn
          (setq e (KG-Safe '(lambda () (KG-DefEntViaCom defname)) nil))
          (if e (setq src "COM"))
        )
      )
      (if e
        (KG-TraceDetail (strcat "BLOCK для \"" (KG-AsString defname)
                                "\" взят из " src))
        (KG-Mark (strcat "нет BLOCK для \"" (KG-AsString defname)
                         "\": tblobjname -> nil, tblsearch -> "
                         (if r "запись без -1 и -2" "nil")
                         ", COM -> nil"))
      )
    )
  )
  e
)

;;;--- Чтение вхождений ----------------------------------------------------
;;; Вынесено ИЗ исполняющего слоя: этот цикл падал на реальном чертеже
;;; три прогона подряд, и проверять его надо на стенде. Все COM-вызовы
;;; внутри -- под перехватом, поэтому вне AutoCAD он просто отдаёт
;;; меньше данных, а не падает.

;;; Модель одного экземпляра.
;;; Каждое поле читается отдельно и со своим запасным значением: на
;;; реальном чертеже часть свойств может не читаться (анонимный
;;; динамический блок, объект внешней ссылки), и раньше любое такое поле
;;; обрывало чтение ВСЕХ вхождений -- чертёж оказывался «пустым».
;;; Метки KG-Mark называют поле: при падении команда назовёт его.
;;; Печатаются они только при (setq KG-TRACE t) -- иначе на большом
;;; чертеже командная строка переполняется и хвост отчёта теряется.
(defun KG-InstanceModel (obj e / ed h defnm eff sp ip lay out nv)
  (setq ed (entget e))
  (setq h (KG-AsString (cdr (assoc 5 ed))))

  (KG-TraceDetail (strcat "вхождение " h ": имя определения"))
  (setq defnm (KG-AsString (cdr (assoc 2 ed))))

  (KG-TraceDetail (strcat "вхождение " h ": эффективное имя"))
  (setq eff (KG-AsString (KG-EffectiveNameOf obj)))
  (if (= eff "") (setq eff defnm))

  (KG-TraceDetail (strcat "вхождение " h ": пространство"))
  (setq sp (KG-Safe '(lambda () (KG-SpaceOf e)) nil))

  (KG-TraceDetail (strcat "вхождение " h ": точка вставки"))
  ;; InsertionPoint приходит variant'ом; (nth …) над ним даёт
  ;; «неверный тип аргумента: consp #<variant …>»
  (setq ip (KG-PointValue
             (KG-Safe '(lambda () (vlax-get-property obj 'InsertionPoint)) nil)
             (list 0.0 0.0 0.0)))

  (KG-TraceDetail (strcat "вхождение " h ": слой"))
  (setq lay (KG-AsString (KG-Field obj 'Layer "0")))

  (KG-TraceDetail (strcat "вхождение " h ": поворот и масштабы"))
  ;; поля читаются каждое под своим перехватом: нечитаемое свойство
  ;; даёт запасное значение, а не обрывает чтение экземпляра
  (setq out
    (list
      (cons "handle" h)
      (cons "def" defnm)
      (cons "eff" eff)
      (cons "space" sp)
      (cons "layer" lay)
      (cons "pos" ip)
      (cons "rot" (KG-AsNum (KG-Field obj 'Rotation 0.0) 0.0))
      (cons "sx" (KG-AsNum (KG-Field obj 'XScaleFactor 1.0) 1.0))
      (cons "sy" (KG-AsNum (KG-Field obj 'YScaleFactor 1.0) 1.0))
      (cons "sz" (KG-AsNum (KG-Field obj 'ZScaleFactor 1.0) 1.0))
      (cons "normal" (list 0.0 0.0 1.0))
    )
  )

  (KG-TraceDetail (strcat "вхождение " h ": состояния вложенных блоков"))
  (setq nv (KG-Safe '(lambda () (KG-InstanceNestedVisibility obj)) nil))
  (if (not nv) (setq nv nil))
  (setq out (cons (cons "nested-vis-src" (KG-AsString KG-LAST-NESTED-SRC)) out))
  ;; Число прочитанных состояний хранится рядом со списком: по одному
  ;; нулю в отчёте нельзя отличить «состояний не было» от «не
  ;; прочитались», а это два разных дефекта.
  (setq out (cons (cons "nested-vis-n" (length nv)) out))
  (setq out (cons (cons "nested-vis" nv) out))

  (KG-TraceDetail (strcat "вхождение " h ": сбор атрибутов"))
  (setq out (cons (cons "attrs"
                        (KG-Safe '(lambda () (KG-GetAttributeValues obj)) nil))
                  out))

  (KG-TraceDetail (strcat "вхождение " h ": динамические свойства"))
  (setq out (cons (cons "dyn-props"
                        (KG-Safe '(lambda () (KG-GetDynamicProperties obj)) nil))
                  out))
  out
)

;; Все вхождения блоков во всём файле (модель + все листы + определения).
;; Возвращает список моделей экземпляров.
(defun KG-CollectInstances ( / ss i e obj m out skipped nospace iserr msg)
  (setq out nil skipped 0 nospace 0)
  (KG-Mark "сбор вхождений блоков")
  (KG-Mark "карта handle -> пространство")
  (setq KG-SPACE-MAP (KG-BuildSpaceMap))
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      ;; entget здесь -- вне перехвата: на вхождении, которое не читается,
      ;; команда оборвалась бы ещё до того, как дошла до проверки
      ;; результата. Handle берётся под перехватом.
      (KG-TraceDetail (strcat "чтение вхождения "
                              (KG-AsString
                                (KG-Safe '(lambda () (cdr (assoc 5 (entget e))))
                                         ""))))
      (setq obj (vl-catch-all-apply '(lambda () (vlax-ename->vla-object e))))
      ;; Метки ПОСЛЕ каждого этапа. В прогоне сборки 48 команда упала с
      ;; «неверная функция: T», а последним напечатанным шагом был
      ;; «вхождение ...: динамические свойства» -- то есть метка, которая
      ;; ставится ДО чтения свойств. Всё, что исполняется после неё,
      ;; оказалось не покрыто метками, и место падения определить не
      ;; удалось. Теперь каждый этап назван.
      ;; KG-IsErr зовётся только под перехватом: vl-catch-all-error-p от
      ;; живого vla-объекта в AutoCAD сам бросает ошибку, а метка стоит
      ;; вне перехвата -- так приборка сама уронила бы команду.
      (KG-TraceDetail (strcat "вхождение получено как объект: "
                              (if (KG-Safe '(lambda () (KG-IsErr obj)) nil)
                                "ОШИБКА" "да")))
      ;; ошибка на одном вхождении не должна обрывать обработку остальных,
      ;; но причину нужно видеть: без неё «не удалось прочитать» бесполезно
      (setq m (if (KG-IsErr obj) obj
                (vl-catch-all-apply '(lambda () (KG-InstanceModel obj e)))))
      (KG-TraceDetail (strcat "модель вхождения прочитана: "
                              (if (KG-Safe '(lambda () (KG-IsErr m)) nil)
                                "ОШИБКА"
                                (if m "да" "nil"))))
      ;; Прогон сборки 49: последняя метка -- «модель вхождения прочитана:
      ;; да», а падение -- до строки «вхождений прочитано». Между ними
      ;; шесть выражений, и ни одно по коду не может дать «неверная
      ;; функция: T». Значит, отказ даёт то, что выглядит безопасным, и
      ;; искать надо перебором: каждое выражение под своим перехватом и со
      ;; своей меткой. Побочный эффект -- отказ одного вхождения больше не
      ;; обрывает команду.
      (KG-TraceDetail "проверка результата чтения")
      ;; Проверка под перехватом: если она сама откажет, iserr останется
      ;; nil, вхождение уйдёт в «пропущено», а метка назовёт этап.
      (setq iserr (KG-Safe '(lambda () (KG-IsErr m)) nil))
      (KG-TraceDetail (strcat "проверка результата: "
                              (if iserr "ошибка" "не ошибка")))
      (if iserr
        (progn
          (KG-TraceDetail "печать предупреждения о вхождении")
          (setq msg (KG-Safe
                      '(lambda ()
                         (strcat "\nВНИМАНИЕ: вхождение "
                                 (KG-AsString (cdr (assoc 5 (entget e))))
                                 " (определение \""
                                 (KG-AsString (cdr (assoc 2 (entget e))))
                                 "\"): " (vl-catch-all-error-message m)))
                      "\nВНИМАНИЕ: вхождение не прочитано (текст причины не собрался)."))
          (princ (KG-AsString msg))
        )
      )
      (KG-TraceDetail "разбор пространства вхождения")
      (if (KG-Safe '(lambda () (KG-CdrCI "space" m)) nil)
        nil
        (setq nospace (1+ nospace)))
      (KG-TraceDetail "вхождение добавлено в список")
      (if (and m (not iserr))
        (setq out (cons m out))
        (setq skipped (1+ skipped))
      )
    )
  )
  ;; Прогон сборки 53 назвал место: «ВНИМАНИЕ: вхождения не прочитаны
  ;; (счёт вхождений)» при том, что все три вхождения к этому моменту были
  ;; прочитаны и добавлены в список. Отказ происходил в подсчёте -- и
  ;; уносил ВЕСЬ список: функция обрывалась, вызывающий получал nil, план
  ;; строился по пустой модели, и INTEGRATE печатала «Заменено 0» при двух
  ;; живых экземплярах в чертеже.
  ;; Подсчёт и печать теперь под перехватом, а (reverse out) -- ПОСЛЕ него:
  ;; отказ строки трассировки больше не стоит команде всех экземпляров.
  ;; Три этапа под отдельными перехватами, а не один на всё.
  ;;
  ;; Прогон сборки 58 назвал место: причина отказа -- шаг "счёт вхождений",
  ;; "неверная функция: T", а строки «вхождений прочитано: N» в логе нет
  ;; ни разу за четыре чтения модели. Значит отказ в этой группе, но из
  ;; четырёх вызовов под одним перехватом не видно, в каком именно.
  ;; Теперь каждый этап назван отдельно, и отказ одного не мешает другим.
  (setq KG-CNT-OK (KG-Safe '(lambda () (KG-Mark "счёт вхождений") t) nil))
  (if (not KG-CNT-OK)
    (princ (strcat "\nВНИМАНИЕ: не поставилась метка \"счёт вхождений\" ("
                   (KG-FailText) ")."))
  )
  (setq KG-INSTCOUNT (KG-Safe '(lambda () (length out)) nil))
  (setq KG-CNT-OK
    (KG-Safe '(lambda ()
                (princ (strcat "\n[счёт] вхождений прочитано: "
                               (KG-AsString KG-INSTCOUNT)
                               (if (> skipped 0)
                                 (strcat ", пропущено из-за ошибок: "
                                         (KG-AsString skipped))
                                 "")))
                t)
              nil))
  (if (not KG-CNT-OK)
    (princ (strcat "\nВНИМАНИЕ: не напечаталось число вхождений ("
                   (KG-FailText) "), значение: "
                   (KG-AsString KG-INSTCOUNT)
                   ", пропущено: " (KG-AsString skipped) "."))
  )
  (setq KG-CNT-OK
    (KG-Safe '(lambda ()
                (if (> skipped 0)
                  (princ (strcat "\nВНИМАНИЕ: " (KG-AsString skipped)
                                 " вхождений не удалось прочитать,"
                                 " они пропущены."))
                )
                (if (> nospace 0)
                  (princ (strcat "\nВНИМАНИЕ: у " (KG-AsString nospace)
                                 " вхождений не определено пространство"
                                 " (они лежат внутри определений блоков);"
                                 " они не заменяются,"
                                 " только перепривязываются."))
                )
                t)
              nil))
  (if (not KG-CNT-OK)
    (princ (strcat "\nВНИМАНИЕ: не напечатались счётчики пропусков ("
                   (KG-FailText) "), пропущено: " (KG-AsString skipped)
                   ", без пространства: " (KG-AsString nospace) "."))
  )
  (if (KG-Safe '(lambda () (not out)) t)
    (princ "\nВНИМАНИЕ: список вхождений не читается, возвращаем пустой."))
  (KG-Safe '(lambda () (reverse out)) nil)
)

;; Вставки, которые указывают на отсутствующее определение.
;;
;; Повод -- прогон сборки 50: INTEGRATE напечатала «УСПЕШНО, замечаний
;; нет» при «Заменено экземпляров: 0», а в чертёже остались два экземпляра
;; семейства. Валидация этого не увидела, потому что искала старые
;; экземпляры ПО ИМЕНИ варианта, а определения к тому моменту были
;; переименованы и сняты. Признак такого состояния общий и проверяется без
;; знания плана: у вставки есть группа 2, а записи блока с этим именем нет.
;; Стоит функция до границы исполняющего слоя, чтобы её можно было
;; проверить на стенде.
(defun KG-DanglingInserts ( / ss i e ed nm n out)
  (setq n 0 out nil)
  (setq ss (KG-Safe '(lambda () (ssget "_X" '((0 . "INSERT")))) nil))
  (if ss
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      (setq ed (KG-Safe '(lambda () (entget e)) nil))
      (setq nm (if ed (KG-AsString (cdr (assoc 2 ed))) ""))
      (if (and (/= nm "")
               (not (KG-Safe '(lambda () (KG_EXDefExists nm)) nil)))
        (progn
          (setq n (1+ n))
          (if (< (length out) 10)
            (setq out (cons (strcat nm " (вставка "
                                    (KG-AsString
                                      (KG-Safe
                                        '(lambda () (cdr (assoc 5 ed))) ""))
                                    ")")
                            out)))
        )
      )
    )
  )
  (list n (reverse out))
)

;; Сколько вставок семейства стоит в чертеже и какими именами они видны.
;;
;; Повод -- прогон, в котором INTEGRATE напечатала «УСПЕШНО, замечаний
;; нет» при «Заменено экземпляров: 0». По отчёту нельзя было отличить
;; «в чертеже действительно нет экземпляров семейства» от «экземпляры
;; есть, но сканирование увидело их под другими именами». Различие
;; принципиальное: во втором случае имена экземпляров меняет наше же
;; освобождение имён до вставки, и план оказывается пустым.
;; Считает по вставкам напрямую, минуя модель и план.
(defun KG-FamilyInstances (base / ss i e ed eff out n lst)
  (setq n 0 lst nil)
  (setq ss (KG-Safe '(lambda () (ssget "_X" '((0 . "INSERT")))) nil))
  (if ss
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      (setq ed (KG-Safe '(lambda () (entget e)) nil))
      (setq eff "")
      (if ed
        (setq eff (KG-AsString
                    (KG-Safe
                      '(lambda ()
                         (KG-EffectiveNameOf (vlax-ename->vla-object e)))
                      ""))))
      (if (= eff "")
        (if ed (setq eff (KG-AsString (cdr (assoc 2 ed))))))
      (setq out (KG-Safe '(lambda () (KG-ParseBlockName eff)) nil))
      (if (and out (KG-StrEq (KG-AsString (nth 0 out))
                             (KG-AsString base)))
        (progn
          (setq n (1+ n))
          (if (not (KG-StrInterCI (list eff) lst))
            (setq lst (cons eff lst)))
        )
      )
    )
  )
  (list n (reverse lst))
)

;; Variant -> safearray, либо nil.
;;; AllowedValues у свойства без списка допустимых значений возвращает
;;; ПУСТОЙ variant (тип 8197, VT_EMPTY). Развёртывание такого значения
;;; через vlax-variant-value бросает ошибку, и именно она обрывала команду:
;;; в первом логе это было "consp #<variant 8197>".
(defun KG-VariantToArray (v / a)
  (if (or (null v) (KG-IsErr v))
    nil
    (progn
      (setq a (vl-catch-all-apply '(lambda () (vlax-variant-value v))))
      (if (KG-IsErr a)
        nil
        (if (KG-IsErr (vl-catch-all-apply
                        '(lambda () (vlax-safearray-get-u-bound a 1))))
          nil
          a))
    )
  )
)

;; Число элементов в safearray (0, если это не массив). Всегда число:
;; результат уходит в (> … 0), а там nil дал бы «fixnump: nil».
(defun KG-ArrayCount (arr / n)
  (if (null arr)
    0
    (progn
      (setq n (vl-catch-all-apply '(lambda () (vlax-safearray-get-u-bound arr 1))))
      (if (KG-Num n) (1+ n) 0)
    )
  )
)

;; Имя свойства среди динамических свойств объекта. Отдельная функция:
;; KG-FindVisibilityProperty зовётся с аргументом, и вложенная лямбда,
;; цитирующая локальную переменную вызывающей функции, подставляется
;; ненадёжно.
(defun KG-FindDynPropByName (obj want / props p nm out)
  (setq out nil)
  (setq props (vl-catch-all-apply
                '(lambda () (vlax-invoke obj 'GetDynamicBlockProperties))))
  (if (and (not (KG-IsErr props)) props)
    (foreach p props
      (if (not out)
        (progn
          (setq nm (vl-catch-all-apply '(lambda () (vla-get-PropertyName p))))
          (if (and (not (KG-IsErr nm)) (KG-StrEq nm want)) (setq out p))
        )
      )
    )
  )
  out
)

;; Свойство среди динамических свойств объекта по признакам.
(defun KG-FindDynPropBy (obj / props p nm allowed out)
  (setq out nil)
  (setq props (vl-catch-all-apply
                '(lambda () (vlax-invoke obj 'GetDynamicBlockProperties))))
  (if (and (not (KG-IsErr props)) props)
    (foreach p props
      (if (not out)
        (progn
          (setq nm (vl-catch-all-apply '(lambda () (vla-get-PropertyName p))))
          (setq allowed (vl-catch-all-apply
                          '(lambda () (vlax-get-property p 'AllowedValues))))
          ;; Параметр видимости обязан иметь список состояний; пустой
          ;; variant (VT_EMPTY) означает, что это не он. Точное имя здесь
          ;; намеренно НЕ сверяется: это задача KG-FindDynPropByName.
          ;; Пока сверялось и здесь, запасной путь подменял основной, и
          ;; «имя из словаря не используется» не меняло результата.
          (if (and (not (KG-IsErr nm))
                   (> (KG-ArrayCount (KG-VariantToArray allowed)) 0)
                   (wcmatch (strcase (KG-AsString nm)) "*ВИДИМ*,*VISIB*"))
            (setq out p)
          )
        )
      )
    )
  )
  out
)

;; Имя параметра видимости из словаря расширения определения блока.
;;
;; Это единственный способ узнать имя, не угадывая его. Параметр видимости
;; хранится в ACAD_ENHANCEDBLOCK расширения записи блока как сущность
;; BLOCKVISIBILITYPARAMETER, а его имя -- в группе 301.
;;
;; Почему это понадобилось. Параметр искали по написанию имени --
;; «*ВИДИМ*» или «*VISIB*». Прогон сборки 63 показал: у вложенных блоков
;; определений «Стойка КП50» и «Стойка КП50 контур» читается ноль состояний
;; при четырёх и шести вложенных вставках, а у «Закладная в полость стойки»
;; состояния есть. Значит, у части блоков параметр назван иначе, и поиск по
;; имени его не находил -- состояния не снимались и вернуть их было нечего.
;; Пользователь видел ровно это: «состояние сброшено на базовую видимость».
(defun KG-VisParamName (obj / eff blk d e ed out x)
  (setq out nil)
  (setq eff (KG-EffectiveNameOf obj))
  (setq blk (if (and eff (/= eff "")) (KG-BlockObj eff) nil))
  (setq d nil)
  (if (and blk (not (KG-IsErr blk)))
    (progn
      (setq e (vl-catch-all-apply
                '(lambda () (vlax-vla-object->ename blk))))
      (if (not (KG-IsErr e))
        (progn
          (setq d (KG-Safe '(lambda ()
                              (dictsearch (entget e) "ACAD_ENHANCEDBLOCK"))
                           nil))
          (if d
            (progn
              (setq e (cdr (assoc 360 d)))
              (if e
                (progn
                  (setq ed (KG-Safe '(lambda () (entget e)) nil))
                  (foreach x ed
                    (if (and (not out) (KG-StrEq (car x) 301))
                      (setq out (KG-AsString (cdr x)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (if (and out (/= out "")) out nil)
)

;; Параметр видимости объекта.
;;
;; Порядок поиска: точное имя из словаря определения, затем прежнее
;; распознавание по написанию имени. Первый способ основной: он не зависит от
;; того, как параметр назвали. Второй оставлен запасным -- на случай, когда
;; словарь не читается.
;;
;; Третий аргумент -- куда записать, каким способом параметр найден:
;; "имя" или "написание". Без этого по отчёту не отличить «параметр не найден»
;; от «найден запасным путём».
(defun KG-FindVisibilityProperty (obj want how / p)
  (setq p nil)
  (if want (setq p (KG-FindDynPropByName obj want)))
  (if (and p how) (setq how "имя"))
  (if (null p)
    (progn
      (setq p (KG-FindDynPropBy obj))
      (if (and p how) (setq how "написание"))
    )
  )
  p
)

;; Параметр видимости: имя берётся из словаря определения.
(defun KG-VisibilityPropertyOf (obj / want)
  (setq want (KG-Safe '(lambda () (KG-VisParamName obj)) nil))
  (KG-FindVisibilityProperty obj want nil)
)

;; Список допустимых состояний видимости экземпляра
(defun KG-GetAllowedVisibilityStates (obj / p arr i n out)
  (setq out nil)
  (setq p (KG-VisibilityPropertyOf obj))
  (if p
    (progn
      (setq arr (KG-VariantToArray
                  (vl-catch-all-apply '(lambda () (vlax-get-property p 'AllowedValues)))))
      (if arr
        (progn
          (setq n (1- (KG-ArrayCount arr)))
          (setq i 0)
          (while (<= i n)
            (setq out (cons (KG-AsString
                              (vl-catch-all-apply
                                '(lambda ()
                                   (vlax-variant-value
                                     (vlax-safearray-get-element arr i)))))
                            out))
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (reverse out)
)

;; Текущее состояние видимости экземпляра. Всегда строка или nil.
;; Значение параметра приходит variant'ом (VT_BSTR): на реальном чертеже
;; в предупреждениях печаталось «#<variant 8 КП45303 70/45>» вместо имени
;; состояния, а сравнение со списком допустимых состояний такой пары не
;; находило -- состояние считалось отсутствующим в новой версии.
(defun KG-GetVisibilityState (obj / p v sv)
  (setq p (KG-VisibilityPropertyOf obj))
  (if p
    (progn
      (setq v (vl-catch-all-apply '(lambda () (vlax-get-property p 'Value))))
      (if (KG-IsErr v)
        nil
        (progn
          (setq sv (vl-catch-all-apply '(lambda () (vlax-variant-value v))))
          (if (KG-IsErr sv) (KG-AsString v) (KG-AsString sv))
        )
      )
    )
    nil
  )
)

;; Установить состояние видимости
(defun KG-SetVisibilityState (obj val / p r)
  (setq p (KG-VisibilityPropertyOf obj))
  (if p
    (progn
      (setq r (vl-catch-all-apply '(lambda () (vlax-put-property p 'Value val))))
      (not (KG-IsErr r))
    )
    nil
  )
)

(if (not KG-TESTING)
(progn

;;;--- Служебные объекты COM -------------------------------------------------

(defun KG-Acad ( / *acad*)
  (setq *acad* (vlax-get-acad-object))
)
(defun KG-Doc () (vla-get-ActiveDocument (KG-Acad)))
(defun KG-Model ( / d) (setq d (KG-Doc)) (vla-get-ModelSpace d))
(defun KG-Blocks () (vla-get-Blocks (KG-Doc)))

;; Объект-определение по имени, nil если нет
(defun KG-BlockObj (name)
  (vl-catch-all-apply
    '(lambda () (vla-Item (KG-Blocks) name)))
)

;; Имя листа по имени блока *Paper_Space*
(defun KG-LayoutNameFor (blockname / lays lay res)
  (setq res blockname)
  (setq lays (vl-catch-all-apply '(lambda () (vla-get-Layouts (KG-Doc)))))
  (if (not (KG-IsErr lays))
    (vlax-for lay lays
      (if (KG-StrEq (vl-catch-all-apply '(lambda () (vla-get-Name lay))) res)
        (setq res (vla-get-Name lay))
      )
    )
  )
  res
)

;;;--- Пространство экземпляра -----------------------------------------------
;;;
;;; Группа 330 (handle владельца) в entget экземпляра есть НЕ во всех
;;; файлах: на реальном чертеже (прогон FINDBUG, 18.09) её не оказалось
;;; ни у одного INSERT, поэтому handent получал nil и падал на каждом
;;; вхождении -- чертеж читался как пустой («прочитано: 0, с ошибками: 3»).
;;;
;;; Рабочий путь: пройти таблицу блоков и для модели и каждого листа
;;; спросить у пространства handle каждого лежащего в нём объекта
;;; (vla-get-Handle). Карта строится один раз за команду.
;;; Группа 330 остаётся запасным вариантом.

;; Пространство экземпляра: "Model" или имя листа.
;;; Принимает ename. nil означает «не удалось определить» -- вызывающий
;;; обязан это обработать, а не считать пространством модель.
(defun KG-SpaceOf (e / h sp)
  (setq h (KG-HandleOf e))
  (setq sp (if h (KG-SpaceOfHandle h) nil))
  (cond
    (sp sp)
    (h (progn
         (setq sp (KG-OwnerBlockName e))
         (cond
           ((null sp) nil)
           ((KG-StrEq sp "*Model_Space") "Model")
           (t (KG-LayoutNameFor sp)))))
    (t nil)
  )
)

;; Handle объекта как строка (группа 5)
(defun KG-HandleOf (e / ed)
  (if (or (null e) (= (type e) (type "")))
    nil
    (progn
      (setq ed (entget e))
      (if ed (KG-AsString (cdr (assoc 5 ed))) nil)
    )
  )
)

;; Handle -> имя пространства по построенной карте, nil если handle не найден
(defun KG-SpaceOfHandle (handle / h p)
  (setq h (strcase (KG-AsString handle)))
  (setq p (assoc h KG-SPACE-MAP))
  (if p (cdr p) nil)
)

;; Карта handle -> имя пространства для модели и всех листов
(defun KG-BuildSpaceMap ( / blocks blk nm sp lay map n)
  (setq map nil n 0)
  (setq blocks (vl-catch-all-apply '(lambda () (KG-Blocks))))
  (if (not (KG-IsErr blocks))
    (vlax-for blk blocks
      (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
      (if (and (not (KG-IsErr nm))
               (not (KG-ComBool
                      (vl-catch-all-apply '(lambda () (vla-get-IsXRef blk))))))
        (progn
          (setq sp
            (cond
              ((wcmatch (strcase (KG-AsString nm)) "`*MODEL_SPACE*") "Model")
              ((wcmatch (strcase (KG-AsString nm)) "`*PAPER_SPACE*")
               (setq lay (vl-catch-all-apply '(lambda () (vla-get-Layout blk))))
               (if (or (KG-IsErr lay) (null lay))
                 (KG-AsString nm)
                 (progn
                   (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name lay))))
                   (if (KG-IsErr nm) nil (KG-AsString nm)))))
              (t nil)))
          (if sp
            (progn
              (setq n (1+ n))
              (setq map (KG-MapSpaceEntities blk sp map))
            )
          )
        )
      )
    )
  )
  ;; обход таблицы блоков может не дать ничего (на реальном чертеже так и
  ;; вышло: «объектов в карте пространств: 0»). Тогда пространства
  ;; берутся из коллекции Layouts -- это второй, независимый путь.
  (if (null map)
    (progn
      (KG-Mark "карта по таблице блоков пуста, обход через Layouts")
      (setq map (KG-BuildSpaceMapByLayouts))
    )
  )
  (KG-Mark (strcat "карта пространств: " (itoa (length map))
                    " объектов, пространств пройдено: " (itoa n)))
  (reverse map)
)

;; Запасной путь: пространства из коллекции Layouts
(defun KG-BuildSpaceMapByLayouts ( / lays lay nm blk map)
  (setq map nil)
  (setq lays (vl-catch-all-apply '(lambda () (vla-get-Layouts (KG-Doc)))))
  (if (not (KG-IsErr lays))
    (vlax-for lay lays
      (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name lay))))
      (setq blk (vl-catch-all-apply '(lambda () (vla-get-Block lay))))
      (if (and (not (KG-IsErr nm)) nm (not (KG-IsErr blk)) blk)
        (setq map (KG-MapSpaceEntities blk (KG-AsString nm) map))
      )
    )
  )
  map
)

;; Дописать в карту handle всех объектов пространства blk
(defun KG-MapSpaceEntities (blk sp map / h)
  (vl-catch-all-apply
    '(lambda ()
       (vlax-for e blk
         (setq h (vl-catch-all-apply '(lambda () (vla-get-Handle e))))
         (if (and (not (KG-IsErr h)) h)
           (setq map (cons (cons (strcase (KG-AsString h)) sp) map))
         )
       )))
  map
)

;; Перестроить карту handle -> пространство (после вставки из буфера)
(defun KG-RefreshSpaceMap ()
  (setq KG-SPACE-MAP (KG-BuildSpaceMap))
)

;; Имя блока-владельца экземпляра через группу 330 (запасной путь)
(defun KG-GetDynamicProperties (obj / props p nm val out)
  (setq out nil)
  ;; Никаких предварительных проверок доступности.
  ;;
  ;; vlax-property-available-p проверяет СВОЙСТВА, а
  ;; GetDynamicBlockProperties -- МЕТОД: проверка всегда возвращала nil,
  ;; условие (and ...) не выполнялось, и у каждого экземпляра читался
  ;; пустой список свойств. Отчёт печатал «Прочитано свойств у старых
  ;; экземпляров: 0», и состояния видимости не переносились.
  ;; Правильной была бы vlax-method-applicable-p, но проще и надёжнее
  ;; сразу вызвать метод под перехватом: у статического блока он
  ;; откажет, и список останется пустым.
  (if obj
    (progn
      (setq props (vl-catch-all-apply
                    '(lambda () (vlax-invoke obj 'GetDynamicBlockProperties))))
      (if (and (not (KG-IsErr props)) props)
        (foreach p props
          (setq nm (vl-catch-all-apply '(lambda () (vla-get-PropertyName p))))
          (setq val (vl-catch-all-apply '(lambda () (vlax-get-property p 'Value))))
          (if (and (not (KG-IsErr nm)) (not (KG-IsErr val)))
            (setq out (cons (cons nm val) out))
          )
        )
      )
    )
  )
  (reverse out)
)

;; Эвристика поиска параметра видимости (п. 9.5 ТЗ).
;; Признаки: имя содержит "видим"/"visib", есть список допустимых значений,
;; свойство доступно для записи.
;









;; Состояния видимости вложенных динамических блоков внутри определения
(defun KG-DefNestedVisibility (defname / blk out ent eo)
  (setq out nil)
  (setq blk (KG-BlockObj defname))
  (if (and blk (not (KG-IsErr blk)))
    (vlax-for e blk
      (if (= (vla-get-ObjectName e) "AcDbBlockReference")
        (progn
          (setq eo e)
          (setq out
            (cons (cons (KG-EffectiveNameOf eo)
                        (KG-GetVisibilityState eo))
                  out))
        )
      )
    )
  )
  (reverse out)
)

;;;--- Адаптер чтения: построение модели чертежа -----------------------------

(defun KG_DBGetModel ( / defs insts lays blk e defs-l insts-l nm d err)
  (KG-Mark "чтение таблицы блоков")
  (setq defs-l nil)
  ;; список пропущенных сбрасывается на каждый прогон: иначе имена
  ;; прошлого отказа висели бы в отчёте следующего.
  (setq KG-SKIP-DEFS nil)
  (vlax-for blk (KG-Blocks)
    (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
    ;; Проверка результата тоже перехвачена: (vl-catch-all-error-p) от
    ;; неожиданного значения сам бросает «неверная функция: T», и тогда
    ;; метка «чтение определения» уже напечатана, а причина осталась бы
    ;; за пределами перехвата свойств ниже.
    (if (not (KG-IsErrSafe nm t))
      (progn
        (KG-TraceDetail (strcat "чтение определения \""
                                (KG-AsString nm) "\""))
        ;; Определение может не читаться: анонимный динамический блок,
        ;; внешняя ссылка, определение, которое ActiveX отдаёт с
        ;; ошибкой. Отказ ОДНОГО определения не обязан обрывать всю
        ;; команду. Прогон сборки 55: снимок не прочитан на "*U54",
        ;; а второй снимок оборвал INTEGRATE целиком на "*U87" --
        ;; «неверная функция: T», уже после вставки из буфера.
        ;; Теперь определение пропускается, причина печатается, а
        ;; модель собирается из остальных.
        (setq d (vl-catch-all-apply
                  '(lambda ()
                     (list
                       (cons "name" nm)
                       (cons "is-xref"
                             (KG-ComBool
                               (vl-catch-all-apply
                                 '(lambda () (vla-get-IsXRef blk)))))
                       (cons "is-layout"
                             (if (wcmatch (strcase (KG-AsString nm))
                                          "`*MODEL_SPACE,`*PAPER_SPACE*")
                               t nil))
                       (cons "is-dynamic"
                             (KG-ComBool
                               (vl-catch-all-apply
                                 '(lambda () (vla-get-IsDynamicBlock blk)))))
                       (cons "nested" (KG-DefNestedRefs nm))
                       (cons "vis-states" (KG-DefVisStates nm))
                       (cons "vis-param" nil)
                     ))))
        (if (KG-IsErrSafe d t)
          (progn
            ;; имя отказа обязательно: без него «пропущено»
            ;; неотличимо от «определения не было»
            (setq err (KG-AsString
                        (vl-catch-all-apply
                          '(lambda () (vl-catch-all-error-message d)))))
            (setq KG-SKIP-DEFS (cons (KG-AsString nm) KG-SKIP-DEFS))
            (princ (strcat "\nВНИМАНИЕ: определение \""
                           (KG-AsString nm) "\" не прочитано ("
                           err ") -- пропущено, обработка продолжается."))
          )
          (if d (setq defs-l (cons d defs-l)))
        )
      )
    )
  )
  ;; Метка ПОСЛЕ цикла: по ней видно, дочитан ли список определений.
  ;; Без неё «чтение определения "X"» оставалась последней меткой и
  ;; при отказе в самом обходе коллекции -- то есть указывала не туда.
  (KG-Mark "таблица блоков прочитана")
  ;; Счётчик прочитанных определений -- и заодно диагностика.
  ;;
  ;; Прогон сборки 57: интеграция прошла полностью, но шесть раз
  ;; напечаталось «не напечаталось число определений (шаг "таблица
  ;; блоков прочитана", неверная функция: T)». Строка состояла из
  ;; четырёх вызовов под одним перехватом, и какой из них отказал --
  ;; видно не было. Теперь вызов один, а печатается сырое значение:
  ;; отказ печати отделён от отказа подсчёта, и предупреждение называет
  ;; значение, на котором случилось.
  (setq KG-DEFCOUNT (KG-Safe '(lambda () (length defs-l)) nil))
  (setq KG-CNT-OK (KG-Safe '(lambda ()
                              (princ (strcat "\n[счёт] определений в списке: "
                                             (KG-AsString KG-DEFCOUNT)))
                              t)
                            nil))
  (if (not KG-CNT-OK)
    (princ (strcat "\nВНИМАНИЕ: не напечаталось число определений ("
                   (KG-FailText) "), значение: "
                   (KG-AsString KG-DEFCOUNT) "."))
  )
  (if KG-SKIP-DEFS
    (if (not (KG-Safe '(lambda ()
                        (KG-Say (strcat
                          "Определений не прочитано и пропущено: "
                          (KG-NumStr (length KG-SKIP-DEFS))))
                        (KG-SayList (reverse KG-SKIP-DEFS) 10)
                        t)
                      nil))
      (princ (strcat "\nВНИМАНИЕ: не напечатался список пропущенных ("
                     (KG-FailText) ")."))
    )
  )
  ;; Сбор вхождений может не дочитаться (прогон сборки 49). Лучше
  ;; INTEGRATE продолжится с неполными данными и назовёт причину, чем
  ;; оборвётся посередине, оставив имена освобождёнными.
  (KG-Mark "сбор вхождений")
  (setq insts-l (KG-Safe '(lambda () (KG-CollectInstances)) nil))
  (if (and (null insts-l) KG-FAIL-STEP)
    (princ (strcat "\nВНИМАНИЕ: вхождения не прочитаны ("
                   (KG-FailText)
                   "). Интеграция продолжена без них.")))
  ;; Модель возвращается ВСЕГДА. Пустая модель -- неполный отчёт,
  ;; отсутствующая модель -- «мастер-версия не найдена» при живом блоке.
  (list
    (cons "defs" (KG-Safe '(lambda () (reverse defs-l)) nil))
    (cons "insts" insts-l)
    (cons "layouts" nil)
  )
)

;; Вложенные имена определений внутри определения defname
(defun KG-DefNestedRefs (defname / blk out nm r)
  (setq out nil)
  (setq blk (KG-BlockObj defname))
  ;; Обход содержания перехвачен целиком: отказ на одном вложенном
  ;; объекте отдаёт уже найденное, а не уносит всю функцию.
  (setq r (vl-catch-all-apply
            '(lambda ()
               (if (and blk (not (KG-IsErr blk)))
                 (vlax-for e blk
                   (if (= (vl-catch-all-apply
                          '(lambda () (vla-get-ObjectName e)))
                         "AcDbBlockReference")
                     (progn
                       (setq nm (KG-EffectiveNameOf e))
                       (if (and nm (not (KG-IsErr nm))
                                (not (member nm out)))
                         (setq out (cons nm out))
                       )
                     )
                   )
                 )
               ))))
  (if (KG-IsErr r) nil (reverse out))
)

;; Допустимые состояния видимости самого определения (через любое вхождение)
(defun KG-DefVisStates (defname / ss i e obj out r)
  (setq out nil)
  ;; Отказ чтения состояний -- не отказ всего определения: ssget,
  ;; ssname и vlax-ename->vla-object на анонимном представлении могут
  ;; не отдать ничего, и раньше это уносило вызывающую функцию.
  (setq r (vl-catch-all-apply
            '(lambda ()
               (setq ss (ssget "_X" (list '(0 . "INSERT")
                                          (cons 2 defname))))
               (if (and ss (> (sslength ss) 0))
                 (progn
                   (setq e (ssname ss 0))
                   (setq obj (vlax-ename->vla-object e))
                   (setq out (KG-GetAllowedVisibilityStates obj))
                 )
               ))))
  (if (KG-IsErr r) nil out)
)
;;
;; Возвращает список ((имя-вложенного . состояние) ...), а состояние
;; представления -- в KG-LAST-NESTED-SRC: "*U" (прочитано у анонимного
;; представления экземпляра), "ОПРЕДЕЛЕНИЕ" (представления нет,
;; прочитаны состояния по умолчанию из родительского определения) или
;; "НЕТ" (ни там, ни там вложенных вставок не найдено).
;;
;; Почему нужен второй путь: на реальном чертеже вставка может указывать
;; не на *U, а прямо на определение -- тогда её вложенные блоки стоят в
;; состояниях по умолчанию, и эти состояния лежат в определении. Без
;; запасного пути отчёт печатал «Прочитано состояний вложенных блоков:
;; 0», и отличить «состояний не было» от «не там искали» было нельзя.
(defun KG-InstanceNestedVisibility (obj / out en ed def e sub nm v eff)
  (setq out nil)
  (setq KG-LAST-NESTED-SRC "НЕТ")
  (setq en (KG-Safe '(lambda () (vlax-vla-object->ename obj)) nil))
  (if en
    (progn
      (setq def (KG_DefNameOfEnt en))
      (setq eff (KG-AsString (KG-EffectiveNameOf obj)))
      (KG-Mark (strcat "вложенные состояния: вставка указывает на \""
                       def "\", эффективное имя \"" eff "\""))
      (if (KG-IsAnonDynName def)
        (progn
          (setq out (KG_ScanDefInsertStates def))
          (if out (setq KG-LAST-NESTED-SRC "*U"))
        )
        ;; представления нет: состояния вложенных блоков этого экземпляра
        ;; равны состояниям по умолчанию в родительском определении
        (progn
          (KG-Mark (strcat "вложенные состояния: представления нет, читаю "
                           "определение \"" eff "\""))
          (if (/= eff "")
            (progn
              (setq out (KG_ScanDefInsertStates eff))
              (if out (setq KG-LAST-NESTED-SRC "ОПРЕДЕЛЕНИЕ"))
            )
          )
        )
      )
    )
  )
  (reverse out)
)

;; Состояния видимости вложенных вставок внутри определения. Обход только
;; через entget: COM-обход содержимого определений на реальном чертеже
;; дал 0xC0000005.
(defun KG_ScanDefInsertStates (defname / e ed sub nm v out cnt)
  (setq out nil cnt 0)
  ;; через KG-DefEnt: tblobjname на сиротских *U отвечает nil
  (setq e (KG-Safe '(lambda () (KG-DefEnt defname)) nil))
  (if e
    (while (and (setq e (KG-Safe '(lambda () (entnext e)) nil))
                (setq ed (KG-Call 'entget (list e) nil))
                (/= (KG-AsString (cdr (assoc 0 ed))) "ENDBLK"))
      (if (KG-StrEq (KG-AsString (cdr (assoc 0 ed))) "INSERT")
        (progn
          (setq cnt (1+ cnt))
          (KG-TraceDetail (strcat "вложенная вставка \""
                                  (KG-AsString (cdr (assoc 2 ed))) "\""))
          (setq sub (KG-Safe '(lambda () (vlax-ename->vla-object e)) nil))
          (if sub
            (progn
              (setq nm (KG-Safe '(lambda () (KG-EffectiveNameOf sub)) nil))
              (setq v (KG-Safe '(lambda () (KG-GetVisibilityState sub)) nil))
              (if (and nm v) (setq out (cons (cons nm v) out)))
            )
          )
        )
      )
    )
  )
  (KG-Mark (strcat "в определении \"" (KG-AsString defname)
                   "\" вложенных вставок: " (itoa cnt)
                   ", состояний прочитано: " (itoa (length out))))
  (reverse out)
)

;;;--- Адаптер исполнения ----------------------------------------------------

;; Handle определения блока. Нужен, чтобы отличить «AutoCAD подставил
;; старое определение» от «определение пришло из буфера»: у пришедшего
;; определения handle другой, даже если имя совпало.
(defun KG_EXDefHandle (defname / blk h)
  (setq blk (KG-BlockObj defname))
  (if (and blk (not (KG-IsErr blk)))
    (progn
      (setq h (vl-catch-all-apply '(lambda () (vla-get-Handle blk))))
      (if (KG-IsErr h) nil (KG-AsString h))
    )
    nil
  )
)

;; Снимок имён определений вместе с их handle: список пар (имя . handle)
(defun KG_EXDefHandleSnapshot ( / out)
  (setq out nil)
  (foreach nm (KG_EXAllDefNames)
    (setq out (cons (cons nm (KG_EXDefHandle nm)) out))
  )
  (reverse out)
)

;; Имена определений, которые пришли из буфера: новых имён нет в снимке,
;; а у прежних имён сменился handle. Сравнение только по именам однажды
;; уже дало ложный ответ на реальном чертеже.
(defun KG-ArrivedByHandle (snap / out nm pair)
  (setq out nil)
  (foreach nm (KG_EXAllDefNames)
    (setq pair (KG-AssocCI nm snap))
    (if (or (null pair) (not (KG-StrEq (cdr pair) (KG_EXDefHandle nm))))
      (setq out (cons nm out))
    )
  )
  (reverse out)
)

(defun KG_EXAllDefNames ( / out nm)
  (setq out nil)
  (vlax-for blk (KG-Blocks)
    (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
    (if (and (not (KG-IsErr nm)) (= (type nm) (type "")))
      (setq out (cons nm out))
    )
  )
  (reverse out)
)

(defun KG_EXDefExists (name / b)
  (setq b (KG-BlockObj name))
  (and b (not (KG-IsErr b)))
)

(defun KG_EXRenameDef (oldname newname / b r)
  (setq b (KG-BlockObj oldname))
  (if (or (not b) (KG-IsErr b))
    nil
    (progn
      (setq r (vl-catch-all-apply '(lambda () (vla-put-Name b newname))))
      (not (KG-IsErr r))
    )
  )
)

(defun KG-EXDefIsDynamic (name / b r)
  (setq b (KG-BlockObj name))
  (if (or (not b) (KG-IsErr b))
    nil
    (progn
      (setq r (vl-catch-all-apply '(lambda () (vla-get-IsDynamicBlock b))))
      (KG-ComBool r)
    )
  )
)

(defun KG_EXDeleteDef (name / b r)
  (setq b (KG-BlockObj name))
  (if (or (not b) (KG-IsErr b))
    nil
    (progn
      (setq r (vl-catch-all-apply '(lambda () (vla-Delete b))))
      (not (KG-IsErr r))
    )
  )
)

;; Сколько вхождений в чертеже ссылаются на каждое определение.
;;
;; Один проход по всем определениям, включая содержимое других
;; определений. По имени понять, свободно ли определение, нельзя: PURGE
;; отказывается удалять «X~до», пока на него ссылается хотя бы одно
;; вхождение -- в том числе внутри другого неиспользуемого определения.
;; Карта ссылок на определения: имя -> список определений, внутри
;; которых есть его вставка (с повторами, длина списка = число ссылок).
;;
;; Только DXF: tblnext -> tblobjname -> entnext -> entget. Ни одного
;; COM-вызова. Обход определений через vlax-for с чтением
;; EffectiveName/Name у вложенной вставки на реальном чертеже дал
;; 0xC0000005 -- нарушение доступа, которое vl-catch-all-apply не
;; ловит. В чертеже, где сиротские *U47/51/52/53/55/56 были удалены
;; PURGE, тот же код работал; с ними -- падал.
;;
;; Имя берётся из группы 2, то есть ФАКТИЧЕСКОЕ. У анонимной вставки
;; динамического блока там *U77, а не имя родителя: значит, родитель
;; получит ноль ссылок. Это осознанно -- удаление родителя AutoCAD всё
;; равно отклонит, пока живо его анонимное представление, а
;; многопроходный цикл сначала убирает сиротские *U.
;; Два прохода AutoCAD PURGE по блокам. Два, а не один: сиротские *U
;; держат на себе старые вложенные определения, первый проход снимает
;; *U, второй -- освободившееся. Ровно то, что вручную делает двойной
;; PURGE, и PURGE в отличие от нас живое представление не тронет.
(defun KG-CleanupPurgeBlocks ( / n ce)
  ;; Ключевые слова с подчёркиванием (_B, _N): без него AutoCAD ждёт
  ;; букву своей локализации, а у пользователя AutoCAD русский.
  ;; Пустого ввода в конце быть не должно: после _N PURGE завершается
  ;; сам, а лишний Enter уходит в командную строку и повторяет
  ;; последнюю команду -- отсюда «Неизвестная команда INTCLEANUP».
  (setq ce (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq n 0)
  (while (< n 3)
    (KG-Safe '(lambda () (command "_.-PURGE" "_B" "*" "_N")) nil)
    (setq n (1+ n))
  )
  (setvar "CMDECHO" ce)
  t
)

;; Техническая вставка из буфера в начало координат.
;; Пробуем PASTECLIP, затем PASTEBLOCK. Возвращает T/nil.
(defun KG_EXPasteAtOrigin ( / ok)
  (setq ok nil)
  (foreach cmd '("_.PASTECLIP" "_.PASTEBLOCK")
    (if (not ok)
      (progn
        (setq ok (vl-catch-all-apply
                   '(lambda ()
                      (setvar "CMDECHO" 0)
                      (command cmd (list 0.0 0.0 0.0))
                      (while (> (getvar "CMDACTIVE") 0) (command))
                      t)))
        (if (KG-IsErr ok) (setq ok nil))
      )
    )
  )
  ok
)

;; Снимок handle'ов всех вхождений
(defun KG-SnapshotDrawing ( / ss i out)
  (setq out nil)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq out (cons (cdr (assoc 5 (entget (ssname ss (setq i (1- i)))))) out))
    )
  )
  (reverse out)
)

;; Новые вхождения, появившиеся после снимка
(defun KG_EXNewInstancesSince (snapshot / ss i h out)
  (setq out nil)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq h (cdr (assoc 5 (entget (ssname ss (setq i (1- i)))))))
      (if (not (member h snapshot)) (setq out (cons h out)))
    )
  )
  (reverse out)
)

;; Модель экземпляра по handle
(defun KG_EXInstanceHandle (handle / e obj r)
  ;; Чтение свойств одного вхождения может не удаться (анонимный
  ;; динамический блок, объект внешней ссылки). Ошибка здесь не должна
  ;; обрывать команду -- раньше именно на этом месте падал INTEGRATE.
  (setq r
    (vl-catch-all-apply
      '(lambda ()
         (setq e (handent (KG-AsString handle)))
         (if e
           (progn
             (setq obj (vlax-ename->vla-object e))
             (KG-InstanceModel obj e)
           )
           nil))))
  (if (KG-IsErr r)
    (progn
      (princ (strcat "\nВНИМАНИЕ: вхождение " (KG-AsString handle)
                     " не удалось прочитать, оно пропущено."))
      nil)
    r
  )
)

;; Создать экземпляр в том же пространстве с теми же свойствами.
;; Возвращает handle нового экземпляра или nil.
(defun KG_EXCreateInstance (defname instmodel / space blk owner newobj atts pt)
  (setq blk (KG-BlockObj defname))
  (if (or (not blk) (KG-IsErr blk))
    nil
    (progn
      ;; целевое пространство: то же, что у исходного экземпляра
      (setq owner
        (cond
          ((KG-StrEq (KG-CdrCI "space" instmodel) "Model") (KG-Model))
          (t (KG-LayoutBlockForSpace (KG-CdrCI "space" instmodel)))
        )
      )
      (if (null owner) (setq owner (KG-Model)))
      ;; все числовые аргументы приводятся: (if значение …) не спасает,
      ;; когда значение есть, но не число, а nil дал бы «fixnump: nil»
      (setq pt (KG-CdrCI "pos" instmodel))
      (if (or (null pt) (< (length pt) 3)) (setq pt (list 0.0 0.0 0.0)))
      (setq newobj
        (vl-catch-all-apply
          '(lambda ()
             (vla-InsertBlock owner
               (vlax-3d-point (list (KG-AsNum (nth 0 pt) 0.0)
                                    (KG-AsNum (nth 1 pt) 0.0)
                                    (KG-AsNum (nth 2 pt) 0.0)))
               defname
               (KG-AsNum (KG-CdrCI "sx" instmodel) 1.0)
               (KG-AsNum (KG-CdrCI "sy" instmodel) 1.0)
               (KG-AsNum (KG-CdrCI "sz" instmodel) 1.0)
               (KG-AsNum (KG-CdrCI "rot" instmodel) 0.0))))
      )
      (if (KG-IsErr newobj)
        nil
        (progn
          (vl-catch-all-apply
            '(lambda () (vla-put-Layer newobj (KG-CdrCI "layer" instmodel))))
          (if (KG-CdrCI "attrs" instmodel)
            (vl-catch-all-apply
              '(lambda () (KG-SetAttributeValues newobj (KG-CdrCI "attrs" instmodel))))
          )
          (cdr (assoc 5 (entget (vlax-vla-object->ename newobj))))
        )
      )
    )
  )
)

;; Блок-владелец для имени пространства (имени листа)
(defun KG-LayoutBlockForSpace (spacename / lays lay res blk)
  (setq res nil)
  (setq lays (vl-catch-all-apply '(lambda () (vla-get-Layouts (KG-Doc)))))
  (if (not (KG-IsErr lays))
    (vlax-for lay lays
      (if (and (null res) (KG-StrEq (vla-get-Name lay) spacename))
        (setq blk (vl-catch-all-apply '(lambda () (vla-get-Block lay))))
      )
    )
  )
  (if (KG-IsErr blk) nil blk)
)

;; Смена эффективного имени вхождения (перепривязка на другое определение)
(defun KG_EXSetEffectiveName (handle defname / e obj r)
  (setq e (handent (KG-AsString handle)))
  (if (null e)
    nil
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq r (vl-catch-all-apply '(lambda () (vla-put-Name obj defname))))
      (if (KG-IsErr r)
        ;; запасной путь: пересоздать вхождение
        (KG-RecreateInstanceSamePlace e defname)
        t
      )
    )
  )
)

;; Пересоздать вхождение на месте с другим определением
(defun KG-RecreateInstanceSamePlace (e defname / obj m h)
  (setq obj (vlax-ename->vla-object e))
  (setq m (KG-InstanceModel obj e))
  (setq h (KG_EXCreateInstance defname m))
  (if h (KG_EXDeleteInstance (cdr (assoc 5 (entget e)))))
  h
)

(defun KG_EXDeleteInstance (handle / e r)
  (setq e (handent (KG-AsString handle)))
  (if (null e)
    nil
    (progn
      (setq r (vl-catch-all-apply
                '(lambda () (vla-Delete (vlax-ename->vla-object e)))))
      (not (KG-IsErr r))
    )
  )
)

;; Восстановить динамические свойства верхнего блока
(defun KG_EXRestoreDynProps (handle dynprops / e obj props p nm val n)
  (setq n 0)
  (setq e (handent (KG-AsString handle)))
  (if (and e dynprops)
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq props (vl-catch-all-apply
                    '(lambda () (vlax-invoke obj 'GetDynamicBlockProperties))))
      (if (and (not (KG-IsErr props)) props)
        (foreach dp dynprops
          (setq nm (car dp))
          (setq val (cdr dp))
          (foreach p props
            (if (and (= (vl-catch-all-apply '(lambda () (vla-get-PropertyName p))) nm)
                     (not (vl-catch-all-apply
                            '(lambda () (vlax-put-property p 'Value val)))))
              (setq n (1+ n))
            )
          )
        )
      )
    )
  )
  n
)

;; Установить состояние видимости вложенного блока внутри экземпляра.
;;
;; ПРЕЖНЯЯ ВЕРСИЯ НЕ РАБОТАЛА ВОВСЕ: она делала (vlax-for sub obj) по
;; вставке. Вставка не перечисляет содержимое своего определения, цикл
;; не выполнялся ни разу, функция возвращала nil, и «состояние
;; видимости не установлено во вложенном блоке» печаталось на каждый
;; экземпляр. Тот же приём, что и у KG-InstanceNestedVisibility:
;; содержимое обходится через entget, а не через COM.
;;
;; Возвращает T (установлено), nil (установить не удалось),
;; "НЕ НАЙДЕН" (вложенной вставки в представлении нет) или
;; "НЕТ ПРЕДСТАВЛЕНИЯ" (вставка смотрит на общее определение, писать
;; туда нельзя). Строковые ответы нужны счётчикам отчёта: по одному
;; нулю нельзя отличить «нечего было восстанавливать» от «не смогли».
(defun KG_EXSetNestedVisibility (handle nestedname value / e def sub out r)
  (setq out nil)
  (setq e (KG-Call 'handent (list handle) nil))
  (if e
    (progn
      ;; После записи динамических свойств верхнего блока вставка
      ;; указывает не на определение варианта, а на анонимное
      ;; представление *U: вложенные вставки этого экземпляра лежат там.
      (setq def (KG_DefNameOfEnt e))
      (cond
        ((and (KG-IsAnonDynName def)
              (setq sub (KG-Call 'KG-DefInsertByName (list def nestedname) nil)))
         (setq r (KG-Call 'KG-SetVisibilityState (list sub value) nil))
         (setq out (if r t nil))
        )
        ((KG-IsAnonDynName def)
         ;; представление есть, а нужной вложенной вставки в нём нет
         (setq out "НЕ НАЙДЕН")
        )
        (t
         ;; представления нет: вставка смотрит прямо на определение
         ;; варианта. Писать туда нельзя -- определение общее для всех
         ;; экземпляров, а состояние нужно только этому.
         (setq out "НЕТ ПРЕДСТАВЛЕНИЯ")
        )
      )
    )
  )
  out
)

;; Геометрия и вложенные динамические блоки переносятся полностью.
;;; Копирование определения блока С СОХРАНЕНИЕМ ДИНАМИЧЕСКИХ СВОЙСТВ.
;;;
;;; Обычное «создать пустое определение и скопировать в него объекты»
;;; даёт СТАТИЧЕСКИЙ блок: параметры динамического блока лежат в словаре
;;; расширения BLOCK_RECORD (ACAD_ENHANCEDBLOCK), API их не отдаёт, и при
;;; таком копировании они не переносятся. Обход: определение клонируется
;;; в невидимый документ ObjectDBX (в памяти), там переименовывается и
;;; копируется обратно -- так динамические свойства сохраняются.
;;; Приём Lee Mac (LM:CopyBlockDefinition).
;; Каждый шаг пишется в KG-DBX-ERR: на реальном чертеже «ObjectDBX
;; недоступен» означал на самом деле «интерфейс получен, а отказал один из
;; шести шагов копирования», и какой именно -- видно не было.
(defun KG_EXCopyDefDeep (srcname newname / dbx abc dbc def out tmp)
  (setq out nil)
  (if (KG_EXDefExists newname)
    t
    (progn
      (setq KG-DBX-ERR nil)
      (setq dbx (KG-ObjectDbx))
      (if (not dbx)
        (if (not KG-DBX-ERR)
          (KG-DBX-Fail "интерфейс ObjectDBX не получен, причина не сообщена")
        )
        (progn
          (setq abc (KG-Safe '(lambda () (KG-Blocks)) nil))
          (if (not abc)
            (KG-DBX-Fail "таблица блоков чертежа не получена")
          )
          (setq dbc (KG-DBX-Try "таблица блоков ObjectDBX (vla-get-Blocks)"
                                '(lambda () (vla-get-Blocks dbx))))
          (setq def (KG-BlockObj srcname))
          (if (or (not def) (KG-IsErr def))
            (KG-DBX-Fail (strcat "определение-источник \"" srcname
                                 "\" не найдено в чертеже"))
          )
          (if (and abc dbc def (not (KG-IsErr def)))
            (progn
              ;; КОРЕНЬ ПРОБЛЕМЫ СО СТАТИЧЕСКИМ ВАРИАНТОМ.
              ;; Document.CopyObjects и Database.CopyObjects -- методы БЕЗ
              ;; возвращаемого значения, vlax-invoke от них даёт nil. Код
              ;; проверял результат вызова и поэтому никогда не шёл дальше
              ;; первого копирования: ни ошибки, ни результата.
              ;; Проверяется не возвращаемое значение, а ПОСЛЕДСТВИЕ --
              ;; появилось ли определение в целевой таблице блоков.
              (KG-DBX-Try "копирование в документ ObjectDBX (CopyObjects)"
                '(lambda ()
                   (vlax-invoke (KG-Doc) 'CopyObjects (list def) dbc)))
              (setq tmp (KG-Safe '(lambda () (vla-Item dbc srcname)) nil))
              (if (or (not tmp) (KG-IsErr tmp))
                (KG-DBX-Fail
                  (strcat "после CopyObjects в документе ObjectDBX нет \""
                          srcname "\""))
                (progn
                  (KG-DBX-Try
                    (strcat "переименование \"" srcname "\" -> \""
                            newname "\" внутри ObjectDBX")
                    '(lambda () (vla-put-Name tmp newname)))
                  (setq def (KG-Safe '(lambda () (vla-Item dbc newname)) nil))
                  (if (or (not def) (KG-IsErr def))
                    (KG-DBX-Fail
                      (strcat "после переименования в ObjectDBX нет \""
                              newname "\""))
                    (progn
                      (KG-DBX-Try
                        "копирование из ObjectDBX в чертёж (CopyObjects)"
                        '(lambda ()
                           (vlax-invoke dbx 'CopyObjects (list def) abc)))
                      (setq out (KG-Safe '(lambda () (KG_EXDefExists newname))
                                         nil))
                      (if (not out)
                        (KG-DBX-Fail
                          (strcat "после копирования определения \""
                                  newname "\" в чертеже нет"))
                      )
                    )
                  )
                )
              )
            )
          )
          (KG-Safe '(lambda () (vlax-release-object dbx)) nil)
        )
      )
      out
    )
  )
)

;;; Запасной путь: скопировать только объекты определения. Работает без
;;; ObjectDBX, но результат СТАТИЧЕСКИЙ -- динамика мастера теряется.
(defun KG-EXCreateDefStatic (newname mastername / src dst arr i n objs r)
  (setq src (KG-BlockObj mastername))
  (if (or (not src) (KG-IsErr src))
    nil
    (if (KG_EXDefExists newname)
      t
      (progn
        (setq dst (vl-catch-all-apply
                    '(lambda () (vla-Add (KG-Blocks)
                                  (vlax-3d-point '(0.0 0.0 0.0)) newname))))
        (if (KG-IsErr dst)
          nil
          (progn
            (setq objs nil n 0)
            (vlax-for e src (setq n (1+ n)))
            (if (= n 0)
              t
              (progn
                (setq arr (vlax-make-safearray vlax-vbobject (cons 0 (1- n))))
                (setq i 0)
                (vlax-for e src
                  (vlax-safearray-put-element arr i e)
                  (setq i (1+ i))
                )
                (setq r (vl-catch-all-apply
                          '(lambda () (vla-CopyObjects (KG-Doc) arr dst nil))))
                (not (KG-IsErr r))
              )
            )
          )
        )
      )
    )
  )
)

;;;--- INTDBXTEST: сохраняется ли динамика при копировании определения ----
;;;
;;; Проба для реального AutoCAD: создаёт копию выбранного определения через
;;; ObjectDBX под именем «имя~тест», печатает, осталась ли копия
;;; динамической, и удаляет её. Чертеж не меняется.
(defun C:INTDBXTEST ( / sel e nm test b ok dyn)
  (vl-load-com)
  (princ "\n=== INTDBXTEST: проверка глубокого клонирования ===")
  (KG-SayKV "ACADVER" (KG-AsString (getvar "ACADVER")))
  (if (not (KG-Safe '(lambda () (KG-ObjectDbx)) nil))
    (progn
      (princ "\nОШИБКА: интерфейс ObjectDBX получить не удалось.")
      (princ "\nВарианты будут создаваться статическими.")
      (if KG-DBX-ERR
        (progn
          (princ "\nЧто пробовалось:")
          (foreach m (reverse KG-DBX-ERR) (princ (strcat "\n  " m)))
        )
      )
    )
    (progn
      (princ "\nObjectDBX доступен.")
      (setq sel (KG-Safe '(lambda () (entsel "\nВыберите блок для пробы: ")) nil))
      (if (or (null sel) (KG-IsErr sel))
        (princ "\nНичего не выбрано.")
        (progn
          (setq e (car sel))
          (setq nm (KG-EffectiveNameOf (vlax-ename->vla-object e)))
          (setq test (strcat nm "~тест"))
          (if (KG_EXDefExists test) (KG_EXDeleteDef test))
          (setq ok
            (KG-DBX-Try (strcat "глубокое копирование \"" nm "\" -> \""
                                test "\"")
                        '(lambda () (KG_EXCopyDefDeep nm test))))
          (if (not ok)
            (progn
              (princ "\nОШИБКА: скопировать определение не удалось.")
              (if KG-DBX-ERR
                (progn
                  (princ "\nОтказавшие шаги:")
                  (foreach m (reverse KG-DBX-ERR)
                    (princ (strcat "\n  " m)))
                )
              )
            )
            (progn
              (setq b (KG-BlockObj test))
              (setq dyn (KG-ComBool
                          (KG-Safe '(lambda () (vla-get-IsDynamicBlock b)) nil)))
              (princ (strcat "\nКопия \"" test "\" создана."))
              (princ (strcat "\nКопия динамическая: " (if dyn "ДА" "НЕТ")))
              (KG_EXDeleteDef test)
              (princ "\nПробное определение удалено, чертеж не изменён.")
            )
          )
        )
      )
    )
  )
  (princ)
)

;;;--- INTRENAMETEST --------------------------------------------------------
;;; Проба: сохраняет ли vla-put-Name динамические свойства определения.
;;;
;;; От ответа зависит запасной путь построения варианта БЕЗ ObjectDBX:
;;; освободить имя мастера, вставить из буфера ещё раз (придёт новое
;;; динамическое определение) и переименовать его в имя варианта. Если
;;; переименование динамику ломает -- этот путь неприменим.
;;;
;;; Проба обратима: имя возвращается на место.
(defun C:INTRENAMETEST ( / sel e nm b d0 tmp r d1 d2)
  (vl-load-com)
  (princ "\n=== INTRENAMETEST: переименование и динамика ===")
  (setq sel (KG-Safe '(lambda () (entsel "\nВыберите блок для пробы: ")) nil))
  (if (or (null sel) (KG-IsErr sel))
    (princ "\nНичего не выбрано.")
    (progn
      (setq e (car sel))
      (setq nm (KG-EffectiveNameOf (vlax-ename->vla-object e)))
      (setq b (KG-BlockObj nm))
      (setq d0 (KG-Safe '(lambda () (KG-EXDefIsDynamic nm)) nil))
      (KG-SayKV "Определение" (KG-AsString nm))
      (KG-SayKV "Динамическое до пробы" (if d0 "ДА" "НЕТ"))
      (if (not d0)
        (princ "\nОпределение статическое -- пробовать нечего, возьмите динамическое.")
        (progn
          (setq tmp (strcat nm "~проба"))
          (if (KG-Safe '(lambda () (KG_EXDefExists tmp)) nil)
            (princ (strcat "\nИмя \"" tmp "\" уже занято -- проба отменена."))
            (progn
              (setq r (KG-Safe '(lambda () (vla-put-Name b tmp)) nil))
              (if (KG-IsErr r)
                (princ (strcat "\nПереименовать не удалось: "
                               (KG-AsString (vl-catch-all-error-message r))))
                (progn
                  (setq d1 (KG-Safe '(lambda () (KG-EXDefIsDynamic tmp)) nil))
                  (KG-SayKV "Динамическое после переименования"
                            (if d1 "ДА" "НЕТ"))
                  (KG-Safe '(lambda () (vla-put-Name (KG-BlockObj tmp) nm)) nil)
                  (setq d2 (KG-Safe '(lambda () (KG-EXDefIsDynamic nm)) nil))
                  (KG-SayKV "Имя возвращено на место"
                            (if (KG-Safe '(lambda () (KG_EXDefExists nm)) nil)
                              "ДА" "НЕТ"))
                  (KG-SayKV "Динамическое после возврата" (if d2 "ДА" "НЕТ"))
                  (princ
                    (if (and d1 d2)
                      (strcat "\nВЫВОД: переименование динамику СОХРАНЯЕТ -- "
                              "вариант можно строить повторной вставкой из "
                              "буфера и переименованием, без ObjectDBX.")
                      (strcat "\nВЫВОД: переименование динамику ЛОМАЕТ -- "
                              "запасной путь неприменим, нужен ObjectDBX.")))
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

;;;--- INTCLEANUP -----------------------------------------------------------
;;; Удалить определения, оставшиеся от интеграции.
;;;
;;; PURGE здесь не помогает из-за ПОРЯДКА: старый вариант семейства
;;; («Комплект КП50 v1.1») сам больше никем не используется, но СОДЕРЖИТ
;;; вхождения определений «…~до». Пока он не удалён, каждое «…~до»
;;; считается использованным и не очищается. Команда удаляет в несколько
;;; проходов: за один проход освобождается то, на что ссылок не осталось.
;;;
;;; Новейшая итерация каждого семейства не удаляется даже при нуле
;;; ссылок -- это источник, из которого строятся варианты.
(defun KG-CleanupRun ( / holders counts names keep cand nm deleted progress
                        round ans hh orph left udel ufail uor inside round2
                        noxref goon uor2)
  (vl-load-com)
  (princ "\n=== INTCLEANUP: очистка определений прошлой интеграции ===")

  ;; Пересчёт нужен и до вопроса, и в каждом проходе, и после: карта
  ;; ссылок меняется по мере удаления.
  (KG-Mark "очистка: карта ссылок")
  (setq holders (KG-Safe '(lambda () (KG-EXRefHolders)) nil))
  (if (null holders)
    ;; Без карты ссылок продолжать нельзя: всё оказалось бы «без
    ;; ссылок», и очистка сняла бы живые определения.
    (princ (strcat "\nОШИБКА: карту ссылок построить не удалось "
                   "(вставок не найдено или выборка не сработала). "
                   "Очистка остановлена, чертёж не изменён."))
    (progn
      ;; маркер успешной выборки лежит в car, карта -- в cdr
      (setq holders (cdr holders))
  (setq counts (KG-CleanupCounts holders))
  (KG-Mark "очистка: список определений")
  (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
  (setq keep (KG-CleanupKeepNewest names))
  (KG-Mark "очистка: подбор кандидатов")
  (setq cand (KG-CleanupCandidates counts names keep))
  (setq orph (KG-CleanupOrphans counts names))
  (KG-Mark "очистка: кандидаты подобраны")

  (if (and (not cand) (not orph))
    (princ "\nЧисто: ни своих свободных определений, ни сиротских *U.")
    (progn
      (if cand
        (progn
          (princ (strcat "\nСвоих определений без ссылок: "
                         (itoa (length cand))))
          (KG-PrincList cand 10)
        )
      )
      (if orph
        (progn
          (princ (strcat "\nСиротских анонимных представлений (*U): "
                         (itoa (length orph))))
          ;; Что держат сироты: без этого «осталось своих без ссылок: 6»
          ;; выглядело как отказ программы, а держали их именно *U.
          (setq inside (KG-Safe '(lambda () (KG-OrphanContents orph)) nil))
          (princ (strcat "\nИз них держат вложенные определения: "
                         (itoa (length (vl-remove-if-not
                                         '(lambda (p) (cdr p))
                                         (if inside inside nil))))))
          (KG-PrincList orph 10)
          (princ "\nЖивое представление отличается от сироты только")
          (princ "\nтем, что на него есть вставка; здесь вставок нет.")
        )
      )
      (princ "\nСвои определения снимает PURGE блоков: он уберёт ВСЕ")
      (princ "\nнеиспользуемые определения в чертеже, не только наше семейство.")
      (princ "\nВсё, что хоть где-то вставлено, PURGE не тронет.")
      (princ "\nСиротские *U стирает программа, если KG-CLEANUP-ERASE-U = t.")
      (princ "\nPURGE снимает их не всегда: в прогоне на 43 сиротах он снял")
      (princ "\n16, а 27 остались -- поэтому полагаться на него нельзя.")
      (if KG-CLEANUP-ASK (princ "\nЗапрос включён (KG-CLEANUP-ASK)."))
      (setq ans "Да")
      (if KG-CLEANUP-ASK
        (progn
          (initget "Да Нет Yes No")
          (setq ans (getkword "\nОчистить? [Да/Нет] <Да>: "))
          (if (null ans) (setq ans "Да"))
        )
      )
      (if (or (KG-StrEq ans "Да") (KG-StrEq ans "Yes"))
        (progn
          (setq deleted nil udel nil ufail nil)
          (setq round 0)
          (if KG-CLEANUP-DELETE
            (progn
              (setq progress t)
              (while (and progress (< round 8))
                (setq round (1+ round))
                (KG-Mark (strcat "очистка: проход " (itoa round)))
                (setq holders
                  (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
                (setq counts (KG-CleanupCounts holders))
                (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
                (setq keep (KG-CleanupKeepNewest names))
                (setq cand (KG-CleanupCandidates counts names keep))
                (setq progress nil)
                (foreach nm cand
                  (KG-Mark (strcat "удаление определения \"" nm "\""))
                  (if (KG-Safe '(lambda () (KG_EXDeleteDef nm)) nil)
                    (progn
                      (setq deleted (cons nm deleted))
                      (setq progress t)
                    )
                  )
                )
              )
              (princ (strcat "\nСнято своими силами: "
                             (itoa (length deleted))
                             " (проходов " (itoa round) ")"))
              (if deleted
                (foreach nm (reverse deleted)
                  (princ (strcat "\n  " nm)))
              )
            )
            (princ (strcat "\nСвои определения самим не удаляются"
                           " (KG-CLEANUP-DELETE = nil), их снимет PURGE."))
          )

          ;; Сиротские *U. PURGE их не трогает, поэтому -- ENTD по записи
          ;; таблицы блоков. Проходов несколько: внутри одного *U может
          ;; лежать другое *U, и оно становится сиротой только после
          ;; стирания внешнего.
          (setq noxref (not (KG-Safe '(lambda () (KG_HasXrefDefs)) nil)))
          (if (and orph (not noxref))
            (princ (strcat "\nВ чертеже есть определения из внешней "
                           "ссылки: сиротские *U не стираются "
                           "(XREF не обрабатываем).")))
          (if (KG-CleanupEraseAllowed KG-CLEANUP-ERASE-U orph noxref)
            (progn
              (setq round2 0 uor orph goon t)
              (while (and goon uor (< round2 8))
                (setq round2 (1+ round2))
                (setq goon nil)
                (KG-Mark (strcat "очистка: стирание сиротских *U, проход "
                                 (itoa round2)))
                (foreach nm uor
                  (KG-TraceDetail (strcat "стирание \"" nm "\""))
                  (if (KG-Safe '(lambda () (KG_EXEraseAnonDef nm)) nil)
                    (progn
                      (setq udel (cons nm udel))
                      ;; следующий проход имеет смысл, только если этот
                      ;; что-то стёр: иначе те же имена упрутся в тот же
                      ;; отказ ещё семь раз
                      (setq goon t)
                    )
                    (setq ufail (cons nm ufail))
                  )
                )
                ;; пересчёт: стёртое *U могло держать другое *U
                (setq holders
                  (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
                (setq counts (KG-CleanupCounts holders))
                (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
                (setq uor (KG-CleanupOrphans counts names))
              )
              (princ (strcat "\nСтёрто сиротских *U: " (itoa (length udel))
                             ", не удалось: " (itoa (length ufail))
                             " (проходов " (itoa round2) ")"))
              (if udel
                (progn
                  (princ "\nВнутри стёртых лежали определения:")
                  (foreach p (KG-Unique
                               (apply 'append
                                      (mapcar 'cdr
                                              (KG-Safe
                                                '(lambda ()
                                                   (KG-OrphanContents
                                                     (reverse udel)))
                                                nil))))
                    (princ (strcat "\n  " (KG-AsString p)))
                  )
                )
              )
              (if ufail
                (progn
                  (princ (strcat "\nНе стёрлись: "
                                 (KG-NumStr (length ufail))))
                  (KG-PrincList (reverse ufail) 10)
                )
              )
            )
          )

          ;; Дальше AutoCAD своим PURGE, три прохода: он снимет свои
          ;; определения, а за ними -- освободившиеся после стирания *U
          ;; старые вложенные. Три -- с запасом на вложенность цепочки.
          (KG-Mark "очистка: PURGE блоков, три прохода")
          (KG-Safe '(lambda () (KG-CleanupPurgeBlocks)) nil)

          ;; Второй проход стирания. Сиротское *U не снимается, пока
          ;; живо определение, вложенное внутрь него; PURGE как раз и
          ;; снимает такие определения -- и освобождает *U, которые
          ;; первый проход стирать отказался. Без повторного прохода они
          ;; так и остались бы сиротами: в прогоне на 37 сиротах после
          ;; PURGE их осталось 25.
          (setq holders
            (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
          (setq counts (KG-CleanupCounts holders))
          (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
          (setq uor2 (KG-CleanupOrphans counts names))
          (if (and KG-CLEANUP-ERASE-U uor2)
            (progn
              (KG-Mark (strcat "очистка: стирание *U после PURGE ("
                               (itoa (length uor2)) ")"))
              (foreach nm uor2
                (KG-TraceDetail (strcat "стирание \"" nm "\""))
                (if (KG-Safe '(lambda () (KG_EXEraseAnonDef nm)) nil)
                  (setq udel (cons nm udel))
                  (setq ufail (cons nm ufail))
                )
              )
            )
          )

          (KG-Mark "очистка: контроль после PURGE")
          (setq holders
            (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
          (setq counts (KG-CleanupCounts holders))
          (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
          (setq keep (KG-CleanupKeepNewest names))
          (setq left (KG-CleanupCandidates counts names keep))
          (setq orph (KG-CleanupOrphans counts names))
          (princ (strcat "\nОсталось своих без ссылок: " (itoa (length left))
                         ", сиротских *U: " (itoa (length orph))))
          (foreach nm left
            (princ (strcat "\n  " nm " -- держит: "
                           (KG-CleanupHolderText holders nm)))
          )
          (foreach nm orph (princ (strcat "\n  " nm)))
        )
        (princ "\nОтменено, чертёж не изменён.")
      )
    )
  )
    )
  )
  (length left)
)

(defun C:INTCLEANUP ( / r)
  (vl-load-com)
  (setq r (KG-CleanupRun))
  (princ)
)

;;; Контрольный снимок таблицы блоков: сколько у каждого определения
;;; прямых вставок. Нужен, чтобы проверить очистку ДО и ПОСЛЕ на большом
;;; чертеже: «своих без ссылок» и «сиротских *U» в отчёте -- это числа, а
;;; здесь видно, какие именно определения остались и держит ли их кто-то.
;;; Вывод разбит на три группы, чтобы не понадобилась сортировка строк.
(defun C:INTCOUNT ( / holders names out n tot grp g nm c)
  (vl-load-com)
  (setq holders (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
  (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
  (princ (strcat "\n=== INTCOUNT: определений "
                 (itoa (length (if names names nil))) " ==="))
  (setq tot 0)
  (foreach grp (list (list "ОПРЕДЕЛЕНИЯ ПРОШЛОЙ ИНТЕГРАЦИИ (~до)"
                           '(lambda (x) (KG-StrContains x "~до")))
                     (list "АНОНИМНЫЕ ПРЕДСТАВЛЕНИЯ (*U)"
                           '(lambda (x) (KG-IsAnonDynName x)))
                     (list "ОСТАЛЬНЫЕ"
                           '(lambda (x)
                              (and (not (KG-StrContains x "~до"))
                                   (not (KG-IsAnonDynName x))))))
    (setq out nil)
    (foreach nm names
      (if (vl-catch-all-apply (nth 1 grp) (list nm))
        (progn
          (setq c (length (KG-CdrCI nm holders)))
          (setq tot (+ tot c))
          (setq out (cons (strcat "\n  " nm " -- вставок: " (itoa c)) out))
        )
      )
    )
    (princ (strcat "\n" (nth 0 grp) ": " (itoa (length out))))
    (foreach g (reverse out) (princ g))
  )
  (princ (strcat "\nВсего прямых вставок своих определений: " (itoa tot)))
  (princ)
)

;;; Короткая выжимка состояния чертежа: десятки строк вместо полного
;;; лога. Нужна потому, что полный лог с KG-TRACE на чертеже с десятками
;;; определений -- это сотни килобайт текста, и в окно чата он не
;;; вставляется. Печатается ровно то, по чему видно, состоялась очистка:
;;; сколько определений осталось, сколько из них без вставок, сколько
;;; сиротских *U и что именно мешает их стереть.
(defun C:INTBRIEF ( / holders names keep cand orph nodo nu inside dang
                      blocked nm n lines)
  (vl-load-com)
  (setq holders (KG-Unmark (KG-Safe '(lambda () (KG-EXRefHolders)) nil)))
  (setq names (KG-Safe '(lambda () (KG_EXAllDefNames)) nil))
  (if (null names) (setq names nil))
  (setq keep (KG-CleanupKeepNewest names))
  (setq cand (KG-CleanupCandidates
               (KG-CleanupCounts holders) names keep))
  (setq orph (KG-CleanupOrphans (KG-CleanupCounts holders) names))
  (setq nodo 0 nu 0)
  (foreach nm names
    (if (KG-StrContains nm "~до") (setq nodo (1+ nodo)))
    (if (KG-IsAnonDynName nm) (setq nu (1+ nu)))
  )
  (princ (strcat "\n=== КРАТКО (сборка " KG-VERSION ") ==="))
  (princ (strcat "\nОпределений всего: " (itoa (length names))))
  (princ (strcat "\n  с суффиксом «~до»: " (itoa nodo)))
  ;; Кто держит определения с «~до». Без этих имён «PURGE не берёт 31
  ;; определение» -- это просто число, а не диагноз.
  (setq lines (KG-Safe '(lambda () (KG-DoHolderLines holders names)) nil))
  (foreach l lines (princ (strcat "\n" l)))
  (princ (strcat "\n  анонимных (*U): " (itoa nu)))
  (princ (strcat "\nСвоих без вставок (кандидаты): " (itoa (length cand))))
  (princ (strcat "\nСиротских *U: " (itoa (length orph))))
  ;; Что держат сироты и что мешает их стереть: две эти строки и есть
  ;; диагноз, когда «Стёрто 0».
  (setq inside (KG-Safe '(lambda () (KG-OrphanContents orph)) nil))
  (if inside
    (princ (strcat "\n  держат вложенные определения: "
                   (itoa (length (KG-Unique
                                   (apply 'append
                                          (mapcar 'cdr inside))))))))
  (setq blocked nil)
  (foreach nm orph
    (if (KG-Safe '(lambda () (KG_HasMInsert nm)) nil)
      (setq blocked (cons nm blocked))
    )
  )
  (princ (strcat "\n  с многострочной вставкой внутри (не стираются): "
                 (itoa (length blocked))))
  (if blocked
    (progn
      (princ "\n  первые из них:")
      (setq n 0)
      (foreach nm (reverse blocked)
        (if (< n 10) (progn (princ (strcat "\n    " nm)) (setq n (1+ n))))
      )
    )
  )
  ;; Вставки, у которых определение пропало. В норме ноль; если не ноль --
  ;; в чертёже есть экземпляры, которые ни на что не указывают, и это надо
  ;; видеть сразу, а не выяснять по косвенным признакам.
  (setq dang (KG-Safe '(lambda () (KG-DanglingInserts)) (list 0 nil)))
  (princ (strcat "\nВставок с отсутствующим определением: "
                 (itoa (KG-AsNum (car dang) 0))))
  (if (> (KG-AsNum (car dang) 0) 0)
    (progn
      (princ "\n  первые из них:")
      (foreach d (car (cdr dang))
        (princ (strcat "\n    " (KG-AsString d))))
    )
  )
  (princ (strcat "\nСтирание *U разрешено: "
                 ;; запасное значение nil, как в KG-CleanupRun: при
                 ;; нечитаемой таблице блоков стирание разрешается
                 (if (KG-CleanupEraseAllowed KG-CLEANUP-ERASE-U orph
                                             (not (KG-Safe
                                                    '(lambda ()
                                                       (KG_HasXrefDefs))
                                                    nil)))
                   "да" "нет")))
  (princ (strcat "\nKG-TRACE = " (if KG-TRACE "t" "nil")))
  (princ)
)

;;; Короткий отчёт об ошибке: 5-8 строк, которые помещаются в сообщение
;;; даже когда полный лог не отправить. Печатает последнюю ошибку, шаг,
;;; на котором команда упала, и состояние чертежа после падения.
;;; Нужен потому, что KG-ErrorRestore обнуляет KG-LAST-STEP, и к моменту,
;;; когда пользователь садится писать письмо, улики уже нет.
(defun C:INTERR ( / )
  (princ (strcat "\n=== ОТЧЁТ ОБ ОШИБКЕ (сборка " KG-VERSION ") ==="))
  (princ (strcat "\nОшибка: "
                 (if KG-LAST-ERROR KG-LAST-ERROR
                     "(не записана -- команда не падала)")))
  (princ (strcat "\nПоследний шаг: "
                 (if (and KG-LAST-ERROR-STEP (/= KG-LAST-ERROR-STEP ""))
                   KG-LAST-ERROR-STEP
                   "(не записан)")))
  (princ (strcat "\nKG-TRACE = " (if KG-TRACE "t" "nil")))
  (princ (strcat "\nKG-AUTOCLEAN = " (if KG-AUTOCLEAN "t" "nil")))
  (princ (strcat "\nKG-CLEANUP-ERASE-U = "
                 (if KG-CLEANUP-ERASE-U "t" "nil")))
  (if KG-MASTER-FREED
    (princ (strcat "\nОсталось временно переименованное определение: \""
                   (KG-AsString (car KG-MASTER-FREED)) "\" -> \""
                   (KG-AsString (cdr KG-MASTER-FREED)) "\""))
  )
  (princ "\nСостояние чертежа после падения:")
  (KG-Safe '(lambda () (C:INTBRIEF)) nil)
  (princ)
)

;;;--- Команды ---------------------------------------------------------------

;; Обработчик ошибок: восстанавливает системные переменные
(defun KG-ErrorRestore (msg / )
  (if KG-SAVED-VARS
    (foreach v KG-SAVED-VARS
      (setvar (car v) (cdr v))
    )
  )
  (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
    (progn
      (princ (strcat "\nОШИБКА: " (KG-AsString msg)))
      ;; без AutoCAD место падения не увидеть, поэтому печатаем последний
      ;; начатый шаг -- по нему ошибка находится однозначно
      (if KG-LAST-STEP
        (princ (strcat "\nПоследний шаг: " KG-LAST-STEP))
      )
      ;; И запоминаем: KG-LAST-STEP в конце этой функции обнуляется, и
      ;; без копии место падения исчезало вместе с ним -- следующая
      ;; команда стирала единственную улику, а лог целиком не всегда
      ;; можно прислать.
      (setq KG-LAST-ERROR (KG-AsString msg))
      (setq KG-LAST-ERROR-STEP (KG-AsString KG-LAST-STEP))
      ;; Команда упала между освобождением имени мастер-версии и
      ;; завершением -- возвращаем имя, иначе в списке блоков останется
      ;; определение "ИМЯ~доN". Возвращаем только если имя свободно:
      ;; если его уже заняло пришедшее из буфера определение, трогать
      ;; его нельзя.
      (if KG-MASTER-FREED
        (progn
          (if (not (KG_EXDefExists (car KG-MASTER-FREED)))
            (progn
              (KG-Safe '(lambda () (KG-RestoreMasterName KG-MASTER-FREED)) nil)
              (princ (strcat "\nОпределение \"" (car KG-MASTER-FREED)
                             "\" возвращено из временного имени \""
                             (cdr KG-MASTER-FREED) "\"."))
            )
            (princ (strcat "\nОпределение осталось под временным именем \""
                           (cdr KG-MASTER-FREED)
                           "\": имя \"" (car KG-MASTER-FREED)
                           "\" занято пришедшим из буфера определением."))
          )
          (setq KG-MASTER-FREED nil)
        )
      )
    )
  )
  (setq KG-LAST-STEP nil)
  (princ)
)

(defun KG-GetAttributeValues (obj / atts arr out item tag val)
  (setq out nil)
  (if (and obj (vlax-property-available-p obj 'HasAttributes)
           (= (vla-get-HasAttributes obj) :vlax-true))
    (progn
      (setq atts (vl-catch-all-apply '(lambda () (vla-GetAttributes obj))))
      (if (and (not (KG-IsErr atts))
               (vlax-safearrayp (vlax-variant-value atts)))
        (progn
          (setq arr (vlax-safearray->list (vlax-variant-value atts)))
          (foreach item arr
            (setq tag (vl-catch-all-apply '(lambda () (vla-get-TagString item))))
            (setq val (vl-catch-all-apply '(lambda () (vla-get-TextString item))))
            (if (and (not (KG-IsErr tag)) (not (KG-IsErr val)))
              (setq out (cons (cons (strcase (KG-AsString tag)) (KG-AsString val)) out))
            )
          )
        )
      )
    )
  )
  (reverse out)
)

(defun KG-SetAttributeValues (obj attrs / atts arr item tag hit val)
  (if (and obj attrs (vlax-property-available-p obj 'HasAttributes)
           (= (vla-get-HasAttributes obj) :vlax-true))
    (progn
      (setq atts (vl-catch-all-apply '(lambda () (vla-GetAttributes obj))))
      (if (and (not (KG-IsErr atts))
               (vlax-safearrayp (vlax-variant-value atts)))
        (progn
          (setq arr (vlax-safearray->list (vlax-variant-value atts)))
          (foreach item arr
            (setq tag (vl-catch-all-apply '(lambda () (vla-get-TagString item))))
            (if (not (KG-IsErr tag))
              (progn
                (setq hit (assoc (strcase (KG-AsString tag)) attrs))
                (if hit
                  (vl-catch-all-apply '(lambda () (vla-put-TextString item (cdr hit))))
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun KG-SaveVars ( / )
  (setq KG-SAVED-VARS
    (list (cons "CMDECHO" (getvar "CMDECHO"))
          (cons "FILEDIA" (getvar "FILEDIA"))
          (cons "CMDDIA"  (getvar "CMDDIA"))
          (cons "NOMUTT"  (getvar "NOMUTT")))
  )
  (setvar "CMDECHO" 0)
  (setvar "FILEDIA" 0)
  (setvar "CMDDIA" 0)
)

;;;--- INTEGRATECHECK: скан без изменений (Этап 2) ---------------------------

(defun C:INTEGRATECHECK ( / *error* model base scan)
  (defun *error* (m) (KG-ErrorRestore m))
  (vl-load-com)
  (KG-SaveVars)
  (setq model (KG-Safe '(lambda () (KG_DBGetModel)) nil))
  (setq base (getstring t "\nСемейство (например ABC1.01): "))
  (if (= base "")
    (princ "\nОШИБКА: не задано семейство.")
    (progn
      (setq scan (KG-ScanFamily model base 99999999))
      (KG-SayKV "Семейство" base)
      (KG-SayKV "Найдено итераций" (KG-NumStr (length (cdr (assoc "iterations" scan)))))
      (KG-PrintScanReport scan)
      (KG-PrintSpaceReport scan)
    )
  )
  (KG-ErrorRestore nil)
  (princ)
)

;;;--- INTEGRATE: полная интеграция -----------------------------------------

(defun C:INTEGRATE ( / *error* m mastername p base newiter techhandle
                       rep snap snap0 beforedefs afterdefs fb
                       mdl pre preren hsnap nm cln)
  (defun *error* (m) (KG-ErrorRestore m))
  (vl-load-com)
  (KG-SaveVars)

  (princ (strcat "\n=== Интеграция новой итерации семейства (сборка "
                 KG-VERSION ") ==="))
  (if KG-TRACE
    (princ (strcat "\nВключена подробная печать шагов (KG-TRACE). "
                   "Отключается командой (setq KG-TRACE nil).")))
  (setq techhandle nil)
  (setq KG-DO-PASTE nil)
  (setq KG-PRERENAMES nil)
  (setq KG-PREDEFS nil)
  (setq KG-ARRIVED nil)
  (setq KG-PRE-CANDIDATES 0)
  (setq KG-FREED-N 0)
  (setq KG-PRENAME-FAILED nil)
  (setq KG-VERBOSE t)                ; шаги видны, если что-то пойдёт не так

  ;; Снимок до запроса: по нему видно, вставил ли пользователь блок сам.
  (setq snap0 (KG-SnapshotDrawing))

  ;; Рекомендуемый путь -- Enter: команда сама освободит имена и вставит
  ;; содержимое буфера. Только в этом порядке вложенные определения
  ;; приходят новыми: при вставке руками AutoCAD подставляет уже
  ;; существующие определения с теми же именами.
  (princ "\nНажмите Enter: команда освободит имена и вставит буфер сама")
  (princ "\n(рекомендуется -- только так обновляются вложенные определения).")
  (princ "\nЛибо выберите уже вставленный мастер-блок без суффикса варианта.")
  (setq m (KG-PickMasterFromUser))

  ;; Режим A: техническая вставка из буфера обмена.
  (if (null m)
    (progn
      (princ "\nВыполняется техническая вставка из буфера...")
      (KG-Mark "снимок вхождений до вставки")
      (setq snap (KG-SnapshotDrawing))
      (KG-Mark "снимок таблицы блоков до вставки")
      (setq beforedefs (KG_EXAllDefNames))
      (setq KG-PREDEFS beforedefs)

      ;; Имена освобождаются ДО вставки: PASTECLIP не переопределяет
      ;; существующее определение -- он печатает "Duplicate definition of
      ;; block ... ignored" и подставляет СТАРОЕ. Пока имя занято, ни
      ;; мастер-версия, ни вложенные определения из буфера не приходят.
      ;;
      ;; Семейство мастер-версии до вставки неизвестно, поэтому
      ;; освобождаются имена всех семейств чертежа, а после вставки те из
      ;; них, что буфер НЕ принёс, возвращаются на место. Чужие семейства
      ;; остаются нетронутыми, а нужное получает новое содержимое.
      ;; Снимок может не прочитаться: лучше INTEGRATE продолжится без
      ;; части данных и назовёт причину, чем оборвётся на середине,
      ;; оставив имена освобождёнными.
      (setq mdl (KG-Safe '(lambda () (KG_DBGetModel)) nil))
      (if (null mdl)
        (princ (strcat "\nВНИМАНИЕ: снимок чертежа не прочитан ("
                       (KG-FailText)
                       "). Интеграция продолжена без него."))
        (KG-Mark "снимок чертежа прочитан")
      )
      (if (null mdl) (setq mdl (list (cons "defs" nil) (cons "insts" nil)
                                     (cons "layouts" nil))))
      ;; Освобождаются имена ВСЕХ пользовательских определений чертежа.
      ;; Список кандидатов, собранный по старым версиям семейства, на
      ;; реальном чертеже дал ровно одно имя, и 41 вложенное определение
      ;; так и не освободилось. Состав новой мастер-версии до вставки
      ;; неизвестен, поэтому угадывать его не нужно: освобождается всё,
      ;; а чего буфер не принёс -- KG-Step_RollbackNotArrived вернёт под
      ;; прежним именем. Анонимные (*U..., *Model_Space), листовые и
      ;; внешние определения не трогаются.
      (setq pre nil)
      (foreach nm (KG-UserDefNames mdl)
        (if (and (/= (substr nm 1 1) "*") (not (member nm pre)))
          (setq pre (cons nm pre))
        )
      )
      ;; Состояния видимости вложенных блоков читаются ДО освобождения
      ;; имён: после переименования определение живёт под служебным именем,
      ;; а вернуть состояние нужно определению под прежним.
      (KG-Mark "снимок состояний вложенных блоков")
      (KG-Safe '(lambda () (KG-SaveNestedVis)) nil)
      (setq KG-PRE-CANDIDATES (length pre))
      (KG-Mark (strcat "освобождение имён до вставки: кандидатов "
                       (KG-NumStr (length pre))))
      (setq KG-PRENAME-FAILED nil)
      (setq preren (KG-Step_PreRename (reverse pre) nil nil mdl))
      (setq KG-FREED-N (length preren))
      (KG-Mark (strcat "освобождено имён: " (KG-NumStr KG-FREED-N)
                       (if KG-PRENAME-FAILED
                         (strcat ", не удалось: "
                                 (KG-NumStr (length KG-PRENAME-FAILED)))
                         "")))
      ;; Снимок handle берётся ПОСЛЕ переименования и ДО вставки:
      ;; переименование handle не меняет, а подмена определения пришедшим
      ;; из буфера -- меняет. Только так видно вложенные определения,
      ;; которые пришли под тем же самым именем: по именам они не
      ;; отличаются от старых и оказались бы в списке «не пришло».
      (setq hsnap (KG-Safe '(lambda () (KG_EXDefHandleSnapshot)) nil))

      (if (KG_EXPasteAtOrigin)
        (progn
          (setq afterdefs (KG_EXAllDefNames))
          (setq KG-ARRIVED (KG-Step_DetectArrived beforedefs afterdefs))
          (if hsnap
            (foreach nm (KG-Safe '(lambda () (KG-ArrivedByHandle hsnap)) nil)
              (if (not (KG-StrInterCI (list nm) KG-ARRIVED))
                (setq KG-ARRIVED (cons nm KG-ARRIVED))
              )
            )
          )
          ;; Что буфер не принёс -- то не наше: имя возвращается на место.
          (setq preren (KG-Step_RollbackNotArrived preren KG-ARRIVED))
          (setq KG-PRERENAMES preren)
          (KG-Mark (strcat "занято пришедшими: " (KG-NumStr (length preren))
                           ", вернулось на место: "
                           (KG-NumStr (- KG-FREED-N (length preren)))))
          (KG-ReportArrived beforedefs afterdefs)
          (KG-Mark "поиск мастер-версии среди пришедших определений")
          (setq mastername (KG-DetectMasterName KG-ARRIVED))
          (setq KG-MASTER-STATE
            (KG-MasterState mastername beforedefs KG-ARRIVED))
          (if mastername
            (progn
              (KG-Mark (strcat "мастер-версия: " (KG-AsString mastername)
                                ", поиск технического экземпляра"))
              (setq techhandle
                (KG-PickMasterHandle (KG_EXNewInstancesSince snap)))
              (setq p (KG-ParseBlockName mastername))
              (setq base (nth 0 p))
              (setq newiter (nth 1 p))
            )
            ;; из буфера не пришло ни одного определения. Обычная причина:
            ;; пользователь уже вставил блок вручную ДО запуска команды.
            (progn
              ;; FB принимает три аргумента: снимок вхождений, снимок
              ;; таблицы блоков и модель.
              (setq fb (KG-FallbackMasterFromDrawing
                         snap0 beforedefs (KG-Safe '(lambda () (KG_DBGetModel)) nil)))
              (if (car fb)
                (progn
                  (setq mastername (nth 1 fb))
                  (setq techhandle (nth 2 fb))
                  (setq base (nth 3 fb))
                  (setq newiter (nth 4 fb))
                )
                (progn
                  (KG-Step_RollbackRename preren)
                  (setq preren nil)
                  (setq KG-PRERENAMES nil)
                  (princ (nth 1 fb))
                )
              )
            )
          )
        )
        (progn
          (KG-Step_RollbackRename preren)
          (setq preren nil)
          (setq KG-PRERENAMES nil)
          (princ "\nОШИБКА: вставка из буфера обмена не выполнена.")
        )
      )
    )
    (progn
      (setq mastername (KG-CdrCI "eff" m))
      (setq techhandle (KG-CdrCI "handle" m))
      (setq p (KG-ParseBlockName mastername))
      (setq base (nth 0 p))
      (setq newiter (nth 1 p))
    )
  )

  (if (null base)
    (princ "\nОШИБКА: не удалось определить мастер-версию.")
    (progn
      (KG-SayKV "Мастер-версия" (KG-MakeName base newiter ""))
      (KG-SayKV "Семейство" base)
      (KG-SayKV "Новая итерация" (KG-IterToStr newiter))
      (KG-Say "Область обработки: весь файл")
      (KG-Mark "интеграция выполнена, печать отчёта")
      (setq KG-PRERENAMES preren)
      (setq rep (KG-Integrate_Model base newiter techhandle KG-DO-PASTE))
      (KG-Mark "печать итогового отчёта")
      (KG-PrintIntegrationReport rep)
      (KG-Mark "регенерация чертежа")
      (command "_.REGEN")
      ;; Очистка -- часть интеграции, а не отдельная обязанность
      ;; пользователя: после замены экземпляров старые определения и
      ;; их анонимные представления всё равно висят мёртвым грузом.
      (if KG-AUTOCLEAN
        (progn
          (KG-Mark "очистка определений прошлой интеграции")
          (setq cln (KG-CleanupRun))
          ;; Итог очистки отдельной строкой: «не удалось: 37» в середине
          ;; длинного вывода читалось как провал команды, хотя на
          ;; чертёж это не влияет -- остаются лишь неиспользуемые
          ;; определения, которые не видно и которые не мешают.
          (princ (strcat "\n=== ИТОГ ОЧИСТКИ: "
                         (if (> (KG-AsNum cln 0) 0)
                           (strcat "осталось неиспользуемых определений: "
                                   (itoa (KG-AsNum cln 0))
                                   ". На чертёж не влияют, снимаются "
                                   "повторным INTCLEANUP или PURGE.")
                           "чисто, лишнего не осталось")
                         " ==="))
        )
        (princ (strcat "\nОчистка отключена (KG-AUTOCLEAN = nil)."
                       " Запустите INTCLEANUP."))
      )
    )
  )
  (KG-ErrorRestore nil)
  (princ)
)

;; Выбор мастер-блока пользователем (Режим B).
;; Возврат nil (Enter без выбора) означает "использовать буфер обмена".
;;; Выбор мастер-блока пользователем.
;;; Любая ошибка чтения свойств перехватывается: команда не должна рваться,
;;; у пользователя всегда остаётся путь через Enter (вставка из буфера).
(defun KG-PickMasterFromUser ( / sel e r)
  (setq sel (vl-catch-all-apply '(lambda () (entsel "\nМастер-блок: "))))
  (setq e nil)
  (if (and sel (not (KG-IsErr sel))
           (= (type sel) (type (list nil))))
    (setq e (car sel))
  )
  ;; e должен быть именно именем объекта; всё остальное (nil при пустом
  ;; щелчке, variant при отмене) означает "пользователь ничего не выбрал"
  (if (or (null e) (KG-IsErr e) (/= (type e) 'ENAME))
    nil
    (progn
      (setq r (vl-catch-all-apply '(lambda () (KG-MasterFromEntity e))))
      (if (KG-IsErr r)
        (progn
          (KG-Say "")
          (KG-Say "ВНИМАНИЕ: не удалось прочитать свойства выбранного блока:")
          (KG-Say (strcat "  " (vl-princ-to-string r)))
          (KG-Say "Нажмите Enter, чтобы команда сама вставила блок из буфера.")
          nil
        )
        r
      )
    )
  )
)

;; Разобрать выбранное вхождение и собрать его модель
(defun KG-MasterFromEntity (e / obj eff p)
  (setq obj (vlax-ename->vla-object e))
  (setq eff (KG-EffectiveNameOf obj))
  (setq p (KG-ParseBlockName eff))
  (if (and p (= (nth 2 p) ""))
    (KG-InstanceModel obj e)
    (progn
      (KG-Say (KG-MasterRejectMsg eff))
      (KG-Say "Укажите вхождение мастер-версии заново или нажмите Enter")
      (KG-Say "для технической вставки из буфера обмена.")
      nil
    )
  )
)

;; Диагностический шаг: выполнить операцию и напечатать результат.
;; При ошибке печатает, на каком объекте и каком шаге она случилась.
(defun KG-DiagStep (who what fn / r)
  (setq r (vl-catch-all-apply fn))
  (if (KG-IsErr r)
    (KG-Say (strcat "  ОШИБКА [" what "] " who " : " (vl-princ-to-string r)))
    (KG-Say (strcat "  " who "  " what ": " (KG-AsString r)))
  )
  r
)

;;;--- INTDIAG: диагностика чтения чертежа ------------------------------------
;;; Проходит по всем определениям и вхождениям и печатает, на каком объекте
;;; чтение ломается. Ничего не меняет. Нужна, когда INTEGRATE падает с
;;; ошибкой типа: по выводу видно конкретное имя блока.
(defun C:INTDIAG ( / *error* blk nm r ss i e h ok bad firstbad obj m)
  (defun *error* (m) (KG-ErrorRestore m))
  (vl-load-com)
  (KG-SaveVars)
  (setq KG-VERBOSE t)

  (KG-Say (strcat "=== INTDIAG, сборка " KG-VERSION " ==="))

  ;; 1. определения
  (KG-Say "Определения:")
  (setq ok 0 bad 0 firstbad nil)
  (vlax-for blk (KG-Blocks)
    (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
    (if (KG-IsErr nm)
      (setq bad (1+ bad))
      (progn
        (KG-Mark (strcat "определение \"" (KG-AsString nm) "\""))
        (setq r (vl-catch-all-apply
                  '(lambda ()
                     (list (KG-DefNestedRefs nm) (KG-DefVisStates nm)))))
        (if (KG-IsErr r)
          (progn
            (setq bad (1+ bad))
            (if (not firstbad) (setq firstbad nm))
            (KG-Say (strcat "  ОШИБКА  " (KG-AsString nm) " : "
                            (vl-princ-to-string r)))
          )
          (setq ok (1+ ok))
        )
      )
    )
  )
  (KG-SayKV "  прочитано" (itoa ok))
  (KG-SayKV "  с ошибками" (itoa bad))

  ;; 2. вхождения
  (KG-Say "Вхождения:")
  (setq KG-SPACE-MAP (KG-BuildSpaceMap))
  (KG-SayKV "  объектов в карте пространств" (KG-NumStr (length KG-SPACE-MAP)))
  (setq ok 0 bad 0)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      (setq h (cdr (assoc 5 (entget e))))
      (KG-Mark (strcat "вхождение " (KG-AsString h)))
      ;; каждый шаг чтения отдельно: по выводу видно, что именно ломается
      (setq obj (vl-catch-all-apply '(lambda () (vlax-ename->vla-object e))))
      (if (KG-IsErr obj)
        (progn
          (setq bad (1+ bad))
          (KG-Say (strcat "  ОШИБКА [vla-object] handle " (KG-AsString h)
                          " : " (vl-princ-to-string obj)))
        )
        (progn
          (KG-DiagStep (strcat "handle " (KG-AsString h)) "эффективное имя"
            '(lambda () (KG-EffectiveNameOf obj)))
          (KG-DiagStep (strcat "handle " (KG-AsString h)) "пространство"
            '(lambda () (KG-SpaceOf e)))
          (KG-DiagStep (strcat "handle " (KG-AsString h)) "полная модель"
            '(lambda () (if (KG-InstanceModel obj e) t nil)))
          (if (KG-IsErr (vl-catch-all-apply '(lambda () (KG-InstanceModel obj e))))
            (setq bad (1+ bad))
            (setq ok (1+ ok))
          )
        )
      )
    )
  )
  (KG-SayKV "  прочитано" (itoa ok))
  (KG-SayKV "  с ошибками" (itoa bad))

  ;; 3. разбор имён семейства
  (KG-Say "Имена, которые разбираются как БАЗА vВЕРСИЯ или БАЗА(N):")
  (vlax-for blk (KG-Blocks)
    (setq nm (vl-catch-all-apply '(lambda () (vla-get-Name blk))))
    (if (not (KG-IsErr nm))
      (progn
        (setq r (KG-ParseBlockName nm))
        (if r
          (KG-Say (strcat "  " (KG-AsString nm) "  ->  база \""
                          (KG-AsString (nth 0 r)) "\" итерация \""
                          (KG-AsString (nth 1 r)) "\" вариант \""
                          (KG-AsString (nth 2 r)) "\""))
        )
      )
    )
  )
  (KG-Say "Готово. Чертеж не изменён.")
  (KG-ErrorRestore nil)
  (princ)
)


(defun C:INTDUMP ( / e obj dp vis allowed nm)
  (vl-load-com)
  (setq e (car (entsel "\nВыберите блок для диагностики: ")))
  (if e
    (progn
      (setq obj (vlax-ename->vla-object e))
      (KG-Say "=== INTDUMP ===")
      (KG-SayKV "Name" (cdr (assoc 2 (entget e))))
      (KG-SayKV "EffectiveName" (KG-EffectiveNameOf obj))
      (KG-SayKV "Handle" (cdr (assoc 5 (entget e))))
      (KG-SayKV "Layout" (KG-SpaceOf e))
      (KG-SayKV "Layer" (vla-get-Layer obj))
      (KG-SayKV "InsertionPoint" (vl-princ-to-string (vla-get-InsertionPoint obj)))
      (KG-SayKV "Scale"
        (strcat (rtos (vla-get-XScaleFactor obj) 2 4) " / "
                (rtos (vla-get-YScaleFactor obj) 2 4) " / "
                (rtos (vla-get-ZScaleFactor obj) 2 4)))
      (KG-SayKV "Rotation" (rtos (vla-get-Rotation obj) 2 6))
      (KG-SayKV "IsDynamicBlock"
        (if (KG-ComBool
              (vl-catch-all-apply '(lambda () (vla-get-IsDynamicBlock obj))))
          "T" "nil"))
      (setq allowed (KG-GetAllowedVisibilityStates obj))
      (KG-SayKV "AllowedValues" (if allowed (vl-princ-to-string allowed) "нет"))
      (KG-SayKV "Visibility" (KG-GetVisibilityState obj))
      (KG-Say "DynamicProperties:")
      (setq dp (KG-GetDynamicProperties obj))
      (if dp
        (foreach p dp
          (princ (strcat "\n  " (KG-AsString (car p)) " = "
                         (vl-princ-to-string (cdr p)))))
        (princ "\n  нет")
      )
      (KG-Say "Nested visibility:")
      (setq nm (KG-InstanceNestedVisibility obj))
      (if nm
        (foreach p nm
          (princ (strcat "\n  " (KG-AsString (car p)) " = "
                         (if (cdr p) (KG-AsString (cdr p)) "-"))))
        (princ "\n  нет")
      )
    )
  )
  (princ)
)

;;;--- INTDUMPDEF: диагностика определения (Этап 0.5) ----------------------

(defun C:INTDUMPDEF ( / nm)
  (vl-load-com)
  (setq nm (getstring t "\nИмя определения блока: "))
  (if (/= nm "")
    (progn
      (KG-Say "=== INTDUMPDEF ===")
      (KG-SayKV "Определение" nm)
      (KG-SayKV "Существует" (if (KG_EXDefExists nm) "да" "нет"))
      (KG-SayKV "Вложенные ссылки" (vl-princ-to-string (KG-DefNestedRefs nm)))
      (KG-SayKV "Рекурсивно вложенные"
                (vl-princ-to-string
                  (KG-GetNestedBlocks (KG_DBGetModel) nm)))
      (KG-SayKV "Состояния видимости" (vl-princ-to-string (KG-DefVisStates nm)))
      (KG-SayKV "Состояния вложенных" (vl-princ-to-string (KG-DefNestedVisibility nm)))
    )
  )
  (princ)
)

;;;--- INTPASTETEST: диагностика вставки из буфера (Этап 0.6) --------------
;;; Отвечает на ключевой вопрос этапа: что реально приходит из буфера и
;;; переименовываются ли конфликтующие определения.

(defun C:INTPASTETEST ( / snap before after newdefs gone i nm newinst h)
  (vl-load-com)
  (KG-SaveVars)
  (KG-Say "=== INTPASTETEST ===")
  (princ "\nСкопируйте мастер-блок в исходном файле, затем нажмите Enter.")
  (getstring "\nEnter для продолжения: ")
  (setq snap (KG-SnapshotDrawing))
  (setq before (KG_EXAllDefNames))
  (if (KG_EXPasteAtOrigin)
    (progn
      (setq after (KG_EXAllDefNames))
      (setq newdefs (KG-StrDiffCI after before))
      (setq gone (KG-StrDiffCI before after))
      (KG-Say "Новые определения после вставки:")
      (if newdefs
        (foreach nm newdefs (KG-Say (strcat "  + " (KG-AsString nm))))
        (KG-Say "  (нет — вероятно, сработало 'Duplicate definition ignored')")
      )
      (KG-Say "Переименованные/исчезнувшие определения:")
      (if gone
        (foreach nm gone (KG-Say (strcat "  - " (KG-AsString nm))))
        (KG-Say "  (нет)")
      )
      ;; Анонимные определения (*U118, *D3 ...) служебными НЕ считаются:
      ;; их создаёт сам AutoCAD, в том числе при вставке динамического
      ;; блока. Иначе список состоял бы только из них.
      (KG-Say "Определения со служебными суффиксами в имени:")
      (setq i nil)
      (foreach nm after
        (if (and (not (KG-IsAnonymousName nm))
                 (KG-IsServiceName nm)
                 (not (member nm before)))
          (progn (KG-Say (strcat "  ! " (KG-AsString nm))) (setq i t))
        )
      )
      (if (not i) (KG-Say "  (нет)"))
      (KG-Say "Новые экземпляры:")
      (setq newinst (KG_EXNewInstancesSince snap))
      (foreach h newinst
        (KG-Say (strcat "  handle " (KG-AsString h) " -> "
                        (KG-CdrCI "eff" (KG_EXInstanceHandle h)))))
      ;; Команда диагностическая, поэтому свою вставку убирает сама.
      ;; Копия мастер-версии, оставленная в начале координат, потом
      ;; попадает в INTEGRATE как лишний экземпляр новой итерации и даёт
      ;; ложное расхождение счётчика и ложную «потерю».
      (foreach h newinst (KG_EXDeleteInstance h))
      (if newinst
        (princ (strcat "\nТехническая вставка удалена ("
                       (KG-NumStr (length newinst))
                       " шт.), чертёж в том же составе, что до команды."))
        (princ "\nНовых экземпляров нет.")
      )
    )
    (princ "\nОШИБКА: вставка из буфера не выполнена.")
  )
  (KG-ErrorRestore nil)
  (princ)
)

;;;--- INTTESTBED: построение тестового стенда (Этап 0) --------------------
;;; Создаёт СТАТИЧЕСКИЕ блоки с правильными именами. Динамические параметры
;;; добавляются вручную в редакторе блоков (LISP не может их создавать).

(defun C:INTTESTBED ( / names nm)
  (setq names '("ABC1.01(1)Стойка" "ABC1.01(2)Ригель"
                "ABC1.01(3)Крышка" "ABC1.01(4)"))
  (foreach nm names
    (if (not (tblsearch "BLOCK" nm))
      (progn
        (entmake (list (cons 0 "BLOCK") (cons 2 nm) (cons 70 2)
                       (cons 10 (list 0.0 0.0 0.0))))
        (entmake (list (cons 0 "CIRCLE") (cons 8 "0")
                       (cons 10 (list 0.0 0.0 0.0)) (cons 40 100.0)))
        (entmake (list (cons 0 "TEXT") (cons 8 "0")
                       (cons 10 (list 120.0 0.0 0.0)) (cons 40 50.0)
                       (cons 1 nm)))
        (entmake (list (cons 0 "ENDBLK") (cons 8 "0")))
      )
    )
  )
  ;; экземпляры: часть в модели
  (entmake (list (cons 0 "INSERT") (cons 2 "ABC1.01(1)Стойка")
                 (cons 10 (list 0.0 0.0 0.0)) (cons 41 1.0)
                 (cons 42 1.0) (cons 43 1.0) (cons 50 0.0)))
  (entmake (list (cons 0 "INSERT") (cons 2 "ABC1.01(2)Ригель")
                 (cons 10 (list 1000.0 0.0 0.0)) (cons 41 1.0)
                 (cons 42 1.0) (cons 43 1.0) (cons 50 0.0)))
  (entmake (list (cons 0 "INSERT") (cons 2 "ABC1.01(3)Крышка")
                 (cons 10 (list 2000.0 0.0 0.0)) (cons 41 1.0)
                 (cons 42 1.0) (cons 43 1.0) (cons 50 0.0)))
  (entmake (list (cons 0 "INSERT") (cons 2 "ABC1.01(4)")
                 (cons 10 (list 3000.0 0.0 0.0)) (cons 41 1.0)
                 (cons 42 1.0) (cons 43 1.0) (cons 50 0.0)))
  (KG-Say "Тестовый стенд создан (статические блоки).")
  (KG-Say "Для полной проверки добавьте вручную:")
  (KG-Say "  1. параметр видимости в каждом блоке (BEDIT -> Visibility);")
  (KG-Say "  2. экземпляры на листы;")
  (KG-Say "  3. мастер-версию ABC1.01(5) с вложенными блоками.")
  (command "_.REGEN")
  (princ)
)


;;;--- Псевдонимы команд (RDB, REPDBLOCK, русские команды) -------------------
(defun C:INTEGRATEPICK ( / *error* rep h)
  (defun *error* (m) (KG-ErrorRestore m))
  (vl-load-com)
  (KG-SaveVars)
  (setq h (KG-PickMasterFromUser))
  (if h
    (progn
      (setq rep (KG-Integrate_Model nil nil h nil))
      (KG-PrintIntegrationReport rep)
      (command "_.REGEN")
    )
    (princ "\nВыбор отменён.")
  )
  (KG-ErrorRestore nil)
  (princ)
)

(defun C:RDB () (C:INTEGRATE))
(defun C:REPDBLOCK () (C:INTEGRATE))
(defun C:ПОДМЕНАБЛОКА () (C:INTEGRATE))
(defun C:ПДБ () (C:INTEGRATE))

(defun C:RDBPICK () (C:INTEGRATEPICK))
(defun C:REPDBLOCKPICK () (C:INTEGRATEPICK))
(defun C:ПДБВЫБОР () (C:INTEGRATEPICK))

(defun C:RDBCHECK () (C:INTEGRATECHECK))
(defun C:REPDBLOCKCHECK () (C:INTEGRATECHECK))
(defun C:ПДБЧЕК () (C:INTEGRATECHECK))

(defun C:RDBDIAG () (C:INTDIAG))
(defun C:ПДБДИАГ () (C:INTDIAG))

(defun C:RDBDUMP () (C:INTDUMP))
(defun C:ПДБДАМП () (C:INTDUMP))

(defun C:RDBDUMPDEF () (C:INTDUMPDEF))
(defun C:ПДБДАМПОПР () (C:INTDUMPDEF))

(defun C:RDBPASTETEST () (C:INTPASTETEST))
(defun C:ПДБТЕСТВСТАВКИ () (C:INTPASTETEST))

(defun C:RDBTESTBED () (C:INTTESTBED))
(defun C:ПДБСТЕНД () (C:INTTESTBED))

(defun C:RDBDBXTEST () (C:INTDBXTEST))
(defun C:ПДБТЕСТDBX () (C:INTDBXTEST))

(defun C:RDBRENAMETEST () (C:INTRENAMETEST))
(defun C:ПДБТЕСТПЕРЕИМ () (C:INTRENAMETEST))

(defun C:RDBCLEANUP () (C:INTCLEANUP))
(defun C:ПДБОЧИСТКА () (C:INTCLEANUP))

(defun C:RDBCOUNT () (C:INTCOUNT))
(defun C:ПДБСЧЁТ () (C:INTCOUNT))

(defun C:RDBBRIEF () (C:INTBRIEF))
(defun C:ПДБКРАТКО () (C:INTBRIEF))

(defun C:RDBERR () (C:INTERR))
(defun C:ПДБОШИБКА () (C:INTERR))

) ; progn
) ; if not KG-TESTING

;; Список команд в баннере обязан совпадать с реально определёнными:
;; в сборке 19 здесь не было INTDBXTEST, и пользователь не мог понять,
;; доступна ли команда диагностики.
(princ
  (strcat
    "\nRepDblock.lsp, сборка " KG-VERSION
    ". Команды: RDB (RepDblock, ПОДМЕНАБЛОКА, ПДБ, INTEGRATE), "
    "RDBPICK (REPDBLOCKPICK, ПДБВЫБОР), "
    "RDBCHECK (INTEGRATECHECK, REPDBLOCKCHECK, ПДБЧЕК), "
    "RDBDIAG (INTDIAG, ПДБДИАГ), "
    "RDBDUMP (INTDUMP, ПДБДАМП), "
    "RDBDUMPDEF (INTDUMPDEF, ПДБДАМПОПР), "
    "RDBPASTETEST (INTPASTETEST, ПДБТЕСТВСТАВКИ), "
    "RDBTESTBED (INTTESTBED, ПДБСТЕНД), "
    "RDBDBXTEST (INTDBXTEST, ПДБТЕСТDBX), "
    "RDBRENAMETEST (INTRENAMETEST, ПДБТЕСТПЕРЕИМ), "
    "RDBCLEANUP (INTCLEANUP, ПДБОЧИСТКА), "
    "RDBCOUNT (INTCOUNT, ПДБСЧЁТ), "
    "RDBBRIEF (INTBRIEF, ПДБКРАТКО), "
    "RDBERR (INTERR, ПДБОШИБКА)."
    " Подробно: (setq KG-TRACE-DETAIL t)."))
(princ)
