;;  income tax form advisor - kiosk based app

;;;;;;;;;;;;;;module MAIN;;;;;;;;;;;;;;;;;;;;

;; data type
(deftemplate user
    (slot income (default 0))
    (slot dependents (default 0))
)

(deftemplate question
    (slot text)
    (slot type)
    (slot ident)
)

(deftemplate answer
    (slot ident)
    (slot text)
)

(deftemplate recommendataion
    (slot form)
    (slot explanation)
)


;; facts
(deffacts question-data
    "these are possible question to ask"
    (question (ident income) (type number) (text "Annual Income?"))
    (question (ident interest) (type yes-no) (text "Earn Interest > $400?"))
    (question (ident dependents) (type number) (text "# of Dependent living with you?"))
    (question (ident childcare) (type yes-no) (text "Childcare Expense?"))
    (question (ident moving) (type yes-no) (text "Moving for Job-relatedness?"))
    (question (ident employee) (type yes-no) (text "Unreimbursed employee expenses?"))
    (question (ident reimburse) (type yes-no) (text "Have reimbursed employee expenses?"))
    (question (ident casualty) (type yes-no) (text "Losses from theft or accident?"))
    (question (ident on-time) (type yes-no) (text "will you file on time?"))
    (question (ident charity) (type yes-no) (text "property charity > $500?"))
    (question (ident home-office) (type yes-no) (text "Work in home office?"))
)

;; newline gloabal
;; (defglobal ?*crlf*= "\n")

;;;;;;;;;;;;;;module startup;;;;;;;;;;;;;;;;;;;;
(defmodule startup)


(defrule print-banner
    =>
        (printout t "Type name and press Enter> ")
        (bind ?name (read))
        (printout t crlf "*******************************" crlf)
        (printout t " Hello, " ?name "." crlf)
        (printout " Welcome to tax form advisor" crlf)
        (printout " Please answer the questions and" crlf)
        (printout " I will tell you what tax forms" crlf)
        (printout " you may need to file." crlf)
        (printout t crlf "*******************************" crlf crlf)
)

;;;;;;;;;;;;;;module interview;;;;;;;;;;;;;;;;;;;;
(defmodule interview)

(defrule request-income
    =>
        (assert (ask income))
)

(defrule request-num-dependents
    =>
        (assert (ask dependents))
)

(defrule assert-user-fact
    (answer (ident income) (text ?i))
    (answer (ident dependents) (text ?d))
    =>
        (assert (user (income ?i) (dependents ?d)))
)

;; total income < 50000 && no dependent -> ask interest
(defrule request-interest-income
    (answer (ident income) (text ?i&:(< ?i 50000)))
    (answer (ident dependents) (text ?d&: (eq ?d 0)))
    =>
        (assert (MAIN::ask interest))
)

;; has dependents -> child care expense?
(defrule request-childcare-expenses
    (answer (ident dependents) (text ?t&: (> ?t 0)))
    =>
    (assert (ask childcare))
)

(defrule request-employee-expenses
    =>
    (assert (ask employee))
)

;; unreimbursed employee expense -> ask reimbursed
(defrule request-reimbursed-expenses
    (answer (ident employee)(text ?t&:(eq ?t yes)))
    => 
        (assert (ask reimbursed))
)

(defrule request-moving
    =>
        (assert (ask moving))
)

(defrule request-casualty
    =>
        (assert (ask casulaty))
)

(defrule request-on-time
    =>
        (assert (ask on-time))
)

(defrule request-charity
    =>
        (assert (ask charity))
)

(defrule request-home-office
    =>
        (assert (ask home-office))
)

;;;;;;;;;;;;;;module recommend - this is the main logic;;;;;;;;;;;;;;;;;;;;
(defmodule recommend)

;;if same forms with different explanation - combine both into one explanation by concat
(defrule combine-recommendation
    ;; binding the fact with variable left side
    ?r1 <- (recommendation (form ?f) (explanation ?e1))
    ?r2 <- (recommendation (form ?f) (explanation ?e2&:(neq ?e1 ?e2)))
    =>
        (retract ?r2)
        (modify ?r1 (explanation (str-cat ?e1 ?*crlf* ?e2)))
)

(defrule form-1040EZ
    (user (income ?i&:(< ?i 50000)) (dependents ?d&: (eq ?d 0)))
    (answer (ident interest) (text no))
    => 
        (assert (recommendation
                    (form 1040EZ)
                    (explanation "Income < threshold, no dependents"))
        )
)

(defrule form-1040A-excess-interest
    (user (income ?i&:(< ?i 50000)))
    (answer (ident interest) (text yes))
    => 
        (assert (recommendation
                    (form 1040A)
                    (explanation "Excess interest income"))
        )
)

(defrule form-1040A
    (user (income ?i&:(< ?i 50000)) (dependents ?d&: (> ?d 0)))
    => 
        (assert (recommendation
                    (form 1040A)
                    (explanation "Income < threshold, with dependents"))
        )
)

(defrule form-1040-income-above-threshold
    (user (income ?i&:(>= ?i 50000)))
    => 
        (assert (recommendation
                    (form 1040)
                    (explanation "Income above threshold"))
        )
)

(defrule form-2441
    (answer (ident childcare) (text yes))
    => 
        (assert (recommendation
                    (form 2441)
                    (explanation "Child care expenses"))
        )
)

(defrule form-2016EZ
    (answer (ident employee) (text yes))
    (answer (ident reimbursed) (text no))
    => 
        (bind ?expl "Unreimbursed employee expenses")
        (assert (recommendation (form 2016EZ)(explanation ?expl))
                (recommendation (form 1040)(explanation ?expl)))
)

(defrule form-2016
    (answer (ident employee) (text yes))
    (answer (ident reimbursed) (text yes))
    =>
    (bind ?expl "Reimbursed employee expenses")
    (assert (recommendation (form 2016) (explanation ?expl))
            (recommendation (form 1040) (explanation ?expl)))
)

(defrule form-3903
    (answer (ident moving) (text yes))
    =>
    (bind ?expl "Moving expenses")
    (assert (recommendation (form 3903) (explanation ?expl))
            (recommendation (form 1040) (explanation ?expl))
    )
)

(defrule form-4684
    (answer (ident casualty) (text yes))
    =>
    (bind ?expl "Losses due to casualty or theft")
    (assert 
        (recommendation (form 4684) (explanation ?expl))
        (recommendation (form 1040) (explanation ?expl))
    )
)

(defrule form-4868
    (answer (ident on-time) (text no))
    => 
    (assert (recommendation (form 4868) (explanation "Filing extension")))
)

(defrule form-8283
    (answer (ident charity) (text yes))
    =>
    (bind ?expl "Excell charitable contributions")
    (assert (recommendation (form 8283) (explanation ?expl))
            (recommendation (form 1040) (explanation ?expl))
    )
)

(defrule form-8829
    (answer (ident home-office) (text yes))
    =>
        (bind ?expl "Home office expenses")
        (assert (recommendation (form 8829) (explanation ?expl))
                (recommendation (form 1040) (explanation ?expl))
        )
)

;; module ask

(defmodule ask)

(deffunction is-of-type (?answer ?type)
    "check that hte answer has the right form"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
        else (if(eq ?type number) then
                (return (numberp ?answer))
            )
        else (return (> (str-length ?answer) 0) )   
    )
)

(deffunction ask-user (?question ?type)
    "ask a question and return the answer"
    (bind ?answer "")
    (while not (is-of-type ?answer ?type) do
        (printout t ?question " ")
        (if (eq ?type yes-no) then
            (printout t "(yes or no) ")
        )
        (bind ?answer (read))
    )
    (return ?answer)
)

(defrule ask-question-by-id
    "Given the identifier of a question, ask it and assert the answer"
    (declare (auto-focus TRUE))
    (question (ident ?id) (text ?text) (type ?type))
    (not (answer (ident ?id)))
    ?ask <- (ask ?id)
    => 
    (bind ?ans (ask-user ?text ?type))
    (assert (answer (ident ?id) (text ?ans)))
    (retract ?ask)
)

;;;;;;;;;;;;;;module report;;;;;;;;;;;;;;;;;;;;

(defmodule report)

(defrule sort-and-print
    ?r1 <- (recommendation (form ?f1) (explanation ?e))
    (not (recommendation (form ?f2&: (< (str-compare ?f2 ?f1) 0)) ))
    =>
    (printout t "** Please take a copy of form " ?f1 crlf)
    (printout t "Explanation: " ?e crlf crlf)
    (retract ?r1)
)

;; running the system
(deffunction run-system()
    (reset)
    (focus startup interview recommend report)
    (run)
)

(while TRUE
    (run-system)
)