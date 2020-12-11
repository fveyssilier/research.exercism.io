;;; Solution to the Common-Lisp research problem on exercise.io

(defpackage common-lisp-1-b
  (:use :cl)
  (:export :make-pokemon :battle))
(in-package :common-lisp-1-b)

(defstruct pkmn
  "Structure representing a pokemon"
  name
  type
  atk
  hp)

; the required make-pokemon function is simply the auto-generated maker for pkmn struct
(setf (fdefinition 'make-pokemon) #'make-pkmn)

(defun turns-to-win (pkmn1 pkmn2)
  "Returns the number of turns needed for pkmn1 to knock-out pkmn2"
  (let ((modifier-table '((FIRE (WATER . 0.5) (GRASS . 2) (FIRE . 1))
                          (GRASS (FIRE . 0.5) (WATER . 2) (GRASS . 1))
                          (WATER (GRASS . 0.5) (FIRE . 2) (WATER . 1)))))
    (do ((hit-points (pkmn-hp pkmn2)
                     (- hit-points (* (pkmn-atk pkmn1)
                                      (cdr (assoc (pkmn-type pkmn2) (cdr (assoc (pkmn-type pkmn1) modifier-table)))))))
         (i 0 (1+ i)))
      ((<= hit-points 0) i))))

(defun battle (pkmn1 pkmn2)
  "Returns the name of the winner in a battle between pkmn1 and pkmn2"
  (if (<= (turns-to-win pkmn1 pkmn2) (turns-to-win pkmn2 pkmn1))
    (pkmn-name pkmn1)
    (pkmn-name pkmn2)))
