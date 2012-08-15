;; -*- lexical-binding: t -*-
(defun edice-p (&rest numbers)
  "Return a function of one arg for displaying a dot.

Function will return non-nil when that arg is in NUMBERS"
  (lambda (num)
    (memq num numbers)))

(defvar edice-array `((,(edice-p 4 5 6)   ,(edice-p 2)     ,(edice-p 3 4 5 6))
                      (,(edice-p 6)       ,(edice-p 1 5 3) ,(edice-p 6))
                      (,(edice-p 3 4 5 6) ,(edice-p 2)     ,(edice-p 4 5 6))))

(defvar edice-color "black")

(defun edice-roll-dice-string-1 (number)
  (if (minusp number)
      (setq number (abs number)
            edice-color (or edice-color
                            "brown4")))
  (mapcar #'(lambda (line)
              (propertize (concat "|"
                                  (mapconcat #'(lambda (dot-fn)
                                                 (if (funcall dot-fn number) "x" " "))
                                             line
                                             " ")
                                  "|")
                          'face `((:background ,edice-color :foreground "white"))))
          edice-array))

(defun edice-roll-dice-string (roll)
  (let ((dice (mapcar #'edice-roll-dice-string-1
                      roll)))
    (mapconcat #'(lambda (line)
                   (mapconcat #'identity
                              line
                              "   "))
               (apply #'maplist #'(lambda (&rest partial-dice)
                                    (mapcar #'car partial-dice))
                      dice)
               "\n")))

(define-derived-mode edice-mode text-mode "edice"
  "A major mode for rolling dice"
  (setq buffer-read-only t)
  (set (make-local-variable 'global-hl-line-mode) nil))

(defun edice-random (ndice nrolls)
  (let (collected
        stopped)
    (while (plusp nrolls)
      (let ((roll (loop for j from 0 below ndice
                        collect (if (memq j stopped)
                                    (- (abs (nth j (car collected))))
                                    (1+ (random 6))))))
        (cond (;; (every #'null roll)
               (every #'minusp roll)
               (setq nrolls 0))
              (t
               ;; (null (intersection roll (car collected)
               ;;                     :test #'(lambda (n1 n2)
               ;;                               (unless (or (null n1)
               ;;                                           (null n2))
               ;;                                 (eql n1 n2)))))
               (decf nrolls)
               (when (zerop (random 2))
                 (pushnew (random ndice) stopped))
               (push roll collected)
               ;; (push (loop for j from 0 below ndice
               ;;             collect (or (nth j roll)
               ;;                         (nth j (car collected))))
               ;;       collected)
))))
    (reverse collected)))

(defvar edice-last-roll-point nil)
(defvar edice-last-roll nil)
(defun edice-roll (&optional howmany)
  (interactive "nHow many dice?")
  (with-current-buffer (pop-to-buffer (get-buffer-create "*edice!*"))
    (unless (eq major-mode 'edice-mode)
      (edice-mode))
    (recenter)
    (let ((buffer-read-only nil)
          (rolls (edice-random howmany 15))
          (delay 0.05))
      (cond (edice-last-roll
             (goto-char edice-last-roll-point)
             (delete-region (point) (point-max))
             (let ((edice-color "black"))
               (insert (edice-roll-dice-string edice-last-roll))))
            (t
             (goto-char (point-max))))
      (insert (format "\n\n%s: rolling %s dice...\n\n" (current-time-string) howmany))

      (while rolls
        (delete-region (point) (point-max))
        (save-excursion
          (setq edice-last-roll-point (point))
          (insert (edice-roll-dice-string (pop rolls))))
        (sit-for (setq delay (* 1.1 delay)))))))

(edice-roll 2)
