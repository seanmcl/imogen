
(defconst Imogen.test-dir "~/save/projects/imogen/tests/fol/basic" "Tests")

(defun Imogen.make-test (beg end)
  "Create a test header that can be parsed by our testing tools."
  (interactive "r")
  (save-window-excursion
    (let* ((files (shell-command-to-string
                   (format "ls -1 %s/*.imo | sort | tail -1" Imogen.test-dir)))
           (_ (string-match "FOL\\([[:digit:]]+\\)\\+1.imo" files))
           (m (match-string 1 files))
           (int (string-to-number m))
           (int (1+ int))
           (name (format "FOL%03d+1" int))
           (file (format "%s/%s.imo" Imogen.test-dir name))
           (name (format "%% Problem : %s" name))
           (is-true (y-or-n-p "True? "))
           (status (if is-true "% Status  : Theorem" "% Status  : NonTheorem"))
           (rating "% Rating  : 0.0")
           (problem (buffer-substring beg end)))
      (find-file file)
      (insert "\n")
      (insert name)
      (insert "\n")
      (insert status)
      (insert "\n")
      (insert rating)
      (insert "\n\n")
      (insert problem)
      (save-buffer))))

(defun Imogen.new-test-from-region (beg end)
  (interactive "r")
