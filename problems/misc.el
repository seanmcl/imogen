(defun hash40()
  (shell-command-to-string "od -N 20 -A n -X /dev/random | tr -d ' \n'"))

(defun i40()
  (interactive)
  (insert (hash40)))

(defun thm ()
  (interactive)
  (let* ((id (hash40)
         (f (read-from-minibuffer "f: "))
         (s (completing-read "s: " '("Theorem" "NonTheorem")))))
    (insert (format
             "{\"id\": \"%s\",\n\"formula\": \"%s\",\n\"status\": \"%s\"}," id f s))
    ))
