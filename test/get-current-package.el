(require 'plsense)
(require 'el-expectations)
(require 'tenv)

(expectations
  (desc "get-current-package not exist package")
  (expect "main"
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (with-current-buffer (find-file-noselect src)
        (erase-buffer)
        (goto-char (point-max))
        (insert (concat "#!/usr/bin/perl\n"
                        "use strict;\n"))
        (plsense--get-current-package))))
  (desc "get-current-package on package definition")
  (expect "main"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "package Hoge"))
        (plsense--get-current-package))))
  (desc "get-current-package exist package")
  (expect "Hoge"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat ";"))
        (plsense--get-current-package))))
  (desc "get-current-package other package")
  (expect "Fuga"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "\n"
                        "use strict;\n"
                        "1;\n"
                        "package Fuga;\n"
                        "my $fuga;"))
        (plsense--get-current-package))))
  (desc "get-current-package commentted package")
  (expect "Fuga"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "\n"
                        "1;\n"
                        "# package Bar;\n"))
        (plsense--get-current-package))))
  )


