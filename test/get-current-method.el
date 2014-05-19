(require 'plsense)
(require 'el-expectations)
(require 'tenv)

(expectations
  (desc "get-current-method not exist sub")
  (expect nil
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (with-current-buffer (find-file-noselect src)
        (erase-buffer)
        (goto-char (point-max))
        (insert (concat "package Hoge;\n"
                        "# my hoge class\n"
                        "use strict;\n"))
        (plsense--get-current-method))))
  (desc "get-current-method on sub definition")
  (expect nil
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "sub get_hoge"))
        (plsense--get-current-method))))
  (desc "get-current-method exist sub")
  (expect "get_hoge"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat " {\n"
                        "  my $self = shift;\n"))
        (plsense--get-current-method))))
  (desc "get-current-method many braces")
  (expect "get_hoge"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "  first { $_->get_name() eq 'hoge' } grep { eval { $_->get_name } } @_;\n"))
        (plsense--get-current-method))))
  (desc "get-current-method last brace")
  (expect "get_hoge"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "}"))
        (plsense--get-current-method))))
  (desc "get-current-method exit sub")
  (expect nil
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "\n"))
        (plsense--get-current-method))))
  (desc "get-current-method not yet exit sub")
  (expect nil
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "\n"
                        "my $self = shift;\n"))
        (plsense--get-current-method))))
  (desc "get-current-method other sub")
  (expect "get_fuga"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "sub get_fuga : PRIVATE {"))
        (plsense--get-current-method))))
  )


