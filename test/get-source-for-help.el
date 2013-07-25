(require 'plsense)
(require 'el-expectations)
(require 'tenv)

(expectations
  (desc "get-source-for-help package")
  (expect "package Hoge::Fuga"
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "#!/usr/bin/perl\n"
                        "package Hoge::Fuga;\n"))
        (search-backward "H" nil t)
        (plsense--get-source-for-help))))
  (desc "get-source-for-help builtin function")
  (expect "my $hoge = shift"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "my $hoge = shift;\n"))
        (search-backward "i" nil t)
        (plsense--get-source-for-help))))
  (desc "get-source-for-help builtin variable")
  (expect "my @arr = @ARGV"
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (insert (concat "my @arr = @ARGV;\n"))
        (search-backward "A" nil t)
        (plsense--get-source-for-help))))
  )


