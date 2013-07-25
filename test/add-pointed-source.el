(require 'plsense)
(require 'el-expectations)
(require 'tenv)

(expectations
  (desc "add-pointed-source add until sub")
  (expect (mock (plsense--request-server
                 "codeadd sub get_hoge {  my $self = shift;  "))
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (tenv-update-file src t
                        "package Hoge;\n"
                        "# my hoge class\n"
                        "use strict;\n"
                        "sub get_hoge {\n"
                        "  my $self = shift;\n"
                        "  ")
      (stub plsense--try-to-ready => nil)
      (stub plsense--ready-p => t)
      (stub plsense--set-current-file => t)
      (stub plsense--set-current-package => t)
      (stub plsense--set-current-method => t)
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (plsense--add-pointed-source))))
  (desc "add-pointed-source add until package")
  (expect (mock (plsense--request-server
                 "codeadd package Hoge;use strict;"))
    (let ((src (tenv-get-tmp-file "plsense" "test")))
      (stub plsense--try-to-ready => nil)
      (stub plsense--ready-p => t)
      (stub plsense--set-current-file => t)
      (stub plsense--set-current-package => t)
      (stub plsense--set-current-method => t)
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (forward-line -2)
        (goto-char (point-at-bol))
        (plsense--add-pointed-source))))
  (desc "add-pointed-source add not has package")
  (expect (mock (plsense--request-server
                 "codeadd use strict;use warnings;print \"hoge\";"))
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (tenv-update-file src t
                        "#!/usr/bin/perl\n"
                        "use strict;\n"
                        "use warnings;\n"
                        "\n"
                        "print \"hoge\";\n"
                        "")
      (stub plsense--try-to-ready => nil)
      (stub plsense--ready-p => t)
      (stub plsense--set-current-file => t)
      (stub plsense--set-current-package => t)
      (stub plsense--set-current-method => t)
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (plsense--add-pointed-source))))
  (desc "add-pointed-source not add while same source")
  (expect nil
    (let ((src (tenv-get-tmp-file "plsense" "test" t t)))
      (stub plsense--try-to-ready => nil)
      (stub plsense--ready-p => t)
      (stub plsense--set-current-file => t)
      (stub plsense--set-current-package => t)
      (stub plsense--set-current-method => t)
      (stub plsense--request-server => t)
      (with-current-buffer (find-file-noselect src)
        (goto-char (point-max))
        (plsense--add-pointed-source))))
  )


