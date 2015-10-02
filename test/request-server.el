(require 'plsense)
(require 'el-expectations)

(expectations
  (desc "request-server do")
  (expect (mock (process-send-string 'hoge "svstat\n"))
    (stub plsense--ready-p => t)
    (stub plsense--get-process => 'hoge)
    (plsense--request-server "svstat"))
  (desc "request-server not do in not ready")
  (expect nil
    (stub plsense--ready-p => nil)
    (stub plsense--get-process => 'hoge)
    (plsense--request-server "svstat"))
  (desc "request-server do force")
  (expect (mock (process-send-string 'hoge "svstat\n"))
    (stub plsense--ready-p => nil)
    (stub plsense--get-process => 'hoge)
    (plsense--request-server "svstat" :force t))
  )

