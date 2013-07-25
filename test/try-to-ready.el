(require 'plsense)
(require 'el-expectations)

(expectations
  (desc "try-to-ready try")
  (expect (mock (plsense--open-buffer *))
    (stub plsense--ready-p => nil)
    (setq plsense--current-ready-check-timer nil)
    (plsense--try-to-ready))
  (desc "try-to-ready not try 1")
  (expect nil
    (stub plsense--ready-p => t)
    (plsense--try-to-ready))
  (desc "try-to-ready not try 2")
  (expect nil
    (stub plsense--ready-p => nil)
    (setq plsense--current-ready-check-timer t)
    (plsense--try-to-ready))
  )

