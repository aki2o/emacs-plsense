(require 'plsense)
(require 'el-expectations)

(expectations
  (desc "receive-server-response get response")
  (expect "Done\n> "
    (setq plsense--server-response "")
    (plsense--receive-server-response nil "Done\n> ")
    plsense--server-response)
  (desc "receive-server-response add response")
  (expect "Done\n> Done\n> "
    (plsense--receive-server-response nil "Done\n> ")
    plsense--server-response)
  (desc "handle-err-response not yet exist module")
  (expect (mock (plsense--handle-err-response "Not yet exist [Hoge] of [/tmp/hoge.pm]"))
    (let ((res (concat "ERROR: Not yet exist [Hoge] of [/tmp/hoge.pm]\n"
                       "ERROR: Check the module status is not 'Nothing' by ready command.\n"
                       "Failed\n> ")))
      (plsense--receive-server-response nil res)))
  ;; (desc "handle-err-response not yet exist module")
  ;; (expect (mock (message "[PlSense] Please exec 'plsense-reopen-current-buffer' on '%s'"
  ;;                        "/tmp/hoge.pm"))
  ;;   (setq plsense--ignore-done-response-p nil)
  ;;   (let ((res (concat "ERROR: Not yet exist [Hoge] of [/tmp/hoge.pm]\n"
  ;;                      "ERROR: Check the module status is not 'Nothing' by ready command.\n"
  ;;                      "Failed\n> ")))
  ;;     (plsense--receive-server-response nil res)))
  ;; (desc "handle-err-response not found method")
  ;; (expect (mock (message "[PlSense] Please exec 'plsense-reopen-current-buffer' on '%s'"
  ;;                        "/tmp/hoge.pm"))
  ;;   (setq plsense--ignore-done-response-p nil)
  ;;   (let ((res (concat "ERROR: Not found [get_hoge] in [Hoge] of [/tmp/hoge.pm]\n"
  ;;                      "ERROR: Check the module status is not 'Nothing' by ready command.\n"
  ;;                      "Failed\n> ")))
  ;;     (plsense--receive-server-response nil res)))
  ;; (desc "handle-err-response not yet set")
  ;; (expect (mock (message "[PlSense] Please exec 'plsense-reopen-current-buffer' on '%s'"
  ;;                        (buffer-name)))
  ;;   (setq plsense--ignore-done-response-p nil)
  ;;   (let ((res (concat "ERROR: Not yet set current file/module by onfile/onmod command\n"
  ;;                      "Failed\n> ")))
  ;;     (plsense--receive-server-response nil res)))
  (desc "receive-server-response not add response")
  (expect "Done\n> Done\n> "
    plsense--server-response)
  )
