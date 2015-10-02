(require 'plsense)
(require 'el-expectations)

;; TODO: Do test by stub server

(expectations
  ;; (desc "get-server-response get done by server start")
  ;; (expect (regexp "^Done$")
  ;;   (when (plsense--exist-process)
  ;;     (kill-process plsense--proc)
  ;;     (sleep-for 1))
  ;;   (setq plsense--proc nil)
  ;;   (stub plsense--ready-p => t)
  ;;   (plsense--get-server-response "svstart" :waitsec 60))
  ;; (desc "get-server-response get multiline by server status")
  ;; (expect "Main Server is .\nWork Server is .\nResolve Server is ."
  ;;   (stub plsense--ready-p => t)
  ;;   (let* ((ret (plsense--get-server-response "svstat"))
  ;;          (ret (replace-regexp-in-string "Not Running" "" ret))
  ;;          (ret (replace-regexp-in-string "Busy" "" ret))
  ;;          (ret (replace-regexp-in-string "Running" "" ret)))
  ;;     ret))
  ;; (desc "get-server-response get done by server stop")
  ;; (expect (regexp "^Done$")
  ;;   (stub plsense--ready-p => t)
  ;;   (plsense--get-server-response "svstop" :waitsec 60))
  (desc "get-server-response timeout")
  (expect t
    (stub plsense--ready-p => t)
    (plsense--log-enable-logging)
    (plsense--get-server-response "svstat")
    (when (get-buffer " *log4e-plsense*")
      (with-current-buffer " *log4e-plsense*"
        (goto-char (point-max))
        (forward-line -1)
        (when (string-match "Timeout get response of svstat" (thing-at-point 'line))
          t))))
  (desc "get-server-response not timeout by long wait")
  (expect "Main Server is .\nWork Server is .\nResolve Server is ."
    ;; (sleep-for 60)
    (stub plsense--ready-p => t)
    (let* ((ret (plsense--get-server-response "svstat" :waitsec 60))
           (ret (replace-regexp-in-string "Not Running" "" ret))
           (ret (replace-regexp-in-string "Busy" "" ret))
           (ret (replace-regexp-in-string "Running" "" ret)))
      ret))
  ;; (desc "get-server-response not force")
  ;; (expect ""
  ;;   (stub plsense--ready-p => nil)
  ;;   (plsense--get-server-response "svstart" :waitsec 60))
  ;; (desc "get-server-response force")
  ;; (expect (regexp "^Done$")
  ;;   (stub plsense--ready-p => nil)
  ;;   (plsense--get-server-response "svstart" :waitsec 60 :force t))
  ;; (desc "get-server-response ignore done")
  ;; (expect ""
  ;;   (stub plsense--ready-p => t)
  ;;   (plsense--get-server-response "svstop" :waitsec 60 :ignore-done t))
  )


