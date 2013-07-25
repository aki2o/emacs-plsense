(require 'plsense)
(require 'el-expectations)

(expectations
  (desc "start process")
  (expect 'run
    (loop for proc in (process-list)
          for procnm = (process-name proc)
          if (string= procnm "plsense")
          do (kill-process procnm))
    (plsense--start-process)
    (loop for proc in (process-list)
          for procnm = (process-name proc)
          if (string= procnm "plsense")
          return (process-status proc)
          finally return nil))
  (desc "exist process")
  (expect t
    (plsense--exist-process))
  (desc "get process")
  (expect t
    (stub plsense--start-process => nil)
    (processp (plsense--get-process)))
  (desc "stop process")
  (expect t
    (plsense--stop-process)
    (sleep-for 1)
    (loop for proc in (process-list)
          for procnm = (process-name proc)
          if (string= procnm "plsense")
          return nil
          finally return t))
  (desc "not exist process")
  (expect nil
    (plsense--exist-process))
  (desc "get process with try start")
  (expect (mock (plsense--start-process))
    (plsense--get-process))
  )


