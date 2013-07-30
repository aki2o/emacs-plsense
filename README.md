[Japanese](https://github.com/aki2o/emacs-plsense/blob/master/README-ja.md)

What's this?
============

This is a extension of Emacs that provide interface for PlSense.

PlSense is a development tool for Perl.  
PlSense provides completion/help optimized for context.  
For detail, see https://github.com/aki2o/plsense/blob/master/README.md

Using this extension, you can do coding Perl on Emacs like the following.


Feature
=======

### Provide a optimized completion by auto-complete.el

When you are on Perl buffer, provide completion optimized for context by auto-complete.el.  
Can identify the context having Argument/Return of method, element of Array/Hash/Reference.  
Provide a optimized completion about the following.

* Variable
* Method
* Module
* Initializer of Class
* LIST of Use/Require statement
* Key of Hash

![demo1](image/demo1.png)

### Provide a optimized help by doing popup or displaying buffer

![demo2](image/demo2.png)

### Provide information of method by eldoc.el

![demo3](image/demo3.png)


Demo
====

http://www.youtube.com/watch?v=Q8XDhxqmaXs


Requirement
===========

* Unix Shell ( e.g. Cygwin on Windows )
* Perl
* PlSense


Install
=======

I recommend using el-get for installing this extension.  
Downloading manually or using auto-install.el are OK,
but installing each the following dependency is required in this case.

### If use el-get.el

2013/07/26 Not yet available.  

If you set `el-get-sources` in your .emacs or site-start.el file,  
You can available el-get to install this extension.

    (setq el-get-sources
          '(
            (:name log4e
                   :website "https://github.com/aki2o/log4e"
                   :description "provide logging framework for elisp."
                   :type github
                   :pkgname "aki2o/log4e")
            (:name yaxception
                   :website "https://github.com/aki2o/yaxception"
                   :description "provide framework about exception like Java for elisp."
                   :type github
                   :pkgname "aki2o/yaxception")
            (:name plsense
                   :website "https://github.com/aki2o/emacs-plsense"
                   :description "provide interface for PlSense that is a development tool for Perl."
                   :type github
                   :pkgname "aki2o/emacs-plsense"
                   :depends (auto-complete log4e yaxception))
            ))
    
### If use auto-install.el

    (auto-install-from-url "https://raw.github.com/aki2o/emacs-plsense/master/plsense.el")

### Dependency

* [auto-complete.el](https://github.com/auto-complete/auto-complete)
* [log4e.el](https://github.com/aki2o/log4e)
* [yaxception.el](https://github.com/aki2o/yaxception)


Configuration
=============

    (require 'plsense)
    ;; Popup help about pointed something
    (setq plsense-popup-help-key "C-:")
    ;; Display help buffer about pointed something
    (setq plsense-display-help-buffer-key "M-:")


Usage
=====

### Show version

* `plsense-version` ... show version of PlSense.

**Note:** Double as a verification of install.

### Start/Stop server

* `plsense-server-start` ... start process of PlSense server.
* `plsense-server-stop` ... stop process of PlSense server.

**Note:** You must finish configuration of PlSense on ahead.  
**Note:** You must do `plsense-server-start` on Emacs regardless of whether PlSense server process exist.  
**Note:** Executing the above command redundantly is OK.  
**Note:** Maybe show `... is failed` despite the success of the above command along of timeout.  
**Note:** In the case, verify status of PlSense server seeing 'Information of server' section below.  

### Information of server

* `plsense-server-status` ... show status of PlSense server.
* `plsense-server-task` ... show the running tasks on PlSense server.

#### Kind of server

* Main Server ... provide completion/help.
* Work Server ... manage task for searching library and analyzing File/Module.
* Resolve Server ... gather result of analyzing File/Module.

#### Status of server

* Running ... can accept client.
* Busy ... can not accept client.
* Not Running ... not yet started.

#### Task of server

* build _File/Module_ ... Analyzing the File/Module.
* find _String_ ... Searching library about the String.

### Active/Inactive provision of completion/help

The above command in 'Start/Stop of server' section, double as a switching active/inactive.  
If you have executed `plsense-server-start`, provision of completion/help is active when open buffer.  
But, not start provision of completion/help until finish analyzing the buffer.  
When start provision of completion/help, show `... is ready.`.

* `plsense-buffer-is-ready` ... show status about analyzing current buffer.
* `plsense-reopen-current-buffer` ... restart analyzing current buffer.

**Note:** Not activate automatically on the buffer that opened before executing `plsense-server-start`.  
**Note:** In the case, execute `plsense-reopen-current-buffer` for activate.  
**Note:** Not activate automatically if the buffer file is not exists.  
**Note:** When execute `find-file`, save the buffer.  

#### Status about analyzing buffer

* Yes ... finished.
* Now Loading ... do analyzing now.
* No ... not start analyzing.
* Not Found ... PlSense server can't identify the buffer file.

**Note:** Maybe show 'No' or 'Not Found' despite the finish of analyzing.  
**Note:** If retry a few times but remain showing 'No', the buffer maybe not activate.  
**Note:** If retry a few times but remain showing 'Not Found', seeing 'Sync server' section below.  

### Sync server

For a optimized completion/help, it's required that synchronization of context between Emacs and PlSense server.  
Normally, it's finished by this extension automatically.  
But, it maybe happen that synchronization is failed by some reason and it isn't recovered automatically.  
If not show completion/help and show error message continuously, execute the following command.

* `plsense-update-location` ... inform Plsense server of current context forcibly.

**Note:** If not recovered by the above command, restart PlSense server.  

### Refresh server

The result of analyzing is gathered on PlSense server with edit buffer on Emacs.  
Then, the following maybe happen by running PlSense server over a long time.  

* Increase of quantity of expending memory by PlSense server.
* If you edit same point in many times, the result of completion/help differ from expectation.

In the case, execute the following command.

* `plsense-server-refresh` ... initialize PlSense server and start analyzing the newest source code.

**Note:** Restart PlSense server is also OK. But if running task exists, the result is lost.


Restriction
===========

### Identify context

PlSense identify context by analyzing a source code of Perl.  
But, can't identify the all context because Perl has a lot of grammar.  
It maybe happen that analyzing is failed and completion/help is not provided.  
For detail, see https://github.com/aki2o/plsense/blob/master/README.md

### Content of help

Picking up the Variable/Method help from PerlDoc maybe failed.  
In the case, show the help of module that the Variable/Method belong to.

### Optimize completion/help

Analyzing is started from current buffer recursively in sequence.  
It like the following.  

CurrentBuffer A => A's UseModule B => B's UseModule C ...

Provision of completion/help is started when finished analyze current buffer.  
But, analyzing continue in the background like the above.  
It means that a few minutes is required for the result of completion/help is optimized.  
It apply when you open/edit/save buffer.

The result of analyzing is saved and used until the file is changed.  
So, If finish the all analyzing, provision of a optimized completion/help is started soon.  

About the time required for the all analyzing is finished,  
see https://github.com/aki2o/plsense/blob/master/README.md

I guess that the Emacs action become sluggish while a many tasks is running.  
If you notice it, you can show the task by `plsense-server-task`.

### Reflect the result of editing buffer

I think the best way is that analyzing is done for latest content of buffer when user require completion/help.  
But, the way is high cost.  
So, do analyzing at the other timing for high-performance.  
At present, the timing is when the following command is executed.

* save-buffer
* newline
* newline-and-indent

Analyzing is not done without executing the above command.  
And, analyzing is simplified at the timing other than save-buffer.  
It means that the following item is not reflect at these timing.

* imported method/variable by use statement

I hope you execute save-buffer frequency.  
For example, the way is using auto-save-buffers.


Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* auto-complete.el ... 1.4.0
* log4e.el ... 0.1
* yaxception.el ... 0.1


**Enjoy!!!**

