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

* plsense-version ... show version of PlSense.

**Note:** double as a verification of install

### Start/Stop server

* plsense-server-start ... start process of PlSense server.
* plsense-server-stop ... stop process of PlSense server.


Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* auto-complete.el ... 1.4.0
* log4e.el ... 0.1
* yaxception.el ... 0.1


**Enjoy!!!**

