(require 'autoinsert)
(auto-insert-mode 1)
(setq auto-insert-alist '())
(setq auto-insert-query nil)

(add-to-list 'auto-insert-alist
             ;; C Program
             '(("\\.c$" . "C Program")
               nil
               "/* -*- C -*- */\n"
               "# include <stdio.h>\n"
               "# include <stdlib.h>\n\n"
               "# include \""
               (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name))
               ".h\"\n"
               ))

(add-to-list 'auto-insert-alist
             ;; C++ Program
             '(("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ Program")
               nil
               "// -*- C++ -*-\n"
               "# include \""
               (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name))
               ".hh\"\n"
               "# include <stdio.h>\n"
               "# include <stdlib.h>\n"
               "# include <string>\n"
               "# include <vector>\n\n"
               ))

(add-to-list 'auto-insert-alist
             '(("\\.py$" . "Python Program")
               nil
               "#! /usr/bin/env python3\n\n"
               "\"\"\"" (file-name-nondirectory buffer-file-name) "\"\"\"\n\n"
               "import os, sys, re, datetime\n"
               "import fileinput\n\n"
               "def main():\n"
               "    pass"
               _
               "\n \n"
               "if __name__ == '__main__':\n"
               "    try:\n"
               "        main()\n"
               "    finally:\n"
               "        None;\n\n"

               (progn (save-buffer)
                      (shell-command (format "chmod +x %s"
                                             (buffer-file-name)))
                      "")
               ))

(provide 'dr-auto-insert)
