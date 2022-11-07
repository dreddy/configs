(require 'autoinsert)

(auto-insert-mode 1)
(setq auto-insert-alist '())
(setq auto-insert-query nil)

(add-to-list 'auto-insert-alist
             '(python-mode
               nil
               "#!/usr/bin/env python3\n"
               "\n"
               _ "\n"
               "\n"
               "if __name__ == '__main__':\n"
               > "\n\n"
               ))

(add-to-list 'auto-insert-alist
             '(("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
               nil
               "/*" \n
               (file-name-nondirectory (buffer-file-name)) \n
               " */" > \n \n
               "#include <iostream>" \n \n
               "using namespace std;" \n \n
               "int main()" \n
               -4 "{" \n
               > _ \n
               > _ "return 0;" \n
               -4 "}" > \n))

(provide 'dr-auto-insert)
