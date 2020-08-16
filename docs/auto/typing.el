(TeX-add-style-hook
 "typing"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("wasysym" "nointegrals")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "ebproof"
    "amsmath"
    "amssymb"
    "amsthm"
    "upgreek"
    "wasysym"
    "xcolor")
   (TeX-add-symbols
    '("deduct" ["argument"] 2)
    '("derives" 3)
    '("rcd" 1)
    '("bnfvar" 1)
    '("consider" 1)
    '("todo" 1)
    '("ignore" 1)
    "define"
    "arrow"
    "emptyrow"
    "spc"
    "ctx"))
 :latex)

