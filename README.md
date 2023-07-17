Works with Chez Scheme 9.5. Framework is modified to work with chez 9.
Beats yscheme in code length (but slower due to improper implementation of iterated register coalescing).
Comparison:

``` text
;; yscheme
** closure analysis report **
       total closures created:  74
       total free var:          73
       average free var:        0.9864864864864865

** code length report **
       total code length:    6183
       average code length:  37.701219512195124

;; mine
** closure analysis report **
       total closures created:  71
       total free var:          74
       average free var:        1.0422535211267605

** code length report **
       total code length:    5861
       average code length:  35.73780487804878
```
