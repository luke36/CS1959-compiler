Works with Chez Scheme 9.5. Framework is modified to work with chez 9.
Beats yscheme in code length (but slower due to improper implementation of iterated register coalescing).
Comparison:

``` text
;; yscheme
** Options **
        forward-locations:     Yes
        closure optimization:  Yes
        pre-optimization:      Yes
        optimize jumps:        Yes

** closure analysis report **
       total closures created:  74
       total free var:          73
       average free var:        0.9864864864864865

** code length report **
       total code length:    6183
       average code length:  37.701219512195124
(time (test-all-analyze))
    1084 collections
    6.566406531s elapsed cpu time, including 0.663341791s collecting
    6.570452345s elapsed real time, including 0.666345217s collecting
    9122336624 bytes allocated, including 9120089792 bytes reclaimed

;; mine
** closure analysis report **
       total closures created:  71
       total free var:          74
       average free var:        1.0422535211267605

** code length report **
       total code length:    5862
       average code length:  35.74390243902439
(time (test-all-analyze))
    1321 collections
    8.371718957s elapsed cpu time, including 0.824038186s collecting
    8.376826559s elapsed real time, including 0.827704188s collecting
    11112810752 bytes allocated, including 11114388528 bytes reclaimed
```
