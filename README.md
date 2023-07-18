Works with Chez Scheme 9.5. Framework is modified to work with chez 9 and r6rs.

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
       total code length:    6153
       average code length:  37.51829268292683
(time (test-all-analyze))
    134 collections
    0.246573714s elapsed cpu time, including 0.009755400s collecting
    0.253762244s elapsed real time, including 0.010013157s collecting
    1126273056 bytes allocated, including 1128266720 bytes reclaimed

;; mine
** Options **
        iterated register coalescing:     Yes
        closure optimization:             Yes
        pre-optimization:                 Yes
        optimize jumps:                   Yes

** closure analysis report **
       total closures created:  71
       total free var:          74
       average free var:        1.0422535211267605

** code length report **
       total code length:    5837
       average code length:  35.59146341463415
(time (test-all-analyze))
    61 collections
    0.155646014s elapsed cpu time, including 0.005947933s collecting
    0.225316195s elapsed real time, including 0.006085444s collecting
    510043376 bytes allocated, including 514482704 bytes reclaimed
```
