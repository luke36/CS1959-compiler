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
        encode large literal:          No
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes

** closure analysis report **
       total closures created:  73
       total free var:          69
       average free var:        0.9452054794520548

** code length report **
       total code length:    5774
       average code length:  35.207317073170735
(time (test-all-analyze))
    59 collections
    0.169994175s elapsed cpu time, including 0.006086615s collecting
    0.237339270s elapsed real time, including 0.006214169s collecting
    493674240 bytes allocated, including 495636848 bytes reclaimed
```
