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
       total closures created:  71
       total free var:          69
       average free var:        0.971830985915493

** code length report **
       total code length:    5753
       average code length:  35.079268292682926
(time (test-all-analyze))
    59 collections
    0.206787233s elapsed cpu time, including 0.007704578s collecting
    0.273332087s elapsed real time, including 0.007845003s collecting
    496021904 bytes allocated, including 496291680 bytes reclaimed
```
