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
    0.349357648s elapsed cpu time, including 0.014140636s collecting
    0.349224759s elapsed real time, including 0.014428504s collecting
    1126257888 bytes allocated, including 1128416816 bytes reclaimed

;; mine
** Options **
        garbage collection:            No
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
       total code length:    5780
       average code length:  35.24390243902439
(time (test-all-analyze))
    62 collections
    0.258806927s elapsed cpu time, including 0.009962852s collecting
    0.279805691s elapsed real time, including 0.010122055s collecting
    516086624 bytes allocated, including 521651584 bytes reclaimed
```

### additional features:
- simple IO
- several syntactic extensions
- basic symbol manipulation
- option to encode large literals
- naieve `call/cc`
- (hopefully correct) garbage collection

I tried to supplement (some) missing ducuments FYI, and would like to add more.
