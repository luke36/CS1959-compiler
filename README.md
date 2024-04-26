Works with Chez Scheme 9.5. Framework is modified to work with chez 9 and r6rs.

Beats yscheme in code length (but slower due to improper implementation of iterated register coalescing).

Comparison (change filename extension to run yscheme):

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
    0.243484369s elapsed cpu time, including 0.010845866s collecting
    0.244023897s elapsed real time, including 0.011174404s collecting
    1126270096 bytes allocated, including 1128770464 bytes reclaimed

;; mine
** Options **
        scheme standard:               r5rs
        garbage collection:            No
        continuation:                  No
        encode large literals:         No
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes
        optimize allocation:           Yes

** closure analysis report **
       total closures created:  73
       total free var:          69
       average free var:        0.9452054794520548

** code length report **
       total code length:    5188
       average code length:  31.634146341463413
(time (test-all-analyze))
    57 collections
    0.164807323s elapsed cpu time, including 0.003457958s collecting
    0.164815706s elapsed real time, including 0.003596846s collecting
    478012224 bytes allocated, including 478985232 bytes reclaimed
```

### additional features
- `quotient`, `remainder`
- `display`, `write`, `read-char`
- `cond`, `quasiquote`, `unquote`
- symbols, `symbol?`
- characters, `char?`, `char->integer`, `integer->char`, `char=?`
- long list literals
- naieve `call/cc`
- `inspect` a continuation (a.k.a the stack)
- (hopefully correct) garbage collection

Conceptually bootstrapping is possible.

### about the course
I tried to supplement (some) missing ducuments FYI, and would like to add more.
