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
        encode large literals:         No
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes

** closure analysis report **
       total closures created:  73
       total free var:          69
       average free var:        0.9452054794520548

** code length report **
       total code length:    5744
       average code length:  35.02439024390244
(time (test-all-analyze))
    65 collections
    0.210114854s elapsed cpu time, including 0.005291485s collecting
    0.210109956s elapsed real time, including 0.005462918s collecting
    538650976 bytes allocated, including 545938400 bytes reclaimed
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
