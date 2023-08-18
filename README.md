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
    0.243484369s elapsed cpu time, including 0.010845866s collecting
    0.244023897s elapsed real time, including 0.011174404s collecting
    1126270096 bytes allocated, including 1128770464 bytes reclaimed

;; mine
** Options **
        scheme standard:               r5rs
        garbage collection:            No
        encode large literals:         No
        optimize globals:              No
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes

** closure analysis report **
       total closures created:  73
       total free var:          69
       average free var:        0.9452054794520548

** code length report **
       total code length:    5818
       average code length:  35.47560975609756
(time (test-all-analyze))
    62 collections
    0.186370017s elapsed cpu time, including 0.003754762s collecting
    0.186402211s elapsed real time, including 0.003896082s collecting
    522822768 bytes allocated, including 521024016 bytes reclaimed
```

the best I can do is:

``` text
** Options **
        scheme standard:               r6rs
        garbage collection:            No
        encode large literals:         Above 0
        optimize globals:              Yes
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes

** closure analysis report **
       total closures created:  67
       total free var:          56
       average free var:        0.835820895522388

** code length report **
       total code length:    5112
       average code length:  31.170731707317074
```

but with extra runtime supports.

### additional features
- `quotient`, `remainder`
- `display`, `write`, `read-char`
- `cond`, `c[ad]+r`, `quasiquote`
- symbols, `symbol?`
- characters, `char?`, `char->integer`, `integer->char`, `char=?`
- long list literals
- `call/cc`
- `inspect` a continuation (a.k.a the stack)
- (hopefully correct) garbage collection

Conceptually bootstrapping is possible.

### about the course
I tried to supplement (some) missing ducuments FYI, and would like to add more.
