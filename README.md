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
        garbage collection:            No
        encode large literal:          No
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
       total code length:    5780
       average code length:  35.24390243902439
(time (test-all-analyze))
    62 collections
    0.188350952s elapsed cpu time, including 0.006983826s collecting
    0.188802094s elapsed real time, including 0.007159432s collecting
    520955408 bytes allocated, including 521337056 bytes reclaimed
```

the best I can do is:

``` text
** Options **
        garbage collection:            No
        encode large literal:          Above 0
        optimize globals:              Yes
        iterated register coalescing:  Yes
        closure optimization:          Yes
        pre-optimization:              Yes
        optimize jumps:                Yes

** closure analysis report **
       total closures created:  69
       total free var:          64
       average free var:        0.927536231884058

** code length report **
       total code length:    5173
       average code length:  31.54268292682927
```

but with extra runtime supports.

### additional features:
- simple IO
- several syntactic extensions
- basic symbol manipulation
- option to encode large literals
- naieve `call/cc`
- (hopefully correct) garbage collection

Conceptually bootstrapping is possible.

### about the course
I tried to supplement (some) missing ducuments FYI, and would like to add more.
