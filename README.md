Works with Chez Scheme 9.5. Framework is modified to work with chez 9.
Beats yscheme in code length. Comparison:

``` text
** closure analysis report **
       total closures created:  74
       total free var:          73
       average free var:        0.9864864864864865

** code length report **
       total code length:    6183
       average code length:  37.701219512195124

** closure analysis report **
       total closures created:  72
       total free var:          74
       average free var:        1.0277777777777777

** code length report **
       total code length:    5936
       average code length:  36.19512195121951
```
