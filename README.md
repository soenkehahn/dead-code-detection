## *status: experimental*

`dead-code-detection` detects dead code in haskell projects.

This project is still in an early stage. Currently only those language
constructs have been implemented that I have stumbled across in projects I used
it on. If the tool encounters a language construct that it doesn't understand
yet, it will crash. (I *think* this is the best behavior since ignoring
unimplemented language constructs would easily result in false positives and
false negatives.) If you use `dead-code-detection` on any project and it
doesn't work due to a not implemented language construct, please consider
opening an issue on github.

``` shell
$ dead-code-detection --root Main -isrc
src/Example/Module.hs:42:23: unusedName
```
