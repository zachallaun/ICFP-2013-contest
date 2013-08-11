# ICFP 2013 contest: synthesis of bitvector programs

## background reading:

- [Dimensions of Program Synthesis](http://www.cis.upenn.edu/~alur/CIS673/gulwani10.pdf): An overview of the field
- [Oracle-Guided Component-Based Program Synthesis](http://www.eecs.berkeley.edu/~sseshia/review/seshia-icse10.pdf): Explanation of a system for synthesizing bitvector and deobfuscation programs
- [Synthesis of Loop-free Programs](http://research.microsoft.com/en-us/um/people/sumitg/pubs/pldi11-loopfree-synthesis.pdf): Similar to oracle-guided blah blah blah

## notes:

### less significant bits matter more than more significant bits (Oracle-guided p.6)

Property 1 (See [28], Chapter 2). A function mapping bitvectors to
bitvectors can be implemented with add, subtract, bitwise and, bitwise
or, and bitwise not instructions if and only if each bit of the output
depends only on bits at or less significant than that bit in each
input operand.

antitonicity! (this occurs to Lindsey, the jury is out whether this matters)

WARNING: most significant bits on the right

    interesting bit (instead of just depending on itself, it depends on previous bits)
    v
  0101000 (x)
+ 1100010 (f)
---------
  1011010

### `fold`

```clj
(fold number init (lambda (x acc) expression))

(define shl8
  (lambda (n)
    (fold 0x1122334455667788 ;; do this 8 times
          n
          (lambda (_ acc) (shl1 acc)))))
```

### 64 bit numbers

All Clojure integer literals are java.lang.Longs, which are 64 bits _signed_. The spec wants unsigned numbers, so we may have to convert some of the outputs given by the server to their signed variants.

### Fails

WORKING ON: {:id "ScdXCm1naycUlAIVnXHDuBWK", :size 8, :operators (plus shl1 shr16)}
WORKING ON: {:id "Gk7s4QYktPbigzWbOaejLUCJ", :size 8, :operators (plus shl1 shr16)}
WORKING ON: {:id "DBEoYLcsmvyBVFmWBgFv5T6x", :size 8, :operators (not shl1 shr4 xor)}
WORKING ON: {:id "3OdmUS8TBma49AMz5wDbzZzh", :size 8, :operators (not plus shl1 shr1)}
WORKING ON: {:id "frZx7ZV64mmNFvBNvOl4beFx", :size 10, :operators (and if0 shl1 shr4)
WORKING ON: {:id "4VDBwPOAnRmORhYBATbJMl4q", :size 11, :operators (and if0 shr1 shr16 xor)}
