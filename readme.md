# absint
an abstract interpreter for a simple flow-chart language. abstract interpreters are tools used in program analysis that uses abstract semantics to analyze and reason about the behaviors of programs. Unlike concrete interpreters, which simulate the exact behavior of a program, abstract interpreters work by over-approximating the error state of a program in the abstract domain (e.g. sign domain) and iteratively refines approximations till fix-point and uses a transfer function to move from the concrete semantics to the abstract and back.

![](https://upload.wikimedia.org/wikipedia/commons/thumb/8/8b/Abstract_interpretation_of_integers_by_signs_svg.svg/800px-Abstract_interpretation_of_integers_by_signs_svg.svg.png)
### specs
* the language
  * assignment
  * arithmetic limited to addition and subtraction
  * assume *Cond* statement, where *Cond* is an equality comparison between variables and variable and constants
  * nondeterministic choice *Cmd1*[]*Cmd2*
  * sequencing *Cmd1*;*Cmd2*
* the interpreter
  * supports sign domain
  * and interval domain

### example
a program in this language (test23)
```
x = 0;
y = 100;
while (x < y) {
    x = x + 1;
    y = y - 1
}
```

interval analysis on the program above (there's extra labels and they're out of order due to walking the ast)
```
label0:                              
label5: 
x: [0, 49]
y: [1, 100]
label4: 
x: [0, 50]
y: [1, 100]
label6: 
x: [0, 50]
y: [0, 100]
label7: 
label2: 
label3: 
x: [0, 49]
y: [1, 100]
label1: 
x: [0, 0]

...miscellaneous ast representation with labels for something that is hairier to parse than
the hand-typed representation below

-------------------label0
x = 0;
-------------------label1
y = 100;
while (x < y) { ---label5
-------------------label3
    x = x + 1;
-------------------label4
    y = y - 1
}
-------------------label6
```

### todo
* make memory and interpreter polymorphic over domains
* add guage domain 
* move modules to lib
* make an interpreter on the concrete semantics
* fuzzing and diff test?
* prove soundness using whyml
