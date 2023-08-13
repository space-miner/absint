# absint
interval analysis by abstract interpretation on a simple language.

![](https://upload.wikimedia.org/wikipedia/commons/thumb/8/8b/Abstract_interpretation_of_integers_by_signs_svg.svg/800px-Abstract_interpretation_of_integers_by_signs_svg.svg.png)
### language specs
* assignment
* arithmetic limited to addition and subtraction
* assume *Cond* statement, where *Cond* is an equality comparison between variables and variable and constants
* nondeterministic choice *Cmd1*[]*Cmd2*
* sequencing *Cmd1*;*Cmd2*

### todo
* fix sequences inside while 
* widening operator
* refactor to support more abstract domains -- i.e. sign analysis
* make an interpreter on the concrete semantics
* fuzzing and diff test?
* prove soundness using whyml
