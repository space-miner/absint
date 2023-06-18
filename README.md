# absint
interval analysis by abstract interpretation on a simple language.

### language specs
* assignment
* arithmetic limited to addition and subtraction
* assume *Cond* statement, where *Cond* is an equality comparison between variables and variable and constants
* nondeterministic choice *Cmd1*[]*Cmd2*
* sequencing *Cmd1*;*Cmd2*

### todo
* fix sequences inside while 
* widening operator
* refactor to support more abstract domains
* sign analysis
* fuzzing
* prove soundness using whyml
