@Make[Report]
@Disable[Contents]

@comment{================================================================}

@Begin[Heading]
OPS5 Language Introduction

Michael Mauldin
October, 1992
@End[Heading]


This document contains a sketchy description of OPS5 language features,
syntax and semantics of conditions and actions.  For more information,
consult the OPS5 manual.

@Section[Production Memory]

create rules with @B[p] (production) or @b[build] (later)

an OPS5 production-rule definition is a list containing
@Begin[Itemize]
a function call to @b[p] 

LHS = one or more condition elements (first not negated), each in Lisp
list format.

a separator = @t{-->}

RHS = one or more actions, each in Lisp list format.
@End[Itemize]

@Section[Sample Rule]
@Begin[Verbatim]
@Tabclear
@Tabdivide[8]
;; IF    the key is on AND the engine is not turning
;; THEN  conclude that the problem is in the starting system
(p bad-starting-system
    (task ^goal diagnose)
    (fact ^name |key is off| ^value no)
    (fact ^name |engine is turning| ^value no)
    -->
    (bind <x> |problem is in starting system|)
    (make fact ^name <x> ^value yes)
    (write (crlf) Concluding <x> (crlf)))
@End[Verbatim]

@Section[Left-Hand Side]

LHS is collection of patterns to be matched against working memory.  Each
pattern contains an element-class name followed by some number of LHS terms.
Each term consists of an @t{^attribute-name} followed by a LHS-value.  The
LHS-value can be a

@Begin[Description]
constant@\in pattern @t{^on couch}, ``couch'' is a constant;
in pattern @t{^GRE 100}, ``100'' is a constant;

variable@\in pattern, @t{^Status <n1>}, ``<n1>'' is variable that will be
bound during matching to an actual value for some element in
working memory;

predicate operator @\one of seven operators may precede
a constant or variable:
=, <>, <=>, <, <=, >=, >; the = is assumed if no operator is present;

disjunction@\in the pattern @t{^weight << light medium >>}, ``<< light
medium >>'' specifies that only one of the set of values, light and
medium, must match; any LHS-values may be contained in the disjunction;
@I[warning] leave spaces between values and angle brackets 
to avoid confusing them with variable brackets; 

conjunction@\in pattern @t[^GRE { > 600 < 800 }], ``{ > 600 < 800 }''
specifies a set of value restrictions all of which must match; any
LHS-values may be contained in the conjunction;

@End[Description]

Restrictions to predicate operators: 

@Begin[Itemize]
<, <=, >= and >
used only with numbers and with variables bound to numbers.
<=> means same type, and <> means not equal.

first occurrence of a variable cannot be
preceded by any predicate other than = (first occurrence establishes binding)

@End[Itemize]

A condition pattern in LHS (other than first) may be negated by putting
a ``-'' in front of the normal pattern

Ordering of condition elements is significant in variable binding,
for conflict resolution and for match efficiency

@Section[RHS of OPS5 Rules]

@Begin[Itemize]
The RHS of the OPS5 rule consists of an ordered sequence of actions.  

The primitive actions that affect working memory are @b[make], @b[modify],
and @b[remove].

The @b[write] action is used to output information.  

The @b[halt] action provides a way of explicitly stopping the firing of
production rules.

RHS can also contain functions
that return values within the actions.  For example, the @B[compute]
function allows OPS5 to do arithmetic.  It provides for infix evaluation of
+,-,*, //, and \\ (respectively addition, subtraction, multiplication,
division, and modulus).  Operations are performed from right to left.  

These and other actions and functions will be demonstrated by example.
@End[Itemize]


@Section[Specific Commands]

@Center[@b[The WATCH Command]]

@Begin[description]
no argument@\Print current watch level (initialized to 1) unchanged

@t{(watch 0)}@\No report of firings or changes to working memory

@t{(watch 1)}@\Report rule name and time tags of each working memory
element for each instantiation fired

@t{(watch 2)}@\In addition to level 1 reports, give each change (add
or delete) to working memory 
@End[description]

@Center[@b[The RUN Command]]

@Begin[Description]
@t{(run)}@\run until a break or halt or no rules in conflict set

@t{(run N)}@\run N steps unless early stop as above

@t{(run 1)}@\for single stepping
@end[Description]

@center[@b[[The WM and PPWM Commands]]

@b[(wm)] -- list the contents of working memory,
optional arguments specify time tags;
if no time tags are given, shows all elements.

@t{(ppwm <pat>)} -- <pat> is pattern (in LHS condition form),
prints all wme's that match <pat>.
No variables, predicates or special characters are allowed in in <pat>.
If pattern is null, all elements are printed.

use with @b[cs] and @b[matches]
to determine why a rule failed to be instantiated at the right time.

@Center[@b[The PM Command]]

@t{(pm <args>)} -- <args> any number of rule names

@Center[@b[The CS Command]]

@t{(cs)} -- lists each instantiated rule in conflict set, one to a
line, followed by currently dominant instantiation (that is, the one to
be fired on next cycle)

@Center[@b[The MATCHES Command]]

@t{(matches <rules>)} -- prints partial matches for rules whose names are
arguments.  For each condition element of specified rules, time tags of
matching wme's are listed, as well as intersections of partial matches.
@Begin[Verbatim]

	(literalize number value)

	(p example-rule
	   (number ^value { <number-1> > 100 } )
	   (number ^value { <number-2> <> <number-1> } )
	   (number ^value { <number-3> < 50 } )
	   -->
	   (write (crlf) <number-1> <number-2> <number-3> ) )

	(make number ^value 101)  ; given time-tag 1

	(make number ^value 102)  ; given time-tag 2

	(make number ^value  11)  ; given time-tag 3
@End[Verbatim]

@Begin[Verbatim]
	=>(matches example-rule)
	
	example-rule
	 ** matches for (1) **
	 2
	 1
	 ** matches for (2) **
	 3
	 2
	 1
	 ** matches for (2 1) **
	 3  1
	 3  2
	 1  2
	 2  1
	 ** matches for (3)
	 3
	nil
@End[Verbatim]
The final intersection, which in this example would be @t{matches for (3 2 1)},
is not included.

Uses:
@Begin[itemize]
a given condition element is never matched,

the intersection of two or more condition elements, each of which is matched,
fails to be satisfied, or

a negated condition element is matched.
@End[itemize]

@Center[@b[The PBREAK Command]]

@Begin[Itemize]
@t{(pbreak <rules>)} -- toggles break/nobreak status of rules

@t{(pbreak)} -- says which rules are broken

breaks after rule fires
@End[Itemize]

@Center[@b[The BACK Command]]

@Begin[Itemize]
@t{(back <n>)} undoes the effects of up to 32 rule firings, provided
there are no external references (user-defined functions) in any RHS
@End[Itemize]

@Center[@b[The MAKE and REMOVE Commands]]

@Begin[Itemize]
@t{(remove *)} deletes everything from working memory.

@t{(remove <args>)} deletes working memory elements with time tags in <args>
@End[Itemize]

@Center[@b[The EXCISE Command]]

@t{(excise <rules>)} --  prevents rules from firing (still in network),
reload to recall, but won't be current on wm.



