

This is a list of reserved attributes, we may use such information
to detect unused attributes in the future


get
set
this
meth
string
int
ignore
unwrap
uncurry
as
optional
get_index
return

obj

val
module
scope

variadic
send

new
set_index

send.pipe -- deprecated, no short-cut
splice -- deprecated, use variadic
config -- done
open -- not decided
inline -- done
    this would conflict exist one or not?
    invalid payload in bs.inline
    seems that we need loose the check
deriving -- done
raw -- not needed
re -- not needed
external -- not needed
time   -- not needed
node   -- not needed
debugger -- not needed
debugger.chrome -- deprecated

keywords is okay `[@open]` is a valid syntax

mark_used_bs_attribute - 
may mark attributes including `inline`