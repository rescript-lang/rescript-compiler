

This is a list of reserved attributes, we may use such information
to detect unused attributes in the future


get
set
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

optional -- done
uncurry -- done
    note [@uncurry 3] is okay but rarely used
    maybe we should remove it
unwrap -- done
string -- done
int -- done
ignore -- done
as  -- done
meth -- done
this -- done
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