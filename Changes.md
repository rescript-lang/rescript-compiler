


# Next

Fixes:
- #1676, `bsb -w` will always build regardless of filetype when fs.watch doesn't send a filename
- #1655, fix #1653 Js.Promise.all[n] interfaces
- #1658, fix typeof = "null" issue
- #1656, bs.get/set/get_index/set_index respects bs.ignore
- #1654, `bsb -init` fails if package or current dir has space (parent dir can have spaces)
- #1678, bs.get{null;undefined}  in object type
- #1692, fix invalid js syntax output
- #1701, fix tailcall handling interaction with  exception handler
Features:
- #1648, exposed `bsc` in the npm environment
- #1647, speical handling `bsb -init .` to reuse current directory
- #1667, fix an optimizaiton bug 

# 1.7.4(May 24, 2017):



internal tools:

- #1583, add -U -D support for bspack

Features:

- #1630, add modules Option, Result, List, and Vector into Js namesapace, update docs

- #1613, allow bs.scope with bs.send/bs.send.pipe/bs.set/bs.get/bs.set_index/bs.get_index
- #1604, add the functions `entries`, `values`, `fromList`, `fromArray` and `map` to `Js.Dict`

- #1632, bsb themes support

Bug fixes :

- #1581, more error checking
- #1633, fix missing installations
- #1581, more error checking %identity

# 1.7.3:

Bug fixes:

- #1556, fix duplicated requires of runtime (report by Chenglou) 

- #1568, internal compiler error

Features:

- #1564: scoped values in FF, see `bs.scope` in the Manual



