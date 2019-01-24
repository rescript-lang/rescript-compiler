type foo = int;;

class o =
  object(this)
    method x : foo = 10
    method y : int = this # x
  end;;


class o =
  object(this)
    method x : foo = 10
    method y = (this # x : int)
  end;;



class o =
  object(this)
    method x : int = (10 : int)
    method y = (this # x : foo)
  end;;
