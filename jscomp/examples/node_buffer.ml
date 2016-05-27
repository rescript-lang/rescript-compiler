
type encoding = string (* [hex], [base64],[utf8]*)

class type buffer = object[@uncurry]
  method toString : encoding -> string 
end

external make : int -> buffer = "Buffer" [@@bs.new]



external make_with_encoding : int -> encoding -> buffer = "Buffer" [@@bs.new]
