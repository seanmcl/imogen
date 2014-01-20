
structure Result = struct
   datatype ('a, 'b) t = Ok of 'a | Error of 'b
end
