module A = struct exception E end
module B = struct exception F = A.E end

exception H = Exception_def.A
