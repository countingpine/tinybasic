'' compile from main folder with: ./tbc -o hello examples/hello.bas
function main(byval arg_c as integer, byval arg_v as zstring ptr ptr) as integer
  print "Hello world!"
  return 0
end function
