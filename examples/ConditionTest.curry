
main = do
  x <- if 2>0 then return (1+1) else return 42
  print (x*x)
