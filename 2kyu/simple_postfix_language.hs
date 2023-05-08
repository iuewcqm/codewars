-- https://www.codewars.com/kata/55a4de202949dca9bd000088

begin fn         = fn []
push stack n  fn = fn $ n:stack
add  (x:y:xs) fn = fn $ (x+y):xs
end  stack       = head stack

main = do
  print $ begin push 4 end -- 4
  print $ begin push 2 push 3 add end -- 5
  print $ begin push 1 push 2 add push 3 add end -- 6
