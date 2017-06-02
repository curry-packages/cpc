import Test.Prop

rev :: [Int] -> [Int]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- Property: reversing two times is the identity:
revRevIsId xs = rev (rev xs) -=- xs

