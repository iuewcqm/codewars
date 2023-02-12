data User = User 
  { rank :: Int
  , progress :: Int
  } deriving Show

newUser :: User
newUser = User (-8) 0

incProgress :: Int -> User -> User
incProgress kata_rank (User r p) = updateUser r (p + calcProgress (calcDiff kata_rank r))
  where updateUser rank progress
          | rank == 8       = User 8 0
          | rank == 0       = updateUser 1 (progress)
          | progress >= 100 = updateUser (rank + 1) (progress - 100)
          | otherwise       = User rank progress
        calcProgress rank_diff
          | rank_diff == -1 = 1
          | rank_diff ==  0 = 3
          | otherwise       = 10*rank_diff*rank_diff
        calcDiff kata_rank user_rank
          | kata_rank < -8|| kata_rank > 8 || kata_rank == 0 = error "invalid kata rank"
          | kata_rank < 0 && user_rank > 0 = kata_rank-user_rank+1
          | kata_rank > 0 && user_rank < 0 = kata_rank-user_rank-1 
          | otherwise                      = kata_rank-user_rank

-- -- maybe more clear solution
-- data User = User { rank :: Int, progress :: Int } deriving Show
-- newUser = User (-8) 0
-- 
-- incProgress k (User r p) = updateUser r (p + calcProgress (calcDiff k r))
--   where updateUser rank progress
--           | rank == 8       = User 8 0
--           | rank == 0       = updateUser 1 progress
--           | progress >= 100 = updateUser (rank + 1) (progress - 100)
--           | otherwise       = User rank progress
--         calcProgress rank_diff
--           | rank_diff == -1 = 1
--           | rank_diff ==  0 = 3
--           | otherwise       = 10*rank_diff*rank_diff
--         calcDiff kata_rank user_rank
--           | kata_rank < -8|| kata_rank > 8 || kata_rank == 0 = error "invalid input"
--           | kata_rank < 0 && user_rank > 0 = kata_rank-user_rank+1
--           | kata_rank > 0 && user_rank < 0 = kata_rank-user_rank-1
--           | otherwise                      = kata_rank-user_rank


main :: IO()
main = do
  print $ newUser
  print $ rank newUser -- -8
  print $ progress newUser -- 0
  let u2 = incProgress (-7) newUser
  print $ progress u2 -- 10
  let u3 = incProgress (-5) u2 -- add 90 progress
  print $ progress u3 -- 0
  print $ rank u3 -- -7
  let user = incProgress (-1) (User (1) 20)
  print $ '\0'
  print $ rank user
  print $ progress user

  let user = incProgress 9 newUser
  print $ rank user
