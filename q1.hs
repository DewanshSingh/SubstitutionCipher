import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import System.IO
import Data.List

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)


tupleToList :: [(a,b)] -> [a]
tupleToList [] = []
tupleToList ((a,b):xs) = a : tupleToList xs

tupleToList2 :: [(a,b)] -> [b]
tupleToList2 [] = []
tupleToList2 ((a,b):xs) = b : tupleToList2 xs

indexof :: Eq a => a -> [a] -> Int
indexof y [] = 0
indexof y xs =  head [p | (p,q) <- zip [0..] xs , q == y]

elem' :: Eq a => a -> [a] -> Int
elem' _ [] = 0
elem' x (y : ys) = if (x == y) then 1 else elem' x ys

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ps | x <- xs, ps <- perms (xs\\[x])]


ans1 :: [Char] -> [Char] -> [Char] -> [Char] -> [Char] -> [Char]
ans1 contents m n w z = [t |  j<-[0..(length contents)-1], let i = if (((contents !! j) `elem` ['A'..'Z']) == True) then (indexof (contents !! j) m)
                                                                   else if (((contents !! j) `elem` ['a'..'z']) == True) then (indexof (contents !! j) n)
                                                                   else 0,
                                                                   let t =
                                                                           {-else if ((contents !! j) == 'b') then 'h'
                                                                           else if ((contents !! j) == 'd') then 'e'
                                                                           else if ((contents !! j) == 'x') then 'a'
                                                                           else if ((contents !! j) == 'f') then 'n'
                                                                           else if ((contents !! j) == 'j') then 'd'-}
                                                                           if (((contents !! j) `elem` ['A'..'Z']) == True) then (w !! i)
                                                                           else if (((contents !! j) `elem` ['a'..'z']) == True) then (z !! i)
                                                                           else (contents !! j) ]
isindict :: [[Char]] -> [[Char]] -> [[Char]]
isindict answer dict =  [ x | x<-answer, (x `elem` dict) == True ]


ans :: [Char] -> [Char] -> [Char] -> [Char]
ans contents out out2 = [t |  j<-[0..(length contents)-1], let i = if (((contents !! j) `elem` out) == True) then (indexof (contents !! j) out)
                                                                   else 0,
                                                                   let t =
                                                                           if (((contents !! j) `elem` out) == True) then (out2 !! i)
                                                                           else (contents !! j) ]

search1 :: (Eq a) => a -> [(a,b)] -> [b]
search1 x = map snd . filter ((==x).fst)

search :: Char -> [(Char,Char)] -> Char
search _ [] = '0'
search x ((a,b):xs) = if x == b then a else search x xs

replace :: Char -> Char -> [Char] -> [Char]
replace y z [] = []
replace y z (x:xs)
  | x==y           = z:replace y z xs
  | otherwise      = x:replace y z xs

moveon :: Char -> Char -> [[Char]] -> [[Char]] -> [[Char]] -> Int
moveon elem1 elem2 wordcont answer2 dict = length $ isindict [t | x<-wordcont, (elem1 `elem` x)==True,let i=(indexof x wordcont), let t = (replace elem1 elem2 (answer2!!i))] dict


lol :: Int->Int->Int->[Char]->[Char]->[[Char]]->[[Char]]->[[Char]]->Int
lol i j x remainingelem remainingelem2 wordcont answer2 dict = if (x < 5)&&(x /= (-1))then (moveon (remainingelem !! i) (remainingelem2 !! j) wordcont answer2 dict)
                                                                            else (-1)

lol2 :: Int->Int->[Char]->[Char]->[[Char]]->[[Char]]->[[Char]]->[(Int,Int)]
lol2 i x remainingelem remainingelem2 wordcont answer2 dict = [(c,t) | j<-[0..(length(remainingelem2))-1],let c=j, let t = lol i j x remainingelem remainingelem2 wordcont answer2 dict]


main = do
    contents <- readFile "newCipher.txt"
    dt <- readFile "dict.txt"
    let dict = (words (dt))
    --putStr contents
    let chars = removeDuplicates contents
    --print( chars )
    let y = mySort [ (x,c) | x<-['A'..'z'], let c = (length.filter (==x)) contents, c>=0 ]
    --print ( y )
    let u = (tupleToList y)
    let m = [x | x<-u, (x `elem` ['A'..'Z'])==True]
    let n = [x | x<-u, (x `elem` ['a'..'z'])==True]
    --print( u )
    --print( m )
    --print( n )
    let z = "etoniarshdlugcywfmbpvkxjqz"
    let w = "ETONIARSHDLUGCYWFMBPVKXJQZ"

    let wordans = ( words (contents) )

    let the1 = [t | x<-wordans,length x == 1,t<-['A'..'z'], x==[t]]
    --print (  the )
    let countthe1 = mySort [ (x,c) | x<-the1, let c = (length.filter (==x)) the1, c>=0 ]
    let thes1 = removeDuplicates countthe1
    --print ( thes1 )
    let aone = (tupleToList thes1)
    --a,I
    let answerone1
                  | ((head(aone) `elem` ['a'..'z']) == True) = (head(aone),'a')
                  | ((head(aone) `elem` ['A'..'Z']) == True) = (head(aone),'I')
    let answerone2
                    | ((head(tail(aone)) `elem` ['a'..'z']) == True) = (head(tail(aone)),'a')
                    | ((head(tail(aone)) `elem` ['A'..'Z']) == True) = (head(tail(aone)),'I')
    --print ( fst (answerone1) )
    --print ( fst (answerone2) )
    let the = [x | x<-wordans, length x == 2]
    --print (  the )
    let countthe = mySort [ (x,c) | x<-the, let c = (length.filter (==x)) the, c>=0 ]
    let thes = removeDuplicates countthe
    let two = (tupleToList thes)
    --print(two)
    let the = [x | x<-wordans, length x == 3]
    --print (  the )
    let countthe = mySort [ (x,c) | x<-the, let c = (length.filter (==x)) the, c>=0 ]
    let thes = removeDuplicates countthe
    let three = (tupleToList thes)
    --print(three)
    --print ( ((three)!!0)!!0 )
    --and,the
    let  cone
            | (((three)!!0)!!0 == fst (answerone1)) || (((three)!!0)!!0 == fst (answerone2)) = [(((three)!!0)!!0,'a'),(((three)!!0)!!1,'n'),(((three)!!0)!!2,'d')]
            | otherwise = [(((three)!!0)!!0,'t'),(((three)!!0)!!1,'h'),(((three)!!0)!!2,'e')]
    let  done
            | (((three)!!0)!!0 == fst (answerone1)) || (((three)!!0)!!0 == fst (answerone2)) = [(((three)!!1)!!0,'t'),(((three)!!1)!!1,'h'),(((three)!!1)!!2,'e')]
            | otherwise = [(((three)!!1)!!0,'a'),(((three)!!1)!!1,'n'),(((three)!!1)!!2,'d')]
    let output = cone++done
    --are
    let first = search 'a' output
    let second = search 'e' output
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!0 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!1) else '\0'
    let output1 = if (third /= '\0') then (output ++ [(third,'r')]) else (output)
    --print(output1)
    --not
    let first = search 't' output1
    let second = search 'n' output1
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!2 == first, ((three)!!j)!!0 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!1) else '\0'
    let output2 = if (third /= '\0') then (output1 ++ [(third,'o')]) else (output1)
    --print(output2)
    --in
    let first = search 'o' output2
    let second = search 'n' output2
    let seco = search 'a' output2
    let thirdlist = [t | j<-[0..(length(two)-1)], ((two)!!j)!!0 /= first, ((two)!!j)!!1 == second, ((two)!!j)!!0 /= seco, let t = (two!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output3 = if (third /= '\0') then (output2 ++ [(third,'i')]) else (output2)
    --print(output3)
    --is
    let first = search 'i' output3
    let second = search 't' output3
    let seco = search 'n' output3
    let thirdlist = [t | j<-[0..(length(two)-1)], ((two)!!j)!!0 == first, ((two)!!j)!!1 /= second, ((two)!!j)!!1 /= seco, let t = (two!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!1) else '\0'
    let output4 = if (third /= '\0') then (output3 ++ [(third,'s')]) else (output3)
    --print(output4)
    --for
    let first = search 'o' output4
    let second = search 'r' output4
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output5 = if (third /= '\0') then (output4 ++ [(third,'f')]) else (output4)
    --print(output5)
    --get
    let first = search 'e' output5
    let second = search 't' output5
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output6 = if (third /= '\0') then (output5 ++ [(third,'g')]) else (output5)
    --print(output6)
    --can
    let first = search 'a' output6
    let second = search 'n' output6
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output7 = if (third /= '\0') then (output6 ++ [(third,'c')]) else (output6)
    --print(output7)
    --any
    let first = search 'a' output7
    let second = search 'n' output7
    let seco = search 'd' output7
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!0 == first, ((three)!!j)!!1 == second, ((three)!!j)!!2 /= seco , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!2) else '\0'
    let output8 = if (third /= '\0') then (output7 ++ [(third,'y')]) else (output7)
    --print(output8)
    --you
    let first = search 'y' output8
    let second = search 'o' output8
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!0 == first, ((three)!!j)!!1 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!2) else '\0'
    let output9 = if (third /= '\0') then (output8 ++ [(third,'u')]) else (output8)
    --print(output9)
    --all
    let first = search 'a' output9
    --let second = search '' output9
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!0 == first, ((three)!!j)!!1 == ((three)!!j)!!2 , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!2) else '\0'
    let output10 = if (third /= '\0') then (output9 ++ [(third,'l')]) else (output9)
    --print(output10)
    --was
    let first = search 'a' output10
    let second = search 's' output10
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output11 = if (third /= '\0') then (output10 ++ [(third,'w')]) else (output10)
    --print(output11)
    --but
    let first = search 'u' output11
    let second = search 't' output11
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second , let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output12 = if (third /= '\0') then (output11 ++ [(third,'b')]) else (output11)
    --print(output12)
    --my
    let first = search 'b' output12
    let second = search 'y' output12
    --let seco = search 'n' output12
    let thirdlist = [t | j<-[0..(length(two)-1)], ((two)!!j)!!0 /= first, ((two)!!j)!!1 == second, let t = (two!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output13 = if (third /= '\0') then (output12 ++ [(third,'m')]) else (output12)
    --print(length(output13))
    --up
    let first = search 'u' output13
    let second = search 's' output13
    --let seco = search 'n' output13
    let thirdlist = [t | j<-[0..(length(two)-1)], ((two)!!j)!!0 == first, ((two)!!j)!!1 /= second, let t = (two!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!1) else '\0'
    let output14 = if (third /= '\0') then (output13 ++ [(third,'p')]) else (output13)
    --print(output14)
    --The
    let first = search 'h' output14
    let second = search 'e' output14
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second ,((((three)!!j)!!0 `elem` ['A'..'Z'] )==True), let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output15 = if (third /= '\0') then (output14 ++ [(third,'T')]) else (output14)
    --print(output15)
    --And
    let first = search 'n' output15
    let second = search 'd' output15
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second ,((((three)!!j)!!0 `elem` ['A'..'Z'] )==True), let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output16 = if (third /= '\0') then (output15 ++ [(third,'A')]) else (output15)
    --print(output16)
    --For
    let first = search 'o' output16
    let second = search 'r' output16
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second ,((((three)!!j)!!0 `elem` ['A'..'Z'] )==True), let t = (three!!j)]
    --if (thirdlist/=[]) then print(thirdlist) else print(output16)
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output17 = if (third /= '\0') then (output16 ++ [(third,'F')]) else (output16)
    --print(output17)
    --But
    let first = search 'u' output17
    let second = search 't' output17
    let thirdlist = [t | j<-[0..(length(three)-1)], ((three)!!j)!!1 == first, ((three)!!j)!!2 == second ,((((three)!!j)!!0 `elem` ['A'..'Z'] )==True), let t = (three!!j)]
    let third = if (thirdlist/=[]) then ((thirdlist!!0)!!0) else '\0'
    let output18 = if (third /= '\0') then (output17 ++ [(third,'B')]) else (output17)
    --print(output18)
    let out = tupleToList(output18) ++ ['Z']
    let out2 = tupleToList2(output18) ++ ['I']
    --print(out)
    --print(out2)
    let contents2 = ans contents out out2
    --print(contents2)
    let wordcont = words (contents)
    let answer2 = words (contents2)
    let o =  isindict answer2 dict
    --print(length(o))
    --print(length(answer2))
    let remainingelem = [t | j<-['a'..'z'], (j `elem` out)==False, let t=j ]
    let remainingelem2 = [t | j<-['a'..'z'], (j `elem` out2)==False, let t=j ]
    --print(remainingelem2)
    --print(elem1)
    --let elcont = [t | x<-wordcont, ((remainingelem !! 0) `elem` x)==True,let i=(indexof x wordcont), let t = (replace (remainingelem !! 0) (remainingelem2 !! 0) (answer2!!i))]
    --print(elcont)
    {-let mo = (moveon (remainingelem !! 0) (remainingelem2 !! 0) wordcont answer2 dict)
    let mo2 = if (mo < 5)&&(mo /= (-1))then (moveon (remainingelem !! 0) (remainingelem2 !! 1) wordcont answer2 dict) else (-1)
    let mo3 = if (mo2 < 5)&&(mo2 /= (-1))then (moveon (remainingelem !! 0) (remainingelem2 !! 2) wordcont answer2 dict) else(-1)
    let mo4 = if (mo3 < 5)&&(mo3 /= (-1))then (moveon (remainingelem !! 0) (remainingelem2 !! 3) wordcont answer2 dict) else (-1)
    let mo5 = if (mo4 < 5)&&(mo4 /= (-1))then (moveon (remainingelem !! 0) (remainingelem2 !! 4) wordcont answer2 dict) else (-1)
    let mo6 = if ((mo5 < 5)&&(mo5 /= (-1))) then (moveon (remainingelem !! 0) (remainingelem2 !! 5) wordcont answer2 dict) else (-1)
    let m = indexof (maximum [mo,mo2,mo3,mo4,mo5,mo6]) [mo,mo2,mo3,mo4,mo5,mo6]
    --let mo = moveon (remainingelem !! 0) (remainingelem2 !! 1) wordcont answer2 dict
    --print(elem1)
    --let elcont = length $ isindict [t | x<-wordcont, (elem1 `elem` x)==True,let i=(indexof x wordcont), let t = (replace (remainingelem !! 0) (remainingelem2 !! 0) (answer2!!i))] dict
    --print(m)-}
    let mlist = [(t,c) | i<-[0..length(remainingelem)-1], let t=i, let c = lol2 i 0 remainingelem remainingelem2 wordcont answer2 dict]
    --print(mlist)
    let mlist2 = tupleToList2 mlist
    --print(mlist2)
    let mlist3 = [t | x<-mlist2, let t = mySort x]
    --print(mlist3)
    let mlist4 = [t | i<-[0..length(remainingelem)-1], let t = (mlist3!!i)!!0]
    --print(mlist4)
    let mlist5 = tupleToList mlist4
    --print(mlist5)
    let something = [t | i<-mlist5, let t = remainingelem2 !! i]
    --print(something)
    let corr = out ++ remainingelem
    let corr2 = out2 ++ something
    let contents2 = ans contents corr corr2
    print(contents2)
    let wordcont = words (contents)
    let answer2 = words (contents2)
    let o =  isindict answer2 dict
    print(length(o))
    print(length(answer2))
    --print(o)



    {-let answer = ans contents m n w z
    let answer2 = words (answer)
    print( answer2 )
    let o =  isindict answer2 dict
    --let o = perms [1,2,3,4]
    print (o)
    print ((length(o)) `quot` (length(answer2)))-}
