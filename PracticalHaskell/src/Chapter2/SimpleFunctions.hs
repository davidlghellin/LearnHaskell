module Chapter2.SimpleFunctions where


firstOrEmpty lst = if not (null lst) then head lst else "empty"

firstOrEmpty2 :: [[Char]] -> [Char]
firstOrEmpty2 lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
list1 +++ list2 = if null list1
                 then list2
                 else (head list1) : (tail list1 +++ list2)

reverse2 :: [a] -> [a]
reverse2 lista = if null lista
                 then []
                 else reverse2 (tail lista) +++ [head lista]


maxmin lista = if null (tail lista)
               then (head lista, head lista)
               else (if (head lista) > fst (maxmin (tail lista))
                     then head lista
                     else fst ( maxmin (tail lista))
                     ,
                     if (head lista) < snd (maxmin (tail lista))
                     then head lista
                     else snd (maxmin (tail lista))
                    ) 
maxmin2 lista = let h = head lista
                in if null (tail lista)
                   then (h,h)
                   else ( if h > t_max then h else t_max
                        ,
                        if h < t_min then h else t_min )
                        where t = maxmin2 (tail lista)
                              t_max = fst t
                              t_min = snd t