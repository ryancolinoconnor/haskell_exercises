import Data.List
-- exercise 1:


data List a = Cons a (List a)
            | Nil
            deriving (Show)
convfromList (Cons x xs) = [x] ++ (convfromList xs)
convfromList Nil  = []

-- exercise 2: 

data Maybe a = Just a
  | Nothing
data MaybeTree b = MaybeNode b (Prelude.Maybe (MaybeTree b)) (Prelude.Maybe (MaybeTree b))
 deriving (Show) 

 -- end of chapter
 -- exercise 1:
numElem:: [a] -> Int
numElem (x:xs) = 1 + numElem(xs)
numElem [] = 0

computeMean [] = 0
computeMean x = (sum x) / (fromIntegral $ numElem x)


palinDrome (x:xs) = [x] ++ (palinDrome xs) ++ [x]
palinDrome [] = []

-- learned about reverse
isPalindrome xs = xs == (reverse xs)


-- 6. 
sortLen = sortBy (\ a b -> compare (numElem a) (numElem b))
-- 7. 
myintersperse (x:xs) a| (numElem xs) >1 = x ++ a ++ (myintersperse xs a)
myintersperse (x:xs) a |(numElem xs) <=1 = x ++ a ++ head xs
-- ["foo","bar"] , -> "foo,bar"

-- 8. calculate the height of a binary tree
data Tree a = Node a (Tree a) (Tree a)
  | Empty
  deriving Show
max x y =  if x>=y
  then x
  else y
binary_height Empty = 0
binary_height (Node root left right) = 
  let left_height = binary_height left
      right_height = binary_height right
  in
     1 + (Prelude.max left_height right_height)


-- 9. a,b,c abc left right or straight Define Direction to represent
data Direction = Left|Right|Straight deriving (Show,Eq)
type Point = (Double, Double)
-- 10. function to calculate the turn
-- https://www.geeksforgeeks.org/direction-point-line-segment/
get_directions::(Point, Point,Point)->Direction
get_directions ((x1,y1),(x2,y2),(x3,y3)) =
  let cross_product =((x1-x2)*(y3-y2)) - ((y1-y2)*(x3-x2))
  in 
  if (cross_product>0)
  then Main.Right
  else if (cross_product<0)
    then Main.Left
  else Main.Straight
-- 11. Implement 10. but for a list of points
get_list_directions::[Point]->[Direction]
get_list_directions (a:b:c:points) = (get_directions (a, b, c)):get_list_directions(b:c:points)
-- 12. Implement Graham's scan algorithm for the convex hull of a set
--
sortBP (x1,y1) (x2,y2) = compare y1 y2 <> compare x1 x2
get_p0 (xs) = head (sortBy(\(x1,y1) (x2,y2) -> sortBP (x1,y1) (x2, y2)) xs)
pol_angle:: Point-> Point ->Double
square num = num * num
-- pol_angle (x1,y1) (x2,y2) = (x1-x2)/(sqrt ((square (x1-x2)) +(square (y1-y2))))
l1_norm (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)
pol_angle (x1,y1) (x2,y2) = (atan2 (y2-y1) (x2-x1))  * (180/pi)
sortPol (xb,yb) (x1,y1) (x2,y2) = 
  compare (pol_angle (xb,yb) (x1,y1))  (pol_angle (xb,yb) (x2,y2)) <> compare (l1_norm (xb,yb) (x2,y2)) (l1_norm (xb,yb) (x1,y1))  
sortbypol xs = let (xp,yp)= (get_p0 xs)
                     in sortBy(\(x1,y1) (x2,y2) ->sortPol (xp,yp) (x1,y1) (x2,y2)) xs
--grahams_scan
--follows pseudocode here: https://en.wikipedia.org/wiki/Graham_scan
--
ccw a b c = let dir = (get_directions (a, b, c))
  in case dir of
  Main.Straight-> 0
  Main.Left -> 1
  Main.Right -> -1
grahams_scan points = let p0= (get_p0 points) in
          let sorted_points = tail (sortbypol points)                            
          in grahams_scan_w_p0 (head sorted_points) (tail sorted_points) [p0] pi p0 True
grahams_scan_w_p0 :: Point -> [Point] -> [Point] -> Double -> Point -> Bool -> [Point]
grahams_scan_w_p0 point points stack last_angle p0 check| check && (last_angle ==pol_angle p0 point) = grahams_scan_w_p0 (head points) (tail points) stack (pol_angle p0 point) p0 True
-- if the stack is too short are ccw is indicating a CCW move we add to the stack and move on
grahams_scan_w_p0 point points stack last_angle p0 check| (length stack)<2||ccw (head (tail stack)) (head stack) point==1  =let new_stack=[point]++stack
                in 
                if ((length points)>0)
                   then grahams_scan_w_p0 (head points) (tail points) new_stack (pol_angle p0 point) p0 True
                else
                    new_stack
-- we are done
grahams_scan_w_p0 point points stack last_angle p0 check| (length points)==0 && ((length stack)<=2) =  [point]++stack
-- eval has failed and we pop from the stack
grahams_scan_w_p0 point points stack last_angle p0 check = grahams_scan_w_p0 point points (tail stack) (pol_angle p0 point) p0 False
-- test
-- input_points = [(0, 3), (1, 1), (2, 2), (4, 4),(0, 0), (1, 2), (3, 1), (3, 3)]
-- grahamas_scan input_points
-- out: [(0.0,3.0),(4.0,4.0),(3.0,1.0),(0.0,0.0)]
