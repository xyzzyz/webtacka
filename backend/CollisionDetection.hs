module CollisionDetection where
import Debug.Trace

data Rectangle = Rectangle { left :: Float
			   , right :: Float
  			   , top :: Float
 			   , down :: Float
			   } deriving (Show, Eq)

data Point = Point Float Float deriving (Show, Eq)

pminus :: Point -> Point -> Point
pminus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

pmul :: Point -> Point -> Float
pmul (Point x1 y1) (Point x2 y2) = x1*y2 - x2*y1

myIntersection :: Segment -> Segment -> Bool
myIntersection (Segment p1 p2) (Segment p3 p4) = 
	(mycheck p1 p2 p3 p4) && (mycheck p3 p4 p1 p2)

mycheck :: Point -> Point -> Point -> Point -> Bool
mycheck p1 p2 p3 p4 = let 
       v1 = p2 `pminus` p1
       v2 = p3 `pminus` p1
       v3 = p4 `pminus` p1
  in check (v1 `pmul` v2) (v1 `pmul` v3)

data Segment = Segment { first :: Point
                       , second :: Point
                       } deriving (Show, Eq)

eps = 0.0000


check l1 l2 = if abs(l1) > eps && abs(l2) > eps then l1*l2 < 0
	else False

data Tree = Leaf [Segment] Rectangle | Node Tree Tree Tree Tree Rectangle deriving Show

anny :: [Maybe a] -> Maybe a
anny [] = Nothing
anny ((Just x):_) = Just x
anny (Nothing:t) = anny t

segTreeIntersection :: Segment -> Tree -> Maybe Segment
segTreeIntersection s (Leaf [] r) = Nothing
segTreeIntersection s (Leaf (l:ls) r) =
   			if (segRectIntersection s r) then  
				(if (myIntersection s l) then Just l
				else segTreeIntersection s (Leaf ls r))
			else Nothing
segTreeIntersection s (Node t1 t2 t3 t4 r) = 
			if (segRectIntersection s r) then
				anny [(segTreeIntersection s t1),(segTreeIntersection s t2), (segTreeIntersection s t3), (segTreeIntersection s t4)]
			else Nothing


addSegment :: Tree -> Segment -> Tree
addSegment (Leaf l r) s = if segRectIntersection s r then 
				(Leaf (s:l) r)
			else (Leaf l r)

addSegment (Node t1 t2 t3 t4 r) s =
			if segRectIntersection s r then
			 	(Node (addSegment t1 s) (addSegment t2 s) (addSegment t3 s) (addSegment t4 s) r)
			else (Node t1 t2 t3 t4 r)

buildTree :: Rectangle -> Tree
buildTree r = if ((right r) - (left r)) < 0.1 then (Leaf [] r)
	      else (Node (leftTop r) (rightTop r) (leftDown r) (rightDown r) r)

leftTop :: Rectangle -> Tree
leftTop (Rectangle l r t d) = 
	let rect = (Rectangle l ((r+l)/2) t ((d+t)/2)) 
		in buildTree rect

rightTop :: Rectangle -> Tree
rightTop (Rectangle l r t d) = 
	let rect = (Rectangle ((l+r)/2) r t ((d+t)/2)) 
		in buildTree rect

leftDown :: Rectangle -> Tree
leftDown (Rectangle l r t d) = 
	let rect = (Rectangle l ((r+l)/2) ((d+t)/2) d) 
		in buildTree rect

rightDown :: Rectangle -> Tree
rightDown (Rectangle l r t d) =
	let rect = (Rectangle ((l+r)/2) r ((d+t)/2) d)
		in buildTree rect

segRectIntersection :: Segment -> Rectangle -> Bool
segRectIntersection (seg @ (Segment f s)) (rect @ (Rectangle l r t d)) =
	if (isPointInRect f rect) then True
	else let (s1, s2, s3, s4) = (rectToSegs rect) in ((myIntersection seg s1) || (myIntersection seg s2) || (myIntersection seg s3) || (myIntersection seg s4)) 
  	
isPointInRect :: Point -> Rectangle -> Bool
isPointInRect (Point x y) (Rectangle l r t d) = (x>=l && x<=r && y<=t && y>=d) 

rectToSegs :: Rectangle -> (Segment, Segment, Segment, Segment)
rectToSegs (Rectangle l r t d) =
	((Segment (Point l t) (Point l d)), (Segment (Point l d) (Point r d)), (Segment (Point r d) (Point r t)), (Segment (Point l t) (Point r t))) 

  
