import Debug.Trace;

data Rectangle = Rectangle { left :: Float
			   , right :: Float
  			   , top :: Float
 			   , down :: Float
			   } deriving (Show, Eq)

data Point = Point Float Float deriving (Show, Eq)

data Segment = Segment { first :: Point
                       , second :: Point
                       } deriving (Show, Eq)

segIntersection :: Segment -> Segment -> Bool
segIntersection (Segment (Point x1 y1) (Point x2 y2)) (Segment (Point x3 y3) (Point x4 y4)) =
		((x1-x2)*(y3-y2)-(y1-y2)*(x3-x4))*((x1-x2)*(y4-y2)-(y1-y2)*(x4-x2))<0 &&
		((x1-x3)*(y4-y3)-(y1-y3)*(x4-x3))*((x2-x3)*(y4-y3)-(y2-y3)*(x4-x3))<0 

data Tree = Leaf [Segment] Rectangle | Node Tree Tree Tree Tree Rectangle deriving Show

segTreeIntersection :: Segment -> Tree -> Bool
segTreeIntersection s (Leaf [] r) = False
segTreeIntersection s (Leaf (l:ls) r) =
   			if (segRectIntersection s r) then  
				(segIntersection s l) || (segTreeIntersection s (Leaf ls r))
			else False
segTreeIntersection s (Node t1 t2 t3 t4 r) = 
			if (segRectIntersection s r) then
				(segTreeIntersection s t1) || (segTreeIntersection s t2) || (segTreeIntersection s t3) || (segTreeIntersection s t4)
			else False


addSegment :: Tree -> Segment -> Tree
addSegment (Leaf l r) s = if segRectIntersection s r then 
				trace "Dodalem" (Leaf (s:l) r)
			else trace "nie dodalem" (Leaf l r) 
addSegment (Node t1 t2 t3 t4 r) s = trace "addSegment" (
			if segRectIntersection s r then
				(Node (addSegment t1 s) (addSegment t2 s) (addSegment t3 s) (addSegment t4 s) r)
			else (Node t1 t2 t3 t4 r))

buildTree :: Rectangle -> Tree
buildTree r = if ((right r) - (left r)) < 0.5 then trace "Test" (Leaf [] r)
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
	else let (s1, s2, s3, s4) = (rectToSegs rect) in ((segIntersection seg s1) || (segIntersection seg s2) || (segIntersection seg s3) || (segIntersection seg s4)) 
  	
isPointInRect :: Point -> Rectangle -> Bool
isPointInRect (Point x y) (Rectangle l r t d) = (x>l && x<r && y<t && y>d) 

rectToSegs :: Rectangle -> (Segment, Segment, Segment, Segment)
rectToSegs (Rectangle l r t d) =
	((Segment (Point l t) (Point l d)), (Segment (Point l d) (Point r d)), (Segment (Point r d) (Point r t)), (Segment (Point l t) (Point r t))) 

  
