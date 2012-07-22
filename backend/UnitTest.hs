import CollisionDetection

main = do
   let tree = buildTree (Rectangle (-1.0) 1.0 1.0 (-1.0)) in
	let tree2 = addSegment tree (Segment (Point 0.5 0.5) (Point 0.7 0.2)) in
		if not (segTreeIntersection (Segment (Point 0.4 0.2) (Point 0.8 0.6)) tree2) then  error "Nie zadzialal test 1"
		else if segTreeIntersection (Segment (Point 0.8 0.6) (Point 0.9 0.9)) tree2 then error "nie zadzialal test 2"
		else if segTreeIntersection (Segment (Point 0.5 0.5) (Point 0.8 0.6)) tree2 then error "Nie zadziala test 3"
		else putStrLn "OK!"
