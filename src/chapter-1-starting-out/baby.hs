doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
						then x
						else x*2
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [x | x <- st, x `elem` ['A', 'Z']]
triangles = [ (x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10] ]
rightTriangles = [ (x,y,z) | z <- [1..10], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]
rightTriangles' = [ (x,y,z) | z <- [1..10], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2, x+y+z==24]
pythagoreanTriples = [ (x,y,z) | z <- [1..10], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2, gcd x y == 1, gcd x z == 1, gcd y z == 1]