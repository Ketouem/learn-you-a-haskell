-- Making our own modules

-- When we import a module, we can use the functions that it exports. A module
-- can also defines functions that it uses internally, but we can see and only
-- use only the ones that it exports

-- A geometry modules

-- At the beggining of the module we set the module name and the name of the
-- exported functions

module Geometry
( sphereVolume,
  sphereArea,
  cubeVolume,
  cubeArea,
  cuboidArea,
  cuboidVolume ) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

-- Not exported because we only want to deal with 3D objects
rectArea :: Float -> Float -> Float
rectArea a b = a * b

-- To use this module `import Geometry` but the caller must be in the same
-- folder as the callee