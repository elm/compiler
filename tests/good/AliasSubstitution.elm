
type Vec2Ext a = { a | x:Float, y:Float }
type Vec2 = Vec2Ext {}

extractVec : Vec2Ext a -> Vec2
extractVec v = { x = v.x, y = v.y }
