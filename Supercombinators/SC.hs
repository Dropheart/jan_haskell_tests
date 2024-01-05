module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _) = True 
isFun _ = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs d = ([x | x <- d, isFun (snd x)], [y | y <- d, not (isFun (snd y))]) 


topLevelFunctions :: Exp -> Int
topLevelFunctions (Let b _) = length (filter isFun (map snd b))
topLevelFunctions _ = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union [] 

freeVars :: Exp -> [Id]
freeVars (Const _) = []
freeVars (Var i)
  | elem i prims = []
  | otherwise = [i]
freeVars (App e e') = union (freeVars e) (unionAll (map freeVars e'))
freeVars (Fun i e) = (freeVars e) \\ i
freeVars (Let b e) = (unionAll (map (freeVars . snd) b)) \\ (freeVars e)

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Fun i e) = buildFVMap e
buildFVMap (App e es) = (buildFVMap e) ++ unionAll (map buildFVMap es)
buildFVMap (Let b e) 
  = [(f', (union fv' (freeVars v')) \\ fv) | (f', v') <- f ] 
  ++ (unionAll (map (buildFVMap . snd) b))
  ++ (buildFVMap e)
  where 
    (f, v) = splitDefs b
    fv = map fst f
    fv' = unionAll (map (freeVars . snd) f)
buildFVMap _ = []



modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions m (Let bs e) = Let (replacer m bs) (modifyFunctions m e)
  where 
    replacer :: [(Id, [Id])] -> [Binding] -> [Binding]
    replacer _ [] = []
    replacer m ((f, Fun as e):bs)
      | elem f (map fst m) = ('$':f, Fun (as ++ (lookUp f m)) (modifyFunctions m e)):(replacer m bs)
    replacer m (b:bs) = b:(replacer m bs)
    -- replacer m _ = []
modifyFunctions m f'@(Var f)
  | elem f (map fst m) && (lookUp f m /= []) = (App (Var ('$':f)) (map Var (lookUp f m)))
  | elem f (map fst m) = Var ('$':f)
  | otherwise = f'
modifyFunctions m c'@(Const a) = c'
modifyFunctions m (App e e') = App (modifyFunctions m e) (map (modifyFunctions m) e')


-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' 
  = undefined
