-- COMMENTS EVERYWHERE FOR PERSONAL USE, SORRY

-- TERM = VAR (Var "x")
--      | (TERM TERM) application, is like function call (pass term2 into term1 => term1(term2))
--      | LAMBDA VAR TERM ("lambda expression", basically function definition where TERM is function and VAR is parameters => think of lambda functions from Scheme)

data Term = Var String | Application Term Term | Lambda String Term
    deriving Show


-- Find all free variables in a Term
freeVars :: Term -> [String]
freeVars (Var variable) = [variable]                                                     -- BASE CASE: Put variable in list and return it
freeVars (Application function argument) = freeVars function ++ freeVars argument        -- RECURSIVE CASE: Reduce each Term in Application to a list of free variables and append
freeVars (Lambda argument expression) = filter (/= argument) (freeVars expression)       -- RECURSIVE CASE: Variables found in Term that are bound as arguments aren't free, so remove them


-- Determine if a Term is in normal form
normalForm :: Term -> Bool
normalForm (Application (Lambda _ _) _) = False                                         -- BASE CASE: Left hand side of Application is lambda expression => not in normal form
normalForm (Var _) = True                                                               -- BASE CASE: Var isn't considered when evaluating normal form
normalForm (Lambda _ expression) = normalForm expression                                -- RECURSIVE CASE: Check if Term is in normal form
normalForm (Application function argument) = normalForm function && normalForm argument -- RECURSIVE CASE: Check both Terms to evaluate normal form


-- An infinite list of distinct variables that can be used if you want to find a fresh variable
listOInts :: [Int]
listOInts = 1 : map (+1) listOInts -- Infinite list of Int

listOVars :: [String]
listOVars = map show listOInts -- Convert infinite list of Int to infinite list of String


-- (substitute e x f) yields e except with f replacing all free occurrences of x in e
substitute :: Term -> String -> Term -> Term
substitute (Var t) formal x
    | t == formal = x
    | otherwise = (Var t)

substitute (Application t1 t2) formal x = (Application (substitute t1 formal x) (substitute t2 formal x)) -- RECURSIVE CASE: Handle Application (check both terms for substitution)

substitute (Lambda parameter expression) formal x                                                         -- Handle Lambda
    | parameter /= formal = (Lambda parameter (substitute expression formal x))                           -- RECURSIVE CASE: formal parameter still in scope, continue substitution
    | otherwise = (Lambda parameter expression)                                                           -- BASE CASE: formal parameter no longer in scope, stop substitution



-- Perform an alpha renaming
-- (alphaRename t x) substitutes x for the formal parameter of t, which must be a lambda expression
-- alphaSub does all substitution after first step

alphaSub :: Term -> String -> String -> Term    -- Takes in same as alphaRename but also tracks formal parameter
alphaSub (Var t) formal x        -- BASE CASE: Handle Var substitution
    | t == formal = (Var x)      -- CASE1: substitute formal parameter
    | otherwise = (Var t)        -- CASE2: don't substitute as t is not formal parameter

alphaSub (Application t1 t2) formal x = (Application (alphaSub t1 formal x) (alphaSub t2 formal x)) -- RECURSIVE CASE: Handle Application (check both terms for substitution)

alphaSub (Lambda parameter expression) formal x                                   -- Handle Lambda
    | parameter /= formal = (Lambda parameter (alphaSub expression formal x))     -- RECURSIVE CASE: formal parameter still in scope, continue substitution
    | otherwise = (Lambda parameter expression)                                   -- BASE CASE: formal parameter no longer in scope, stop substitution

alphaRename :: Term -> String -> Term                                                  -- Initial step of alpha renaming
alphaRename (Lambda formal (Lambda parameter expression)) x
    | formal /= parameter = (Lambda x (Lambda parameter (alphaSub expression formal x))) -- if formal doesn't go out of scope, continue renaming
    | otherwise = (Lambda x (Lambda parameter expression))                               -- if formal goes out of scope, stop renaming

alphaRename (Lambda formal expression) x = (Lambda x (alphaSub expression formal x))     -- replace formal parameter with x and do substitution



-- Perform a beta reduction
-- Beta reduction is function application
-- E.g. assuming some encoding of 2, 7, *, we have the following β-reduction: (λn.n * 2) 7 → 7 * 2
-- Can assume first argument is Lambda

findVar :: Term -> Term -> Int -> String                                                         -- get first variable from listOVars that isn't free in first or second parameter for alphaRename
findVar firstArg secondArg index
    | (not (elem (listOVars !! index) (freeVars firstArg))) && (not (elem (listOVars !! index) (freeVars secondArg))) = listOVars !! index
    | otherwise = findVar firstArg secondArg (index+1)


betaReduce :: Term -> Term -> Term                                                                    -- First step of beta reduction
betaReduce (Lambda parameter expression) secondArg                                                    -- if formal parameter of lambda is free in secondArg, must alpha rename lambda
    | (elem parameter (freeVars secondArg)) = betaReduce (alphaRename (Lambda parameter expression) (findVar (Lambda parameter expression) (secondArg) 0)) secondArg
    | otherwise = (substitute expression parameter secondArg)                                            -- Get rid of Lambda, substitute formal parameter with term



-- Put term in normal form (Case for Var not needed because normalForm should handle it)
normalizeWrapped :: Term -> Term
normalizeWrapped (Application (Lambda parameter expression) t) = betaReduce (Lambda parameter expression) t -- Do first possible reduction found (outermost) and return to check normal form
normalizeWrapped (Application t1 t2)
    | (not (normalForm t1)) = Application (normalizeWrapped t1) t2 -- also need wrapper here because t2 won't get normalised unless t1 already is
    | (not (normalForm t2)) = Application t1 (normalizeWrapped t2)
    | otherwise = Application t1 t2
normalizeWrapped (Lambda parameter expression) = Lambda parameter (normalizeWrapped expression)

normalize :: Term -> Term                               -- Wrapper for the normalization process
normalize t                                             -- In order to make sure that we're always doing the outermost/leftmost reduction, we do max 1 reduction before we return to this
    | normalForm t = t                                  -- If it's normalForm, return output
    | otherwise = normalize (normalizeWrapped t)        -- If it's not normalForm, find next leftmost/outermost reduction

normalizeWrapperCount :: Term -> Int -> Term
normalizeWrapperCount t count
    | count == 0 = t
    | otherwise = normalizeWrapperCount (normalizeWrapped t) (count-1)
    
-- (λ f x . x)
-- (LambdaS ["f", "x"] (VarS "x"))

-- ApplicationS e [e1,e2,e3] == e e1 e2 e3 = (((e e1) e2) e3)
-- (Application (Application (Var e) (Var e1)) (Var e2))

data TermS = VarS String | ApplicationS TermS [TermS] | LambdaS [String] TermS
    deriving Show

deSugar :: TermS -> Term

deSugar (VarS s) = Var s                                                                     -- Handle VarS

deSugar (ApplicationS e args)                                                                -- Handle ApplicationS (Curried)
    | (length args) == 0 = deSugar e                                                         -- BASE CASE: args is empty
    | (length args) == 1 = (Application (deSugar e) (deSugar (head args)))                   -- BASE CASE: final element of args
    | otherwise = (Application (deSugar (ApplicationS e (init args))) (deSugar (last args))) -- RECURSIVE CASE: nest deSugared ApplicationS

deSugar (LambdaS args expr)                                                                  -- Handle LambdaS
    | (length args) == 1 = (Lambda (head args) (deSugar expr))                               -- BASE CASE: final element of argList
    | otherwise = Lambda (head args) (deSugar (LambdaS (tail args) expr))                    -- RECURSIVE CASE: nest deSugared LambdaS


testCaseS :: TermS
testCaseS = ApplicationS (LambdaS ["t"] (ApplicationS (LambdaS ["m","n"] (ApplicationS (VarS "n") [ LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ VarS "f", ApplicationS (VarS "f") [ VarS "x" ] ] ), VarS "m" ] ) ) [ ApplicationS (LambdaS ["n"] (ApplicationS (LambdaS ["m","n", "f"] (ApplicationS (VarS "m") [ VarS "n", VarS "f" ] ) ) [ VarS "n", VarS "n" ] ) ) [ ApplicationS (LambdaS ["n"] (ApplicationS (LambdaS ["m","n", "f"] (ApplicationS (VarS "m") [ ApplicationS (VarS "n") [ (VarS "f") ] ] ) ) [ VarS "n", VarS "n" ] ) ) [ VarS "t" ] ], ApplicationS (LambdaS ["n"] (ApplicationS (LambdaS ["m","n", "f"] (ApplicationS (VarS "m") [ ApplicationS (VarS "n") [ VarS "f" ] ] ) ) [ VarS "n", VarS "n" ] ) ) [ VarS "t" ] ] ) ) [ ApplicationS (LambdaS ["m","n"] (ApplicationS (VarS "n") [ LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ VarS "f", ApplicationS (VarS "f") [ (VarS "x") ] ] ), VarS "m" ] ) ) [ ApplicationS (LambdaS ["m","n","f"] (ApplicationS (VarS "m") [ ApplicationS (VarS "n") [ (VarS "f") ] ] ) ) [ LambdaS ["f", "x"] (VarS "x"), ApplicationS (LambdaS ["m","n"] (ApplicationS (VarS "n") [ (LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ (VarS "f"), (ApplicationS (VarS "f") [ (VarS "x") ] ) ] ) ), (VarS "m") ] ) ) [ (ApplicationS (LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ (VarS "f"), (ApplicationS (VarS "f") [ (VarS "x") ] ) ] ) ) [ (LambdaS ["f", "x"] (VarS "x") ) ] ), (ApplicationS (LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ (VarS "f"), (ApplicationS (VarS "f") [ (VarS "x") ] ) ] ) ) [ (LambdaS ["f", "x"] (VarS "x") ) ] ) ] ], ApplicationS (LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ VarS "f", ApplicationS (VarS "f") [ (VarS "x") ] ] ) ) [ ApplicationS (LambdaS ["n","f","x"] (ApplicationS (VarS "n") [ (VarS "f"), (ApplicationS (VarS "f") [ (VarS "x") ] ) ] ) ) [ (LambdaS ["f", "x"] (VarS "x") ) ] ] ] ]

----------------------------------
-- Extra credit: Sweeten a term --
----------------------------------
applicationSCleanup :: TermS -> TermS
applicationSCleanup (ApplicationS (ApplicationS (t1a) (t1b)) t2) = ApplicationS t1a (t1b++t2)
applicationSCleanup t = t

lambdaSCleanup :: TermS -> TermS
lambdaSCleanup (LambdaS s1 (LambdaS s2 t)) = LambdaS (s1++s2) t
lambdaSCleanup t = t                                                            

sugar :: Term -> TermS
sugar (Var s) = VarS s
sugar (Lambda s1 (Lambda s2 t)) = lambdaSCleanup(LambdaS [s1,s2] (sugar t))
sugar (Lambda s t) = LambdaS [s] (sugar t)
sugar (Application (Application (t1a) (t1b)) t2) = applicationSCleanup (ApplicationS (sugar t1a) [sugar t1b, sugar t2])
sugar (Application t1 t2) = ApplicationS (sugar t1) [sugar t2]

---------------
-- ATTENTION --
---------------
-- You said you wanted testCase on the last two lines, so I just want to call to attention that I have sugar completed above for extra credit
-- Wasn't sure if I should have put it below testCase or not so I left it above
testCase :: Term
testCase = (normalize . deSugar) testCaseS