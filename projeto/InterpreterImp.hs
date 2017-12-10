module InterpreterImp where

import System.Environment
import Control.Monad
import Control.Monad.State
import ParserImp
import qualified Data.Map as M

data IType = IInt Integer
           | IBool Bool
           | IUndef

instance Show IType where
        show (IInt i)    = show i
        show (IBool b)   = show b
        show IUndef      = "undef"

type Scope  = M.Map String IType

getVariable :: Scope -> String -> IType
getVariable scope var =
    case M.lookup var scope of
         Just x -> x
         Nothing -> error $ "Variable " ++ var ++ " is not in scope"

evalAExpr :: Scope -> AritExpression -> Integer
evalAExpr scope (IntConst val) = val
evalAExpr scope (Neg expr) = negate $ evalAExpr scope expr
evalAExpr scope (IVar var) = 
    case getVariable scope var of
        IInt x -> x
        IUndef -> error $ "Variable " ++ var ++ " undefined"
        _      -> error $ "Variable " ++ var ++ " is not of type Int"
evalAExpr scope (ABinary op exp1 exp2) =
    case op of
        Add  -> evalAExpr scope exp1 + evalAExpr scope exp2
        Mult -> evalAExpr scope exp1 * evalAExpr scope exp2
        Sub  -> evalAExpr scope exp1 - evalAExpr scope exp2
        Div  -> evalAExpr scope exp1 `div` evalAExpr scope exp2

evalBExpr :: Scope -> BoolExpression -> Bool
evalBExpr scope (BoolConst val) = val
evalBExpr scope (Not expr) = not $ evalBExpr scope expr
evalBExpr scope (BVar var) =
    case getVariable scope var of
        IBool x -> x
        IUndef  -> error $ "Variable " ++ var ++ " undefined"
        _       -> error $ "Variable " ++ var ++ " is not of type Bool"
evalBExpr scope (BBinary op exp1 exp2) =
    case op of
        And -> evalBExpr scope exp1 && evalBExpr scope exp2
        Or  -> evalBExpr scope exp1 || evalBExpr scope exp2
evalBExpr scope (RBinary op exp1 exp2) =
    case op of
        Less    -> evalAExpr scope exp1 <  evalAExpr scope exp2
        Greater -> evalAExpr scope exp1 >  evalAExpr scope exp2
        Equal   -> evalAExpr scope exp1 == evalAExpr scope exp2

evalExpr :: Scope -> Expression -> IType
evalExpr scope expr =
    case expr of
        IntExpr    expr -> IInt    $ evalAExpr scope expr
        BoolExpr   expr -> IBool   $ evalBExpr scope expr
        VarExpr    var  -> getVariable scope var

eval :: Command -> StateT Scope IO ()
eval Skip = return ()

eval (Chain stmts) = mapM_ eval stmts

eval (Declaration vars) =
    modify (flip (foldl (\m v -> M.insert v IUndef m)) vars)

eval (Assign var value) = do
    scope <- get
    modify (M.insert var $ evalExpr scope value)

eval (Print expr) = do
    scope <- get
    lift $ putStrLn $ show $ evalExpr scope expr

eval (If bexpr stmt1 stmt2) = do
    scope <- get
    if evalBExpr scope bexpr
        then eval stmt1
        else eval stmt2

eval while@(While bexpr body) = do
    scope <- get
    when (evalBExpr scope bexpr) $ eval body >> eval while

interpret :: String -> IO ()
interpret filename = do
    source <- readFile filename
    let program = parseSource source
    evalStateT (eval program) M.empty

main :: IO ()
main = do
    [filename] <- getArgs
    interpret filename
