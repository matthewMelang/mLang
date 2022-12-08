module Main where

import Data.List
import Data.Char
import Data.Bits
import System.Directory
import qualified GHC.IO.Encoding as Encoding
{- -------------------------------------Language Structure------------------------------------- -}

type Map = [(String, String)]
zwsp = '\8203' -- Zero width space, Used for seperating symbols

type Ident = String

type Modifiers = [Mod]

type VarList = [(Ident, Modifiers, Value)]

type FuncList = [(Ident, Function)]

type ClassList = [(Ident, Class)]

type Env = (VarList, FuncList, ClassList)

type PrintStack = [Value]

                -- Name  Vars              body 
data Function = Fn Ident [Instr] Modifiers Instr
              deriving (Show, Eq)

              -- Name            Body
data Class = Cl Ident Modifiers Instr
           deriving (Show, Eq)

data Expr = Const   Value
          | Var     Ident
          | ArExpr  AExpr
          | BlExpr  BExpr
          | Ternary BExpr Expr Expr
          | FuncRet Ident [Expr]
          | CCreate Ident [Expr]
          | ClsGet  Ident Expr
          | ArrInit Modifiers [Expr]
          | ArrGet  Ident Expr
          -- BUILT IN FUNCTIONS
          | Type    Expr
          | ToInt   Expr
          | ToDbl   Expr
          | ToBol   Expr
          | ToStr   Expr
          | ToChr   Expr
          deriving (Show, Eq)

data AExpr = Add     Expr  Expr
           | Sub     Expr  Expr
           | Mul     Expr  Expr
           | Div     Expr  Expr
           | Exp     Expr  Expr
           | Mod     Expr  Expr
           | ShiftL  Expr  Expr
           | ShiftR  Expr  Expr
           | RotL    Expr  Expr
           | RotR    Expr  Expr
           | AAnd    Expr  Expr
           | AOr     Expr  Expr
           | AXor    Expr  Expr
           | Neg     Expr
           | Comp    Expr
           | Inc     Expr
           | Dec     Expr
           deriving (Show, Eq)

data BExpr = Eq      Expr  Expr
           | NotEq   Expr  Expr
           | LeT     Expr  Expr
           | LTE     Expr  Expr
           | GrT     Expr  Expr
           | GTE     Expr  Expr
           | BAnd    Expr  Expr
           | BOr     Expr  Expr
           | BXor    Expr  Expr
           | Not     Expr
           deriving (Show, Eq)

data Instr = Assign  Ident Modifiers Expr
           | ChnAsgn Ident Modifiers Instr
           | Delete  Ident
           | If      Expr  Instr
           | Else    Instr 
           | IfElse  Expr  Instr Instr
           | For     Instr Expr  Instr Instr
           | ForIn   Expr  Expr  Instr
           | While   Expr  Instr
           | DoWhile Instr Expr
           | Switch  Expr  Instr
           | Case    Expr  Instr
           | Default Instr
           | Func    Function
           | FuncRun Ident [Expr]
           | Cls     Class
           | ClsRun  Ident Instr
           | ArrSet  Ident Expr Expr
           | Block   [Instr] Env
           | Do      [Instr]
           | Print   Expr
           deriving (Show, Eq)

data BinaryOp = BinAdd 
              | BinSub
              | BinMul
              | BinDiv
              | BinExp
              | BinMod
              | BinAddEq
              | BinSubEq
              | BinMulEq
              | BinDivEq
              | BinExpEq
              | BinModEq
              | BinEq
              | BinNotEq
              | BinAAnd
              | BinAOr
              | BinAXor
              | BinBAnd
              | BinBOr
              | BinBXor
              | BinShiftL
              | BinShiftR
              | BinRotL
              | BinRotR
              | BinLT
              | BinLTE
              | BinGT
              | BinGTE
              deriving (Show, Eq)

data UnaryOp = UnaNot
             | UnaComp
             | UnaInc
             | UnaDec
             | UnaPrint
             | UnaPrintLn
             | UnaType
             | UnaDelete
             deriving (Show, Eq)

data Keywords = KW_Ternary
              | KW_If
              | KW_Else
              | KW_For
              | KW_In
              | KW_While
              | KW_Do
              | KW_Switch
              | KW_Case
              | KW_Default
              | KW_Func
              | KW_Class
              | KW_New
              | KW_Break
              deriving (Show, Eq)

data Token = Tok_Const  Value
           | Tok_Var    Ident
           | Tok_Assign
           | Tok_BOp    BinaryOp
           | Tok_UOp    UnaryOp
           | Tok_KW     Keywords
           | Tok_SemiC
           | Tok_LPar
           | Tok_RPar
           | Tok_LBra
           | Tok_RBra
           | Tok_LBrk
           | Tok_RBrk
           | Tok_Comma
           | Tok_Colon
           | Tok_Dot
           | Tok_EOL
           | Tok_CmtBlock [Token]
           | Tok_CmtSt
           | Tok_CmtEnd
           | Tok_CmtSL
           | Tok_CmtSLBlock [Token]
           | Tok_Mod    [Mod]
           | Tok_PE     Expr
           | Tok_PI     Instr
           | Tok_Err    Int Char
           deriving (Show, Eq)

data Value = NumVal    Int
           | DoubleVal Double
           | BoolVal   Bool
           | StrVal    String
           | CharVal   Char
           | ObjVal    Class
           | ArrVal    [Value]
           | NullVal
           deriving (Show, Eq)
           

data Type = TypeInt
          | TypeDbl
          | TypeBol
          | TypeStr
          | TypeChr
          | TypeUDF
          | TypeArr Type
          | TypeObj Ident
          deriving (Show, Eq)

data Mod = ModPublic
         | ModPrivate
         | ModReadOnly
         | ModType Type
         deriving (Show, Eq)


{- -------------------------------------CODE MAPPING------------------------------------- -}

{-  -}
convertLine :: [String] -> Map -> String
convertLine [] map = []
convertLine (x:xs) map = let lineList = (splitBy [' '] x) in
                         let converted = (convertStr lineList map) in
                         if (length xs >= 1) 
                            then (converted) ++ ['\n'] ++ convertLine xs map
                            else (converted) ++ convertLine xs map
                         
{- -}
convertStr :: [String] -> Map -> String
convertStr [] map = []
convertStr (x:xs) map = let lookupResult = lookup' x map in
                        let delim = if (length xs >= 1) then [' '] else [] in
                        case lookupResult of
                        Just res -> res ++ delim ++ convertStr xs map
                        Nothing  -> (convertWord (splitBy [zwsp] x) map) ++ delim ++ convertStr xs map

{- -}
convertWord :: [String] -> Map -> String
convertWord [] map = []              
convertWord (x:xs) map = let lookupResult = lookup' x map in
                         case lookupResult of
                         Just res -> res ++ convertWord xs map
                         Nothing  -> (convertLetter x map) ++ convertWord xs map

{- -}
convertLetter :: String -> Map -> String
convertLetter [] map = []
convertLetter (x:xs) map = let lookupResult = lookup' [x] map in
                           case lookupResult of
                           Just res -> res ++ convertLetter xs map
                           Nothing  -> convertLetter xs map

{- 
    Convert takes a input string and a map and converts the input string using the map and returns it.
    Can be used to both encode and decode strings.
-}
convert :: String -> Map -> String
convert input map = let lineList = (splitBy ['\n'] input) in
                        convertLine lineList map

{- -}
lookup' :: String -> [(String, String)] -> Maybe String
lookup' _ [] = Nothing
lookup' key ((k, v):rest) =
    let (k':krest) = splitBy [zwsp] k in
    if (key == k') then Just v
       else if (key == v) then Just k
            else lookup' key rest

{- -}
splitBy :: [Char] -> String -> [String]
splitBy delimiter = foldr f [[]] 
            where f c l@(x:xs) | c `elem` delimiter = []:l
                               | otherwise = (c:x):xs

{- -}
buildMap :: [String] -> Map
buildMap [] = [("","")]
buildMap (x:xs) = let kvp = (splitBy [' '] x) in
                  ([(head kvp ++ [zwsp], last kvp)] ++ buildMap xs)

{- ------------------------------------CODE LEXICAL ANALYSIS----------------------------------- -}

{-
    Lexer takes a decoded string input and converts it into a list of tokens
    which can be used for parsing later.
-}
lexer :: String -> Int -> [Token]
lexer "" pos = []
lexer ('+':'=' : s)                   pos = Tok_BOp BinAddEq  : lexer s (pos + 2)
lexer ('+':'+' : s)                   pos = Tok_UOp UnaInc    : lexer s (pos + 2)
lexer ('+' : s)                       pos = Tok_BOp BinAdd    : lexer s (pos + 1)
lexer ('-':'=' : s)                   pos = Tok_BOp BinSubEq  : lexer s (pos + 2)
lexer ('-':'-' : s)                   pos = Tok_UOp UnaDec    : lexer s (pos + 2)
lexer ('-' : s)                       pos = Tok_BOp BinSub    : lexer s (pos + 1)
lexer ('/':'/':s)                     pos = Tok_CmtSL         : lexer s (pos + 2)
lexer ('*':'/':s)                     pos = Tok_CmtEnd        : lexer s (pos + 2)
lexer ('/':'*':s)                     pos = Tok_CmtSt         : lexer s (pos + 2)
lexer ('*':'=' : s)                   pos = Tok_BOp BinMulEq  : lexer s (pos + 2)
lexer ('*' : s)                       pos = Tok_BOp BinMul    : lexer s (pos + 1)
lexer ('/':'=' : s)                   pos = Tok_BOp BinDivEq  : lexer s (pos + 2)
lexer ('/' : s)                       pos = Tok_BOp BinDiv    : lexer s (pos + 1)
lexer ('^':'=' : s)                   pos = Tok_BOp BinExpEq  : lexer s (pos + 2)
lexer ('^' : s)                       pos = Tok_BOp BinExp    : lexer s (pos + 1)
lexer ('%':'=' : s)                   pos = Tok_BOp BinModEq  : lexer s (pos + 2)
lexer ('%' : s)                       pos = Tok_BOp BinMod    : lexer s (pos + 1)
lexer ('!':'=' : s)                   pos = Tok_BOp BinNotEq  : lexer s (pos + 2)
lexer ('=':'=' : s)                   pos = Tok_BOp BinEq     : lexer s (pos + 2)
lexer ('=' : s)                       pos = Tok_Assign        : lexer s (pos + 1)

lexer ('&':'&' : s)                   pos = Tok_BOp BinBAnd   : lexer s (pos + 2)
lexer ('|':'|' : s)                   pos = Tok_BOp BinBOr    : lexer s (pos + 2)
lexer ('X':'O':'R' : s)               pos = Tok_BOp BinBXor   : lexer s (pos + 3)

lexer ('&' : s)                       pos = Tok_BOp BinAAnd   : lexer s (pos + 1)
lexer ('|' : s)                       pos = Tok_BOp BinAOr    : lexer s (pos + 1)
lexer ('x':'o':'r' : s)               pos = Tok_BOp BinAXor   : lexer s (pos + 3)

lexer ('<':'<' : s)                   pos = Tok_BOp BinShiftL : lexer s (pos + 2)
lexer ('>':'>' : s)                   pos = Tok_BOp BinShiftR : lexer s (pos + 2)
lexer ('R':'o':'t':'L' : s)           pos = Tok_BOp BinRotL   : lexer s (pos + 4)
lexer ('R':'o':'t':'R' : s)           pos = Tok_BOp BinRotR   : lexer s (pos + 4)
lexer ('<':'=' : s)                   pos = Tok_BOp BinLTE    : lexer s (pos + 2)
lexer ('<' : s)                       pos = Tok_BOp BinLT     : lexer s (pos + 1)
lexer ('>':'=' : s)                   pos = Tok_BOp BinGTE    : lexer s (pos + 2)
lexer ('>' : s)                       pos = Tok_BOp BinGT     : lexer s (pos + 1)
lexer ('!' : s)                       pos = Tok_UOp UnaNot    : lexer s (pos + 1)
lexer ('~' : s)                       pos = Tok_UOp UnaComp   : lexer s (pos + 1)
lexer (';' : s)                       pos = Tok_SemiC         : lexer s (pos + 1)
lexer (',' : s)                       pos = Tok_Comma         : lexer s (pos + 1)
lexer (':' : s)                       pos = Tok_Colon         : lexer s (pos + 1)
lexer ('.' : s)                       pos = Tok_Dot           : lexer s (pos + 1)
lexer ('?' : s)                       pos = Tok_KW KW_Ternary : lexer s (pos + 1)
lexer ('(':s)                         pos = Tok_LPar          : lexer s (pos + 1)
lexer (')':s)                         pos = Tok_RPar          : lexer s (pos + 1)
lexer ('{':s)                         pos = Tok_LBra          : lexer s (pos + 1)
lexer ('}':s)                         pos = Tok_RBra          : lexer s (pos + 1)
lexer ('[':s)                         pos = Tok_LBrk          : lexer s (pos + 1)
lexer (']':s)                         pos = Tok_RBrk          : lexer s (pos + 1)
lexer ('i':'f':s)                     pos = Tok_KW KW_If      : lexer s (pos + 2)
lexer ('e':'l':'s':'e':s)             pos = Tok_KW KW_Else    : lexer s (pos + 4)
lexer ('f':'o':'r':s)                 pos = Tok_KW KW_For     : lexer s (pos + 3)
lexer ('i':'n':s)                     pos = Tok_KW KW_In      : lexer s (pos + 2)
lexer ('w':'h':'i':'l':'e':s)         pos = Tok_KW KW_While   : lexer s (pos + 5)
lexer ('d':'o':s)                     pos = Tok_KW KW_Do      : lexer s (pos + 2)
lexer ('s':'w':'i':'t':'c':'h':s)     pos = Tok_KW KW_Switch  : lexer s (pos + 6)
lexer ('c':'a':'s':'e':s)             pos = Tok_KW KW_Case    : lexer s (pos + 4)
lexer ('d':'e':'f':'a':'u':'l':'t':s) pos = Tok_KW KW_Default : lexer s (pos + 7)
lexer ('p':'r':'i':'n':'t':'L':'n':s) pos = Tok_UOp UnaPrintLn: lexer s (pos + 7)
lexer ('p':'r':'i':'n':'t':s)         pos = Tok_UOp UnaPrint  : lexer s (pos + 5)
lexer ('d':'e':'f':s)                 pos = Tok_KW KW_Func    : lexer s (pos + 3)
lexer ('c':'l':'a':'s':'s':s)         pos = Tok_KW KW_Class   : lexer s (pos + 5)
lexer ('n':'e':'w':s)                 pos = Tok_KW KW_New     : lexer s (pos + 3)
lexer ('b':'r':'e':'a':'k':s)         pos = Tok_KW KW_Break   : lexer s (pos + 5)
lexer ('d':'e':'l':'e':'t':'e':s)     pos = Tok_UOp UnaDelete : lexer s (pos + 6)

lexer ('t':'y':'p':'e':'U':'d':'f':s)     pos = Tok_Mod [(ModType TypeUDF)] : lexer s (pos + 7)
lexer ('t':'y':'p':'e':'O':'b':'j':s)     pos = Tok_Mod [(ModType (TypeObj ""))] : lexer s (pos + 7)
lexer ('t':'y':'p':'e':'C':'h':'r':s)     pos = Tok_Mod [(ModType TypeChr)] : lexer s (pos + 7)
lexer ('t':'y':'p':'e':'S':'t':'r':s)     pos = Tok_Mod [(ModType TypeStr)] : lexer s (pos + 7)
lexer ('t':'y':'p':'e':'B':'o':'l':s)     pos = Tok_Mod [(ModType TypeBol)] : lexer s (pos + 8)
lexer ('t':'y':'p':'e':'D':'b':'l':s)     pos = Tok_Mod [(ModType TypeDbl)] : lexer s (pos + 7)
lexer ('t':'y':'p':'e':'I':'n':'t':s)     pos = Tok_Mod [(ModType TypeInt)] : lexer s (pos + 7)
lexer ('r':'e':'a':'d':'O':'n':'l':'y':s) pos = Tok_Mod [ModReadOnly] : lexer s (pos + 8)
lexer ('p':'r':'i':'v':'a':'t':'e':s)     pos = Tok_Mod [ModPrivate] : lexer s (pos + 7)
lexer ('p':'u':'b':'l':'i':'c':s)         pos = Tok_Mod [ModPublic] : lexer s (pos + 6)

lexer ('t':'y':'p':'e':s)             pos = Tok_UOp UnaType   : lexer s (pos + 4)

lexer ('N':'U':'L':'L':s)             pos = Tok_Const (NullVal)       : lexer s (pos + 4)
lexer ('T':'r':'u':'e' : s)           pos = Tok_Const (BoolVal True)  : lexer s (pos + 4)
lexer ('F':'a':'l':'s':'e' : s)       pos = Tok_Const (BoolVal False) : lexer s (pos + 5)
lexer ('\'':x:'\'' : s)               pos = Tok_Const (CharVal x)     : lexer s (pos + 3)

lexer ('"' : s)                 pos = let (str, (x:rst)) = span (/= '"') s
                                      in Tok_Const (StrVal str) : lexer rst (pos + 1)

lexer (x : s)                   pos | isDigit x  = let (num, (y:rst)) = span isDigit s -- pos + 1 isnt right. Needs to be updated
                                                   in if (y == '.') 
                                                      then let (num', rst') = span isDigit rst
                                                           in let dblVal = [x] ++ num ++ [y] ++ num' 
                                                              in Tok_Const (DoubleVal (read dblVal)) : lexer rst' (pos + 1)
                                                      else Tok_Const (NumVal (read (x : num))) : lexer (y:rst) (pos + 1)

lexer (x : s)                   pos | isAlpha x  = let (str, rst) = span (\x -> isAlphaNum x || x == '_') s -- pos + 1 isnt right. Needs to be updated
                                                   in  Tok_Var (x : str) : lexer rst (pos + 1)

lexer ('\n' : s)                pos = Tok_EOL : lexer s pos
lexer (x : s)                   pos | isSpace x  = lexer s (pos + 1)
lexer ('\\':'8':'2':'0':'3': s) pos = lexer s pos
lexer (x : s) pos                   = [Tok_Err pos x]

{- ------------------------------------CODE SHIFT REDUCTION----------------------------------- -}

sr :: [Token] -> [Token] -> [Token]
sr (Tok_Var x : s) queue = sr (Tok_PE (Var x) : s) queue
sr (Tok_Const c : s) queue = sr (Tok_PE (Const c) : s) queue

-- ARITHMETIC EXPRESSIONS
sr (Tok_PE e2 : Tok_BOp BinAdd : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Add e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinSub : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Sub e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinMul : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Mul e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinDiv : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Div e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinExp : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Exp e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinMod : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (Mod e1 e2)) : s) queue

-- APPLY TO SELF
sr (Tok_PE e2 : Tok_BOp BinAddEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Add (Var i) e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinSubEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Sub (Var i) e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinMulEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Mul (Var i) e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinDivEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Div (Var i) e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinExpEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Exp (Var i) e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinModEq : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Mod (Var i) e2))) : s) queue

-- INCREMENTERS & DECREMENTERS
sr (Tok_UOp UnaInc : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Add (Var i) (Const (NumVal 1))))) : s) queue
sr (Tok_UOp UnaInc : Tok_PE e : s) queue = sr (Tok_PE (ArExpr (Add e (Const (NumVal 1)))) : s) queue
sr (Tok_UOp UnaDec : Tok_PE (Var i) : s) queue = sr (Tok_PI (Assign i [] (ArExpr (Sub (Var i) (Const (NumVal 1))))) : s) queue
sr (Tok_UOp UnaDec : Tok_PE e : s) queue = sr (Tok_PE (ArExpr (Sub e (Const (NumVal 1)))) : s) queue

-- BIT SHIFTING & ROTATING
sr (Tok_PE e2 : Tok_BOp BinShiftL : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (ShiftL e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinShiftR : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (ShiftR e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinRotL   : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (RotL   e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinRotR   : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (RotR   e1 e2)) : s) queue

-- BINARY ANDING / ORING
sr (Tok_PE e2 : Tok_BOp BinAAnd : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (AAnd e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinAOr  : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (AOr  e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinAXor : Tok_PE e1 : s) queue = sr (Tok_PE (ArExpr (AXor e1 e2)) : s) queue

sr (Tok_PE e2 : Tok_BOp BinSub : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Sub e1 e2))) : s) queue
sr (Tok_PE e  : Tok_BOp BinSub : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Sub e e'))) : s) queue

-- UNARY ARITHMETIC EXPRESSIONS (COMPLEMENT & NEGATION)
sr (Tok_PE e : Tok_UOp UnaComp : s) queue = sr (Tok_PE (ArExpr (Comp e)) : s) queue
sr (Tok_PE e : Tok_BOp BinSub  : s) queue = sr (Tok_PE (ArExpr (Neg  e)) : s) queue

-- BOOLEAN EXPRESSIONS
sr (Tok_PE e2 : Tok_BOp BinEq    : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (Eq    e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinNotEq : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (NotEq e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinLT    : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (LeT   e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinLTE   : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (LTE   e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinGT    : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (GrT   e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinGTE   : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (GTE   e1 e2)) : s) queue
sr (Tok_PE e  : Tok_UOp UnaNot   : s)             queue = sr (Tok_PE (BlExpr (Not   e))     : s) queue

-- BOOLEAN ANDING / ORING
sr (Tok_PE e2 : Tok_BOp BinBAnd : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (BAnd e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinBOr  : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (BOr  e1 e2)) : s) queue
sr (Tok_PE e2 : Tok_BOp BinBXor : Tok_PE e1 : s) queue = sr (Tok_PE (BlExpr (BXor e1 e2)) : s) queue

-- TYPE MODIFIERS
sr (Tok_PE (Var x) : Tok_Mod [(ModType (TypeObj ""))] : s) queue = sr (Tok_Mod [(ModType (TypeObj x))] : s) queue
sr (Tok_PE (Var x) : Tok_Mod ((ModType (TypeObj "")) : s') : s) queue = sr (Tok_Mod ((ModType (TypeObj x)) : s') : s) queue
sr (Tok_Mod x : Tok_Mod y : s) queue = sr (Tok_Mod (x ++ y) : s) queue
sr (Tok_RBrk : Tok_LBrk : Tok_Mod [ModType x] : s) queue = sr (Tok_Mod [ModType (TypeArr x)] : s) queue

-- DEALLOCATE VARIABLE
sr (Tok_PE (Var v) : Tok_UOp UnaDelete : s) queue = sr (Tok_PI (Delete v) : s) queue

-- ARRAY CREATION
sr (Tok_PI (Block b e') : Tok_RBra : Tok_PE e : Tok_LBra : s) queue = sr (Tok_PE (ArrInit [] [e]) : s) queue
sr (Tok_PI (Block b e') : Tok_RBra : Tok_PE e : Tok_Comma : s) queue = sr (Tok_PE (ArrInit [] [e]) : Tok_RBra : s) queue
sr (Tok_PE (ArrInit _ lst) : Tok_RBra : Tok_PE e : s) queue = sr (Tok_PE (ArrInit [] (e:lst)) : Tok_RBra : s) queue
sr (Tok_PE (ArrInit _ lst) : Tok_RBra : Tok_Comma : Tok_PE e : s) queue = sr (Tok_PE (ArrInit [] (e:lst)) : Tok_RBra : s) queue
sr (Tok_PE (ArrInit _ lst) : Tok_RBra : Tok_LBra : s) queue = sr (Tok_PE (ArrInit [] lst) : s) queue
sr (Tok_PI (Assign v (ml:mlRest) (ArrInit [] valLst)) : s) queue = sr (Tok_PI (Assign v (ml:mlRest) (ArrInit (ml:mlRest) valLst)) : s) queue

-- ARRAY GET/SET INDEX
sr (Tok_RBrk : Tok_PE e : Tok_LBrk : Tok_PI (Assign v ml (Var arr)) : s) queue = sr (Tok_PI (Assign v ml (ArrGet arr e)) : s) queue
sr (Tok_PE val : Tok_Assign : Tok_RBrk : Tok_PE index : Tok_LBrk : Tok_PE (Var arr) : s) queue = sr (Tok_PI (ArrSet arr index val) : s) queue

-- ASSIGNMENT
sr (Tok_PI (Assign v ml exp) : Tok_Mod ml' : s) queue = sr (Tok_PI (Assign v ml' exp) : s) queue
sr (Tok_PE (Var v) : Tok_Mod ml : s) queue = 
    let newA = srTypedAssgn (Var v) ml
    in sr (Tok_PI newA : s) queue

sr (Tok_PE e : Tok_Assign : Tok_PE (Var v) : s) queue = sr (Tok_PI (Assign v [ModType TypeUDF] e) : s) queue
sr (Tok_PE e : Tok_BOp BinAdd : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Add e e'))) : s) queue
sr (Tok_PE e : Tok_BOp BinMul : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Mul e e'))) : s) queue
sr (Tok_PE e : Tok_BOp BinDiv : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Div e e'))) : s) queue
sr (Tok_PE e : Tok_BOp BinExp : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Exp e e'))) : s) queue
sr (Tok_PE e : Tok_BOp BinMod : Tok_PI (Assign v ml e') : s) queue = sr (Tok_PI (Assign v ml (ArExpr (Mod e e'))) : s) queue
sr (Tok_PE e : Tok_Assign : Tok_PI a@(Assign v ml e') : s) queue = 
    let newA = srFixAssign (Left e) a
    in sr (Tok_PI newA : s) queue
sr (Tok_PE e : Tok_Assign : Tok_PI a@(ChnAsgn v ml e') : s) queue = 
    let newA = srFixAssign (Left e) a
    in sr (Tok_PI newA : s) queue
sr (Tok_PI e : Tok_Assign : Tok_PI a@(Assign v ml e') : s) queue = 
    let newA = srFixAssign (Right e) a
    in sr (Tok_PI newA : s) queue


-- PRINTING
sr (Tok_PE e1 : Tok_UOp UnaPrintLn : s) queue = sr (Tok_PI (Print (ArExpr (Add e1 (Const (StrVal "\n"))))) : s) queue
sr (Tok_PE e  : Tok_UOp UnaPrint   : s) queue = sr (Tok_PI (Print e) : s) queue
sr (Tok_PE e2 : Tok_BOp BinAdd : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Add e1 e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinMul : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Mul e1 e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinDiv : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Div e1 e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinExp : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Exp e1 e2))) : s) queue
sr (Tok_PE e2 : Tok_BOp BinMod : Tok_PI (Print e1) : s) queue = sr (Tok_PI (Print (ArExpr (Mod e1 e2))) : s) queue

-- TERNARY IF
sr (Tok_PE e2 : Tok_Colon : Tok_PE e1 : Tok_KW KW_Ternary : Tok_PE (BlExpr b) : s) queue = sr (Tok_PE (Ternary b e1 e2) : s) queue

-- IF BLOCKS
sr (Tok_PI (Else e2) : Tok_PI (IfElse b e1 (If b' e1')) : s) queue = sr (Tok_PI (IfElse b e1 (IfElse b' e1' e2)) : s) queue
sr (Tok_PI (Else e2) : Tok_PI (If b e1) : s)                 queue = sr (Tok_PI (IfElse b e1 e2) : s) queue
sr (Tok_PI e : Tok_PE b : Tok_KW KW_If : s)                  queue = sr (Tok_PI (If b e) : s) queue
sr (Tok_PI e : Tok_KW KW_Else : s)                           queue = sr (Tok_PI (Else e) : s) queue

-- SWITCH STATEMENT
sr (Tok_PI exe : Tok_PE c : Tok_KW KW_Case : s)       queue = sr (Tok_PI (Case c exe) : s) queue
sr (Tok_PI exe : Tok_KW KW_Default : s)               queue = sr (Tok_PI (Default exe) : s) queue
sr (Tok_PE c : Tok_KW KW_Switch : s)                  queue = sr (Tok_PI (Switch c (Block [] ([], [], []))) : s) queue
sr (Tok_PI (Block b lenv) : Tok_PI (Switch c list):s) queue = sr (Tok_PI (Switch c (Block b lenv)) : s) queue

-- FOR LOOPS
sr (Tok_PI exe : Tok_RPar : Tok_PI upd : Tok_PE cond : Tok_PI init : Tok_LPar : Tok_KW KW_For : s) queue = sr (Tok_PI (For init cond upd exe) : s) queue
sr (Tok_PI exe : Tok_PE list : Tok_KW KW_In : Tok_PE var : Tok_KW KW_For : s)                      queue = sr (Tok_PI (ForIn var list exe) : s) queue
sr (Tok_PI exe : Tok_PE list : Tok_KW KW_In : Tok_PI (Assign x ml y) : Tok_KW KW_For : s)          queue = sr (Tok_PI (ForIn (Var x) list exe) : s) queue

-- WHILE LOOPS
sr (Tok_PI exe : Tok_PE cond : Tok_KW KW_While : s) queue = sr (Tok_PI (While cond exe) : s) queue

-- CODE BLOCK
sr (Tok_RBra : s)                                   queue = sr (Tok_PI (Block [] ([], [], [])) : Tok_RBra : s) queue
sr (Tok_PI (Block b e) : Tok_RBra : Tok_LBra : s)   queue = sr (Tok_PI (Block b e) : s) queue
sr (Tok_PI (Block b e) : Tok_RBra : Tok_PI exp : s) queue = sr (Tok_PI (Block (exp : b) e) : Tok_RBra : s) queue

-- MULTI LINE COMMENTS
sr (Tok_CmtEnd : s)                                queue = sr (Tok_CmtBlock[] : Tok_CmtEnd : s) queue
sr (Tok_CmtBlock b : Tok_CmtEnd : Tok_CmtSt : s)   queue = sr (Tok_CmtBlock b : s) queue
sr (Tok_CmtBlock b : Tok_CmtEnd : x : s)           queue = sr (Tok_CmtBlock(x:b) : Tok_CmtEnd : s) queue

-- SINGLE LINE COMMENTS

-- FUNCTION DEFINITIONS
sr (Tok_PI (Block b e) : Tok_RPar : Tok_LPar : Tok_PE (Var fName) : Tok_KW KW_Func : s) queue = sr (Tok_PI (Func (Fn fName [] [ModType TypeUDF] (Block b e)))  : s) queue
sr (Tok_PI (Block b e) : Tok_PE (Var argn)   : Tok_PE (Var fName) : Tok_KW KW_Func : s) queue = 
    let newA = Assign argn [ModType TypeUDF] (Const NullVal)
    in sr (Tok_PI (Func (Fn fName [newA] [ModType TypeUDF] (Block b e))) : s) queue
sr (Tok_PI (Block b e) : Tok_PI a@(Assign i ml e') : Tok_PE (Var fName) : Tok_KW KW_Func : s) queue = sr (Tok_PI (Func (Fn fName [a] [] (Block b e))) : s) queue
sr (Tok_PI (Block b e) : Tok_RPar : Tok_PE (Var argn) : Tok_Comma : s) queue = 
    let newA = Assign argn [ModType TypeUDF] (Const NullVal)
    in sr (Tok_PI (Func (Fn "" [newA] [] (Block b e))) : Tok_RPar : s) queue
sr (Tok_PI (Block b e) : Tok_RPar : Tok_PI a@(Assign i ml e') : Tok_Comma : s) queue = sr (Tok_PI (Func (Fn "" [a] [] (Block b e))) : Tok_RPar : s) queue
sr (Tok_PI (Block b e) : Tok_PI (FuncRun fName varLst) : Tok_KW KW_Func : s) queue =
    let aLst = srAssign varLst
    in sr (Tok_PI (Func (Fn fName aLst [ModType TypeUDF] (Block b e))) : s) queue
sr (Tok_PI (Block b e) : Tok_RPar : Tok_LPar : Tok_PI a@(Assign fName ml e') : s) queue = sr (Tok_PI (Func (Fn fName [] ml (Block b e))) : s) queue

sr (Tok_PI (Func (Fn _ vL mL block)) : Tok_RPar : Tok_PE (Var argn) : s) queue =
    let newA = Assign argn [ModType TypeUDF] (Const NullVal)
    in sr (Tok_PI (Func (Fn "" (newA:vL) mL block)) : Tok_RPar : s) queue
sr (Tok_PI (Func (Fn _ vL mL block)) : Tok_RPar : Tok_LPar : Tok_PE (Var fName) : Tok_KW KW_Func : s) queue = sr (Tok_PI (Func (Fn fName vL mL block)) : s) queue
sr (Tok_PI (Func (Fn _ vL mL block)) : Tok_RPar : Tok_PI a@(Assign i ml e') : s) queue = sr (Tok_PI (Func (Fn "" (a:vL) mL block)) : Tok_RPar : s) queue
sr (Tok_PI (Func (Fn _ vL mL block)) : Tok_RPar : Tok_Comma : Tok_PE (Var argn) : s) queue = 
    let newA = Assign argn [ModType TypeUDF] (Const NullVal)
    in sr (Tok_PI (Func (Fn "" (newA:vL) mL block)) : Tok_RPar : s) queue
sr (Tok_PI (Func (Fn _ vL mL block)) : Tok_RPar : Tok_Comma : Tok_PI a@(Assign i ml e') : s) queue = sr (Tok_PI (Func (Fn "" (a:vL) mL block)) : Tok_RPar : s) queue

sr (Tok_PI (Func (Fn n vl _ body)) : Tok_Mod ml : s) queue = sr (Tok_PI (Func (Fn n vl ml body)) : s) queue

-- FUNCTION CALLS
sr (Tok_RPar : Tok_LPar : Tok_PE (Var fName) : s) queue = sr (Tok_PI (FuncRun fName []) : s) queue
sr (Tok_RPar : Tok_LPar : Tok_PI (Assign v ml (Var fName)) : s) queue = sr (Tok_PI (Assign v ml (FuncRet fName [])) : s) queue
sr (Tok_PE e : Tok_PE (Var fName) : s) queue = sr (Tok_PI (FuncRun fName [e]) : s) queue
sr (Tok_RPar : Tok_PE e : Tok_Comma : s) queue = sr (Tok_PI (FuncRun "" [e]) : Tok_RPar : Tok_Comma : s) queue
sr (Tok_RPar : Tok_PI (FuncRun name valLst) : s) queue = sr (Tok_PI (FuncRun "" [(FuncRet name valLst)]) : Tok_RPar : s) queue

sr (Tok_PI (FuncRun _ valLst) : Tok_RPar : Tok_Comma : Tok_PE e : s) queue = sr (Tok_PI (FuncRun "" (e:valLst)) : Tok_RPar : s) queue
sr (Tok_PI (FuncRun _ valLst) : Tok_RPar : Tok_Comma : Tok_PI (FuncRun name valLst') : s) queue = sr (Tok_PI (FuncRun "" ((FuncRet name valLst'):valLst)) : Tok_RPar : s) queue
sr (Tok_PI (FuncRun _ valLst) : Tok_RPar : Tok_LPar : Tok_PE (Var fName) : s) queue = sr (Tok_PI (FuncRun fName valLst) : s) queue
sr (Tok_PI (FuncRun name valLst) : Tok_PE (Var fName) : s) queue = sr (Tok_PI (FuncRun fName [(FuncRet name valLst)]) : s) queue
sr (Tok_PI (FuncRun _ valLst) : Tok_RPar : Tok_LPar : Tok_PI (Assign v ml (Var fName)) : s) queue = sr(Tok_PI (Assign v ml (FuncRet fName valLst)) : s) queue

-- CLASS INIT
sr (Tok_PI body@(Block b e) : Tok_PE (Var cName) : Tok_KW KW_Class : s) queue = sr (Tok_PI (Cls (Cl cName [] body)) : s) queue
sr (Tok_PI (Cls (Cl cName _ body)) : Tok_Mod ml : s) queue = sr (Tok_PI (Cls (Cl cName ml body)) : s) queue

-- CLASS CREATE
sr (Tok_PI (FuncRun cName valLst) : Tok_KW KW_New : s) queue = sr (Tok_PE (CCreate cName valLst) : s) queue

-- CLASS DOT OPERATOR
sr (Tok_PE (Var n) : Tok_Dot : Tok_PI (Assign v ml (Var clsObj)) : s) queue = sr (Tok_PI (Assign v ml (ClsGet clsObj (Var n))) : s) queue
sr (Tok_PE (Var n) : Tok_Dot : Tok_PI (Assign v ml (ClsGet x y)) : s) queue = 
    let newC = srFixClsGet (Var n) (ClsGet x y)
    in sr (Tok_PI (Assign v ml newC) : s) queue
sr (Tok_PI f@(FuncRun fName valLst) : Tok_Dot : Tok_PE (Var clsObj) : s) queue = sr (Tok_PI (ClsRun clsObj f) : s) queue
sr (Tok_PI c@(ClsRun _ _) : Tok_Dot : Tok_PE (Var clsObj) : s) queue = sr (Tok_PI (ClsRun clsObj c) : s) queue

sr (Tok_RPar : Tok_LPar : Tok_PI (Assign v ml c@(ClsGet x y)) : s) queue = 
    let newC = srFixClsGet' c
    in sr (Tok_PI (Assign v ml newC) : s) queue

sr (Tok_PE val@(Const _) : Tok_PI (Assign v ml c@(ClsGet x y)) : s) queue = 
    let newC = srFixClsGetFuncRet val c
    in sr (Tok_PI (Assign v ml newC) : s) queue

sr (Tok_PI f@(FuncRun _ valLst) : Tok_RPar : Tok_LPar : Tok_PI (Assign v ml c@(ClsGet _ _)) : s) queue = 
    let newC = srFixClsGetFuncRet' f c
    in sr (Tok_PI (Assign v ml newC) : s) queue

sr (Tok_PI (Assign var ml (Const x)) : Tok_Dot : Tok_PE (Var clsObj) : s) queue = sr (Tok_PI (ClsRun clsObj (Assign var ml (Const x))) : s) queue
sr (Tok_PI (Assign var ml (Var x)) : Tok_Dot : Tok_PE (Var clsObj) : s) queue = sr (Tok_PI (ClsRun clsObj (Assign var ml (Var x))) : s) queue

-- TYPES & TYPE CASTING
sr (Tok_PE e : Tok_UOp UnaType : s)                                 queue = sr (Tok_PE (Type e)  : s) queue
sr (Tok_PE e : Tok_RPar : Tok_Mod [ModType TypeInt] : Tok_LPar : s) queue = sr (Tok_PE (ToInt e) : s) queue
sr (Tok_PE e : Tok_RPar : Tok_Mod [ModType TypeDbl] : Tok_LPar : s) queue = sr (Tok_PE (ToDbl e) : s) queue
sr (Tok_PE e : Tok_RPar : Tok_Mod [ModType TypeBol] : Tok_LPar : s) queue = sr (Tok_PE (ToBol e) : s) queue
sr (Tok_PE e : Tok_RPar : Tok_Mod [ModType TypeStr] : Tok_LPar : s) queue = sr (Tok_PE (ToStr e) : s) queue
sr (Tok_PE e : Tok_RPar : Tok_Mod [ModType TypeChr] : Tok_LPar : s) queue = sr (Tok_PE (ToChr e) : s) queue

-- MISC
sr (Tok_EOL : s) queue = sr s queue
sr (Tok_SemiC : Tok_PI e : s) queue = sr (Tok_PI e : s) queue
sr (Tok_SemiC : Tok_PE e : s) queue = sr (Tok_PE e : s) queue
sr (Tok_RPar : Tok_PE e : Tok_LPar : s) queue = sr (Tok_PE e : s) queue
sr (Tok_RPar : Tok_PI e : Tok_LPar : s) queue = sr (Tok_PI e : s) queue
sr s (q0 : qRest) = sr (q0 : s) qRest
sr s [] = s

srAssign :: [Expr] -> [Instr]
srAssign [] = []
srAssign ((Var x):xs) = [Assign x [ModType TypeUDF] (Const NullVal)] ++ srAssign xs 

srTypedAssgn :: Expr -> [Mod] -> Instr
srTypedAssgn (Var x) modLst = let varType = getVarTypes modLst
                              in if (length varType == 0)
                              then (Assign x modLst (Const NullVal))
                              else case head varType of
                                   TypeInt -> (Assign x modLst (Const (NumVal 0)))
                                   TypeDbl -> (Assign x modLst (Const (DoubleVal 0.0)))
                                   TypeBol -> (Assign x modLst (Const (BoolVal False)))
                                   TypeStr -> (Assign x modLst (Const (StrVal "")))
                                   TypeChr -> (Assign x modLst (Const (CharVal ' ')))
                                   _       -> (Assign x modLst (Const NullVal))

srFixClsGet :: Expr -> Expr -> Expr
srFixClsGet exprAdd exprBase = case exprBase of
                               (ClsGet x y) -> (ClsGet x (srFixClsGet exprAdd y))
                               (Var x) -> (ClsGet x exprAdd)

srFixClsGet' :: Expr -> Expr
srFixClsGet' exprBase = case exprBase of
                               (ClsGet x y) -> (ClsGet x (srFixClsGet' y))
                               (Var x) -> FuncRet x []

srFixClsGetFuncRet :: Expr -> Expr -> Expr
srFixClsGetFuncRet exprParam exprBase = case exprBase of
                                        (ClsGet x y) -> (ClsGet x (srFixClsGetFuncRet exprParam y))
                                        (Var x) -> FuncRet x [exprParam]

srFixClsGetFuncRet' :: Instr -> Expr -> Expr
srFixClsGetFuncRet' exprAdd@(FuncRun _ valLst) exprBase = case exprBase of
                                                          (ClsGet x y) -> (ClsGet x (srFixClsGetFuncRet' exprAdd y))
                                                          (Var x) -> FuncRet x valLst

getVarTypes :: [Mod] -> [Type]
getVarTypes [] = []
getVarTypes (x:xs) = case x of
                (ModType t) -> [t] ++ getVarTypes xs
                _ -> getVarTypes xs

checkReadOnly :: [Mod] -> Bool
checkReadOnly [] = False
checkReadOnly (x:xs) = case x of
                       (ModReadOnly) -> True
                       _ -> checkReadOnly xs

isPrivatePublic :: [Mod] -> Mod
isPrivatePublic [] = ModPublic
isPrivatePublic (x:xs) = case x of
                         (ModPrivate) -> ModPrivate
                         (ModPublic)  -> ModPublic
                         _ -> isPrivatePublic xs

srFixAssign :: (Either Expr Instr) -> Instr -> Instr
srFixAssign (Left x)  y = srFixAssign'  x y
srFixAssign (Right x) y = srFixAssign'' x y

srFixAssign' :: Expr -> Instr -> Instr
srFixAssign' exprAdd exprBase = case exprBase of
                               (Assign x ml y@(Const x')) -> (Assign x ml exprAdd)
                               (Assign x ml y@(Var x')) -> (ChnAsgn x ml (Assign x' ml exprAdd))
                               (ChnAsgn x ml y) -> (ChnAsgn x ml (srFixAssign' exprAdd y))

srFixAssign'' :: Instr -> Instr -> Instr
srFixAssign'' exprAdd exprBase = case exprBase of
                                (Assign x ml y@(Var x')) -> (ChnAsgn x ml (ChnAsgn x' ml exprAdd))
                                (ChnAsgn x ml y) -> (ChnAsgn x ml (srFixAssign'' exprAdd y))

parseExpr :: [Token] -> [Instr]
parseExpr [] = []
parseExpr x = getParsed (sr [] x)

{- -}
getParsed :: [Token] -> [Instr]
getParsed []             = []
getParsed (Tok_PI p : s) = p : getParsed s
getParsed (x : xs)       = getParsed xs

{- ------------------------------------CODE EXECUTION----------------------------------- -}

eval :: Expr -> (Env, PrintStack) -> (Value, PrintStack)
eval (Const x) (env@(vL, fL, cL), ps)   = (x, ps)
eval (Var x)   (env@(vL, fL, cL), ps)   = (lookupVarList x vL, ps)

-- ARTITMETIC EXPRESSIONS
eval (ArExpr (Add e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((addVals v1 v2), ps'')
eval (ArExpr (Sub e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((subVals v1 v2), ps'')
eval (ArExpr (Mul e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((mulVals v1 v2), ps'')
eval (ArExpr (Div e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((divVals v1 v2), ps'')
eval (ArExpr (Exp e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((expVals v1 v2), ps'')
eval (ArExpr (Mod e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((modVals v1 v2), ps'')
eval (ArExpr (ShiftL e1 e2)) (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((shiftLVals v1 v2), ps'')
eval (ArExpr (ShiftR e1 e2)) (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((shiftRVals v1 v2), ps'')
eval (ArExpr (RotL e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((rotLVals v1 v2), ps'')
eval (ArExpr (RotR e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((rotRVals v1 v2), ps'')
eval (ArExpr (AAnd e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((aAndVals v1 v2), ps'')
eval (ArExpr (AOr e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((aOrVals v1 v2), ps'')
eval (ArExpr (AXor e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                      in let (v2, ps'') = (eval e2 (env, ps'))
                                                      in ((aXorVals v1 v2), ps'')
eval (ArExpr (Neg e1))       (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e1 (env, ps))
                                                      in (negVals myV, ps')
eval (ArExpr (Comp e1))      (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e1 (env, ps))
                                                      in (compVals myV, ps')
-- BOOLEAN EXPRESSIONS
eval (BlExpr(Eq e1 e2))    (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((eqVals v1 v2), ps'')
eval (BlExpr(NotEq e1 e2)) (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((notEqVals v1 v2), ps'')
eval (BlExpr(LeT e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((ltVals v1 v2), ps'')
eval (BlExpr(LTE e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((lteVals v1 v2), ps'')
eval (BlExpr(GrT e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((gtVals v1 v2), ps'')
eval (BlExpr(GTE e1 e2))   (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((gteVals v1 v2), ps'')
eval (BlExpr (BAnd e1 e2)) (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((bAndVals v1 v2), ps'')
eval (BlExpr (BOr e1 e2))  (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((bOrVals v1 v2), ps'')
eval (BlExpr (BXor e1 e2)) (env@(vL, fL, cL), ps) = let (v1, ps') = (eval e1 (env, ps))
                                                    in let (v2, ps'') = (eval e2 (env, ps'))
                                                    in ((bXorVals v1 v2), ps'')
eval (BlExpr(Not e1))      (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e1 (env, ps))
                                                    in (notVals myV, ps')
-- TYPE INFRENCE & TYPE CASTING
eval (Type e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                       in let myT = getType myV
                                       in let myTStr = typeToStr myT
                                       in (myTStr, ps')
eval (ToInt e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                        in (valToInt myV, ps')
eval (ToDbl e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                        in (valToDouble myV, ps')
eval (ToBol e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                        in (valToBool myV, ps')
eval (ToStr e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                        in (valToString myV, ps')
eval (ToChr e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval e (env, ps))
                                        in (valToChar myV, ps')
-- ARRAYS
eval (ArrInit ml valLst) (env@(vL, fL, cL), ps) = let valLst' = evalArrList valLst ml env
                                                  in (ArrVal valLst', ps)

eval (ArrGet aName index) (env@(vL, fL, cL), ps) = let arr = lookupVarList aName vL
                                                   in let (index', ps') = eval index (env, ps)
                                                   in case index' of
                                                      (NumVal index'') -> case arr of
                                                                          (NullVal) -> (NullVal, ps)
                                                                          (ArrVal valLst) -> (getArrAtIndex valLst index'', ps')
                                                      _ -> (NullVal, ps')

-- CLASSES
eval (ClsGet clsObj (Var clsVar)) (env@(vL, fL, cL), ps) =
    let obj = lookupVarList clsObj vL
    in case obj of
       (ObjVal (Cl cName ml (Block b lenv@(lVL, lFL, lCL)))) -> let (ml', myVal) = lookupVarList' clsVar lVL
                                                                in if (ModPublic `elem` ml')
                                                                   then (myVal, ps)
                                                                   else (NullVal, ps)
       _ -> (NullVal, ps)

eval (ClsGet clsObj f@(FuncRet fName valLst)) (env@(vL, fL, cL), ps) = 
    let obj = lookupVarList clsObj vL
    in case obj of
       (ObjVal (Cl cName ml (Block b lenv@(lVL, lFL, lCL)))) -> 
            let myFunc = lookupFuncList fName lFL
            in case myFunc of
            (Nothing) -> (NullVal, ps)
            (Just (Fn fName' varLst ml' body)) -> 
                if (ModPublic `elem` ml')
                then let (fVal, ps') = eval (FuncRet fName valLst) (lenv, ps)
                     in (fVal, ps')
                else (NullVal, ps)                                    
       _ -> (NullVal, ps)

eval (ClsGet clsObj c@(ClsGet _ _)) (env@(vL, fL, cL), ps) = 
    let obj = lookupVarList clsObj vL
    in case obj of
       (ObjVal (Cl cName ml (Block b lenv@(lVL, lFL, lCL)))) -> eval c (lenv, ps)                                  
       _ -> (NullVal, ps)

-- RETURNING FUNCTIONS
eval (FuncRet fName valLst) (env@(vL, fL, cL), ps) = 
    let func = lookupFuncList fName fL
    in case func of
       (Nothing) -> (NullVal, ps)
       (Just f)  -> let (nEnv@(nVL, nFL, nCL), ps') = exec (FuncRun fName valLst) (env, ps)
                    in (lookupVarList "return" nVL, ps')

-- CLASS CREATION
eval (CCreate cName valLst) (env@(vL, fL, cL), ps) = 
    let myClass = lookupClassList cName cL
    in case myClass of
       (Nothing) -> (NullVal, ps)
       (Just c@(Cl cName' ml' body@(Block b lenv@(lVL, lFL, lCL)))) -> 
            let (nEnv@(nVL, nFL, nCL), ps') = exec (Do b) (lenv, ps)
            in let createFunc = lookupFuncList "create" nFL
            in case createFunc of
               (Nothing) -> (ObjVal (Cl cName' ml' (Block b (nVL, nFL, nCL))), ps)
               (Just f@(Fn fName fVL fML body)) -> let (nEnv'@(nVL', nFL', nCL'), ps'') = exec (FuncRun fName valLst) (nEnv, ps')
                                                   in (ObjVal (Cl cName' ml' (Block b (nVL', nFL', nCL'))), ps'') 

-- MISC
eval (Ternary b e1 e2) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval (BlExpr b) (env, ps))
                                                in if (myV == (BoolVal True)) then (eval e1 (env, ps')) 
                                                                              else (eval e2 (env, ps'))

getArrAtIndex :: [Value] -> Int -> Value
getArrAtIndex [] index = NullVal
getArrAtIndex valLst index = case (itemOf index valLst) of
                             (Nothing) -> NullVal
                             (Just x) -> x


itemOf :: Int -> [a] -> Maybe a
x `itemOf` xs = let xslen = length xs 
                       in if ((abs x) > xslen) 
                          then Nothing 
                          else Just (xs !! (x `mod` xslen))

evalArrList :: [Expr] -> Modifiers -> Env -> [Value]
evalArrList [] ml env = []
evalArrList (e:es) ml@[ModType (TypeArr x)] env = let (val, ps') = eval e (env, [])
                                                  in let typeV = getType val
                                                  in let newV = if (typeV == x)
                                                                then val
                                                                else getTypeDefValue x
                                                  in [newV] ++ evalArrList es ml env
evalArrList (e:es) ml env = let (val, ps') = eval e (env, [])
                            in let typeV = getType val
                            in let typeI = getVarTypes ml
                            in let newV = if (typeV `elem` typeI) || (TypeUDF `elem` typeI)
                                          then val
                                          else getTypeDefValue (head typeI)
                            in [newV] ++ evalArrList es ml env

valToInt :: Value -> Value
valToInt (NumVal x) = NumVal x
valToInt (DoubleVal  x) = NumVal (round x)
valToInt (BoolVal x) = if (x) then NumVal 1 else NumVal 0
valToInt (StrVal x) = let n = (read x :: Int)
                      in NumVal n
valToInt (CharVal x) = NumVal (digitToInt x)
valToInt _ = NumVal 0

valToDouble :: Value -> Value
valToDouble (NumVal x) = DoubleVal (fromIntegral x)
valToDouble (DoubleVal x) = DoubleVal x
valToDouble (BoolVal x) = if (x) then DoubleVal 1 else DoubleVal 0
valToDouble (StrVal x) = let n = (read x :: Double)
                         in DoubleVal n
valToDouble (CharVal x) = let i = digitToInt x
                          in DoubleVal (fromIntegral i)
valToDouble _ = DoubleVal 0.0

valToBool :: Value -> Value
valToBool (NumVal 0)     = BoolVal False
valToBool (NumVal _)     = BoolVal True
valToBool (DoubleVal  0) = BoolVal False
valToBool (DoubleVal  _) = BoolVal True
valToBool (BoolVal x)    = BoolVal x
valToBool _              = BoolVal False

valToString :: Value -> Value
valToString (NumVal x) = StrVal (show x)
valToString (DoubleVal  x) = StrVal (show x)
valToString (BoolVal x) = if (x) then StrVal "True" else StrVal "False"
valToString (StrVal x) = StrVal x
valToString (CharVal x) = StrVal [x]
valToString _ = StrVal ""

valToChar :: Value -> Value
valToChar n@(NumVal x) = let (StrVal str) = valToString n
                       in CharVal (head str)
valToChar d@(DoubleVal  x) = let (StrVal str) = valToString d
                           in CharVal (head str)
valToChar (BoolVal x) = if (x) then CharVal 'T' else CharVal 'F'
valToChar (StrVal x) = CharVal (head x)
valToChar (CharVal x) = CharVal x
valToChar _ = CharVal ' '

getType :: Value -> Type
getType (NumVal _) = TypeInt
getType (DoubleVal _) = TypeDbl
getType (BoolVal _) = TypeBol
getType (StrVal _) = TypeStr
getType (CharVal _) = TypeChr
getType (ArrVal (x:xs)) = (TypeArr (getType x))
getType (ObjVal c@(Cl i _ _)) = (TypeObj i) 
getType (NullVal) = TypeUDF
getType _ = TypeUDF

typeToStr :: Type -> Value
typeToStr TypeInt     = StrVal "typeInt"
typeToStr TypeDbl     = StrVal "typeDbl"
typeToStr TypeBol     = StrVal "typeBol"
typeToStr TypeStr     = StrVal "typeStr"
typeToStr TypeChr     = StrVal "typeChr"
typeToStr (TypeArr x) = StrVal "typeArr"
typeToStr (TypeObj x) = StrVal ("typeObj " ++ x)
typeToStr _           = StrVal "typeUdf"

getTypeDefValue :: Type -> Value
getTypeDefValue (TypeInt)   = (NumVal 0)
getTypeDefValue (TypeDbl)   = (DoubleVal 0.0)
getTypeDefValue (TypeBol)   = (BoolVal False)
getTypeDefValue (TypeStr)   = (StrVal "")
getTypeDefValue (TypeChr)   = (CharVal ' ')
getTypeDefValue (TypeArr x) = (ArrVal [])
getTypeDefValue _           = (NullVal)

compVals :: Value -> Value
compVals (NumVal x) = NumVal (complement x)
compVals (BoolVal x) = let x' = if (x == True) then 1 else 0
                       in NumVal (complement x')
compVals _ = NullVal

notVals :: Value -> Value
notVals (BoolVal x) = BoolVal (not x)
notVals _ = NullVal

negVals :: Value -> Value
negVals (NumVal x) = NumVal (-x)
negVals (DoubleVal x) = DoubleVal (-x)
negVals (BoolVal x) = let x' = if (x == True) then 1 else 0
                      in NumVal (-x')
negVals _ = NullVal

eqVals :: Value -> Value -> Value
eqVals (NumVal x)    (NumVal y)    = BoolVal (x == y)
eqVals (DoubleVal x) (DoubleVal y) = BoolVal (x == y)
eqVals (BoolVal x)   (BoolVal y)   = BoolVal (x == y)
eqVals (StrVal x)    (StrVal y)    = BoolVal (x == y)
eqVals (CharVal x)   (CharVal y)   = BoolVal (x == y)
eqVals NullVal       NullVal       = BoolVal True
eqVals _ _ = NullVal

notEqVals :: Value -> Value -> Value
notEqVals (NumVal x)    (NumVal y)    = BoolVal (x /= y)
notEqVals (DoubleVal x) (DoubleVal y) = BoolVal (x /= y)
notEqVals (BoolVal x)   (BoolVal y)   = BoolVal (x /= y)
notEqVals (StrVal x)    (StrVal y)    = BoolVal (x /= y)
notEqVals (CharVal x)   (CharVal y)   = BoolVal (x /= y)
notEqVals NullVal       NullVal       = BoolVal True
notEqVals _ _ = NullVal

aAndVals :: Value -> Value -> Value
aAndVals (NumVal x)  (NumVal y)  = NumVal ((.&.) x y)
aAndVals _ _ = NullVal

bAndVals :: Value -> Value -> Value
bAndVals (BoolVal x) (BoolVal y) = BoolVal (x && y)
bAndVals _ _ = NullVal

aOrVals :: Value -> Value -> Value
aOrVals (NumVal x)  (NumVal y)  = NumVal ((.|.) x y)
aOrVals _ _ = NullVal

bOrVals :: Value -> Value -> Value
bOrVals (BoolVal x) (BoolVal y) = BoolVal (x || y)
bOrVals _ _ = NullVal

aXorVals :: Value -> Value -> Value
aXorVals (NumVal x) (NumVal y)  = NumVal (xor x y)
aXorVals _ _ = NullVal

bXorVals :: Value -> Value -> Value
bXorVals (BoolVal x) (BoolVal y) = BoolVal (booleanXor x y)
bXorVals _ _ = NullVal

booleanXor :: Bool -> Bool -> Bool
booleanXor a b = (a || b) && not (a && b)

shiftLVals :: Value -> Value -> Value
shiftLVals (NumVal x) (NumVal y)  = NumVal (shiftL x y)
shiftLVals (NumVal x) (BoolVal y) = let y' = if (y == True) then 1 else 0
                                    in NumVal (shiftL x y')
shiftLVals (BoolVal x) (NumVal y) = let x' = if (x == True) then 1 else 0
                                    in NumVal (shiftL x' y)
shiftLVals _ _ = NullVal

shiftRVals :: Value -> Value -> Value
shiftRVals (NumVal x) (NumVal y)  = NumVal (shiftR x y)
shiftRVals (NumVal x) (BoolVal y) = let y' = if (y == True) then 1 else 0
                                    in NumVal (shiftR x y')
shiftRVals (BoolVal x) (NumVal y) = let x' = if (x == True) then 1 else 0
                                    in NumVal (shiftR x' y)
shiftRVals _ _ = NullVal

rotLVals :: Value -> Value -> Value
rotLVals (NumVal x)  (NumVal y)  = NumVal (rotateL x y)
rotLVals (NumVal x)  (BoolVal y) = let y' = if (y == True) then 1 else 0
                                   in NumVal (rotateL x y')
rotLVals (BoolVal x) (NumVal y)  = let x' = if (x == True) then 1 else 0
                                   in NumVal (rotateL x' y)
rotLVals _ _ = NullVal

rotRVals :: Value -> Value -> Value
rotRVals (NumVal x)  (NumVal y)  = NumVal (rotateR x y)
rotRVals (NumVal x)  (BoolVal y) = let y' = if (y == True) then 1 else 0
                                   in NumVal (rotateR x y')
rotRVals (BoolVal x) (NumVal y)  = let x' = if (x == True) then 1 else 0
                                   in NumVal (rotateR x' y)
rotRVals _ _ = NullVal

ltVals :: Value -> Value -> Value
ltVals (NumVal x)    (NumVal y)    = BoolVal (x < y)
ltVals (NumVal x)    (DoubleVal y) = BoolVal ((fromIntegral x :: Double) < y)
ltVals (NumVal x)    (BoolVal y)   = let y' = if (y == True) then 1 else 0
                                     in BoolVal (x < y')
ltVals (DoubleVal x) (DoubleVal y) = BoolVal (x < y)
ltVals (DoubleVal x) (NumVal y)    = BoolVal (x < (fromIntegral y :: Double))
ltVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1.0 else 0.0
                                     in BoolVal (x < y')
ltVals (BoolVal x)   (BoolVal y)   = BoolVal (x < y)
ltVals (BoolVal x)   (NumVal y)    = let x' = if (x == True) then 1 else 0
                                     in BoolVal (x' < y)
ltVals (BoolVal x)   (DoubleVal y) = let x' = if (x == True) then 1.0 else 0.0
                                     in BoolVal (x' < y)
ltVals _ _ = NullVal

lteVals :: Value -> Value -> Value
lteVals (NumVal x)    (NumVal y)    = BoolVal (x <= y)
lteVals (NumVal x)    (DoubleVal y) = BoolVal ((fromIntegral x :: Double) <= y)
lteVals (NumVal x)    (BoolVal y)   = let y' = if (y == True) then 1 else 0
                                      in BoolVal (x <= y')
lteVals (DoubleVal x) (DoubleVal y) = BoolVal (x <= y)
lteVals (DoubleVal x) (NumVal y)    = BoolVal (x <= (fromIntegral y :: Double))
lteVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1.0 else 0.0
                                      in BoolVal (x <= y')
lteVals (BoolVal x)   (BoolVal y)   = BoolVal (x <= y)
lteVals (BoolVal x)   (NumVal y)    = let x' = if (x == True) then 1 else 0
                                      in BoolVal (x' <= y)
lteVals (BoolVal x)   (DoubleVal y) = let x' = if (x == True) then 1.0 else 0.0
                                      in BoolVal (x' <= y)
lteVals _ _ = NullVal

gtVals :: Value -> Value -> Value
gtVals (NumVal x)    (NumVal y)    = BoolVal (x > y)
gtVals (NumVal x)    (DoubleVal y) = BoolVal ((fromIntegral x :: Double) > y)
gtVals (NumVal x)    (BoolVal y)   = let y' = if (y == True) then 1 else 0
                                     in BoolVal (x > y')
gtVals (DoubleVal x) (DoubleVal y) = BoolVal (x > y)
gtVals (DoubleVal x) (NumVal y)    = BoolVal (x > (fromIntegral y :: Double))
gtVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1.0 else 0.0
                                     in BoolVal (x > y')
gtVals (BoolVal x)   (BoolVal y)   = BoolVal (x > y)
gtVals (BoolVal x)   (NumVal y)    = let x' = if (x == True) then 1 else 0
                                     in BoolVal (x' > y)
gtVals (BoolVal x)   (DoubleVal y) = let x' = if (x == True) then 1.0 else 0.0
                                     in BoolVal (x' > y)
gtVals _ _ = NullVal

gteVals :: Value -> Value -> Value
gteVals (NumVal x)    (NumVal y)    = BoolVal (x >= y)
gteVals (NumVal x)    (DoubleVal y) = BoolVal ((fromIntegral x :: Double) >= y)
gteVals (NumVal x)    (BoolVal y)   = let y' = if (y == True) then 1 else 0
                                      in BoolVal (x >= y')
gteVals (DoubleVal x) (DoubleVal y) = BoolVal (x >= y)
gteVals (DoubleVal x) (NumVal y)    = BoolVal (x >= (fromIntegral y :: Double))
gteVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1.0 else 0.0
                                      in BoolVal (x >= y')
gteVals (BoolVal x)   (BoolVal y)   = BoolVal (x >= y)
gteVals (BoolVal x)   (NumVal y)    = let x' = if (x == True) then 1 else 0
                                      in BoolVal (x' >= y)
gteVals (BoolVal x)   (DoubleVal y) = let x' = if (x == True) then 1.0 else 0.0
                                      in BoolVal (x' >= y)
gteVals _ _ = NullVal

modVals :: Value -> Value -> Value
modVals (NumVal x) (NumVal y)   = NumVal (x `mod` y)
modVals (BoolVal x) (BoolVal y) = let x' = if (x == True) then 1 else 0
                                  in let y' = if (y == True) then 1 else 0
                                  in NumVal (x' `mod` y')
modVals _ _ = NullVal

expVals :: Value -> Value -> Value
expVals (NumVal x) (NumVal y)   = NumVal (x ^ y)
expVals (BoolVal x) (BoolVal y) = let x' = if (x == True) then 1 else 0
                                  in let y' = if (y == True) then 1 else 0
                                  in NumVal (x' ^ y')
expVals _ _ = NullVal

divVals :: Value -> Value -> Value
divVals (NumVal x) (NumVal y)   = NumVal (x `div` y)
divVals (BoolVal x) (BoolVal y) = let x' = if (x == True) then 1 else 0
                                  in let y' = if (y == True) then 1 else 0
                                  in NumVal (x' `div` y')
divVals _ _ = NullVal

mulVals :: Value -> Value -> Value
mulVals (NumVal x) (NumVal y)       = NumVal (x * y)
mulVals (DoubleVal x) (DoubleVal y) = DoubleVal (x * y)
mulVals (BoolVal x) (BoolVal y)     = let x' = if (x == True) then 1 else 0
                                      in let y' = if (y == True) then 1 else 0
                                      in NumVal (x' * y')
mulVals _ _ = NullVal

subVals :: Value -> Value -> Value
subVals (NumVal x) (NumVal y)       = NumVal (x - y)
subVals (NumVal x) (DoubleVal y)    = DoubleVal ((fromIntegral x :: Double) - y)
subVals (NumVal x) (BoolVal y)      = let y' = if (y == True) then 1 else 0
                                      in NumVal (x - y')
subVals (DoubleVal x) (DoubleVal y) = DoubleVal (x - y)
subVals (DoubleVal x) (NumVal y)    = DoubleVal (x - (fromIntegral y :: Double))
subVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1 else 0
                                      in DoubleVal (x - y')
subVals (BoolVal x) (BoolVal y)     = let x' = if (x == True) then 1 else 0
                                      in let y' = if (y == True) then 1 else 0
                                      in NumVal (x' - y')
subVals (BoolVal x) (NumVal y)      = let x' = if (x == True) then 1 else 0
                                      in NumVal (x' - y)
subVals (BoolVal x) (DoubleVal y)   = let x' = if (x == True) then 1.0 else 0.0
                                      in DoubleVal (x' - y)
subVals (NullVal) (NullVal)         = NullVal
subVals _ _ = NullVal
                  
addVals :: Value -> Value -> Value
addVals (NumVal x) (NumVal y)       = NumVal (x + y)
addVals (NumVal x) (DoubleVal y)    = DoubleVal ((fromIntegral x :: Double) + y)
addVals (NumVal x) (BoolVal y)      = let y' = if (y == True) then 1 else 0
                                      in NumVal (x + y')
addVals (NumVal x) (StrVal y)       = StrVal (show x ++ y)

addVals (DoubleVal x) (DoubleVal y) = DoubleVal (x + y)
addVals (DoubleVal x) (NumVal y)    = DoubleVal (x + (fromIntegral y :: Double))
addVals (DoubleVal x) (BoolVal y)   = let y' = if (y == True) then 1.0 else 0.0
                                      in DoubleVal (x + y')
addVals (DoubleVal x) (StrVal y)    = StrVal (show x ++ y)

addVals (BoolVal x) (BoolVal y)     = let x' = if (x == True) then 1 else 0
                                      in let y' = if (y == True) then 1 else 0
                                      in NumVal (x' + y')
addVals (BoolVal x) (NumVal y)      = let x' = if (x == True) then 1 else 0
                                      in NumVal (x' + y)
addVals (BoolVal x) (DoubleVal y)   = let x' = if (x == True) then 1.0 else 0.0
                                      in DoubleVal (x' + y)
addVals (BoolVal x) (StrVal y)    = StrVal (show x ++ y)

addVals (StrVal x) (StrVal y)       = StrVal (x ++ y)
addVals (StrVal x) (CharVal y)      = StrVal (x ++ [y])
addVals (StrVal x) (NumVal y)       = StrVal (x ++ show y)
addVals (StrVal x) (DoubleVal y)    = StrVal (x ++ show y)
addVals (StrVal x) (BoolVal y)      = StrVal (x ++ show y)
addVals (StrVal x) (NullVal)        = StrVal (x ++ "NULL")

addVals (CharVal x) (CharVal y)     = StrVal ([x] ++ [y])
addVals (CharVal x) (StrVal y)      = StrVal ([x] ++ y)
addVals (NullVal) (NullVal)         = NullVal
addVals _ _ = NullVal


exec :: Instr -> (Env, PrintStack) -> (Env, PrintStack)
-- ASSIGNMENT
exec (Assign i mL e) (env@(vL, fL, cL), ps) = let (v, ps') = eval e (env, ps)
                                              in let vL' = setVarList i mL v vL
                                              in ((vL', fL, cL), ps')

exec (ChnAsgn i mL e@(Assign i' mL' e'))  (env@(vL, fL, cL), ps) = let (nEnv@(nVL, nFL, nCL), ps') = exec e (env, ps)
                                                                   in let (v, ps'') = eval (Var i') (nEnv, ps')
                                                                   in let nVL' = setVarList i mL v nVL
                                                                   in ((nVL', nFL, nCL), ps'')
exec (ChnAsgn i mL e@(ChnAsgn i' mL' e')) (env@(vL, fL, cL), ps) = let (nEnv@(nVL, nFL, nCL), ps') = exec e (env, ps)
                                                                   in let (v, ps'') = eval (Var i') (nEnv, ps')
                                                                   in let nVL' = setVarList i mL v nVL
                                                                   in ((nVL', nFL, nCL), ps'')

-- LOCALIZED CODE BLOCKS
exec (Block [] lenv@(lVL, lFL, lCL)) (genv@(gVL, gFL, gCL), ps) = let mVL = (mergeLocalVarList gVL lVL)
                                                                  in let mFL = (mergeLocalFuncList gFL lFL)
                                                                  in let mCL = (mergeLocalClassList gCL lCL)
                                                                  in ((mVL, mFL, mCL), ps)

exec (Block (e : es) lenv@(lVL, lFL, lCL)) (genv@(gVL, gFL, gCL), ps) = let cVL = (combineVarList gVL lVL)
                                                                        in let cFL = (combineFuncList gFL lFL)
                                                                        in let cCL = (combineClassList gCL lCL)
                                                                        in let (nEnv@(nVL, nFL, nCL), ps') = exec e ((cVL, cFL, cCL), ps)
                                                                        in exec (Block es nEnv) (genv, ps')

-- GLOBAL CODE BLOCKS
exec (Do [])       (env@(vL, fL, cL), ps) = (env, ps)
exec (Do (e : es)) (env@(vL, fL, cL), ps) = let (nEnv@(nVL, nFL, nCL), ps') = exec e (env, ps)
                                            in exec (Do es) (nEnv, ps')

-- ARRAY SETTING
exec (ArrSet aName index val) (env@(vL, fL, cL), ps) = let (val', ps') = eval val (env, ps)
                                                       in let ((NumVal ind'), ps'') = eval index (env, ps')
                                                       in let arr = lookupVarList aName vL
                                                       in case arr of
                                                          (NullVal)    -> (env, ps')
                                                          (ArrVal lst) -> let newArrLst = ArrVal (arrSetIndex lst ind' 0 val')
                                                                          in let nVL = setVarList' aName [] newArrLst vL      -- MOD LIST SHOULDNT BE EMPTY
                                                                          in ((nVL, fL, cL), ps'')

-- IF STATEMENTS
exec (If b e) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval b (env, ps))
                                       in if (myV == (BoolVal True)) 
                                          then exec e (env, ps') 
                                          else (env, ps')

exec (IfElse b e1 e2) (env@(vL, fL, cL), ps) = let (myV, ps') = (eval b (env, ps))
                                               in if (myV == (BoolVal True)) 
                                                  then exec e1 (env, ps') 
                                                  else exec e2 (env, ps')

-- SWITCH STATEMENTS
exec (Switch myCase block@(Block b lenv)) (env@(vL, fL, cL), ps) = let (mCV, ps') = (eval myCase (env, ps))
                                                                   in execSwitch mCV block (env, ps')

-- FOR LOOPS
exec (For (Block ib _) cond (Block ub _) body@(Block b lenv)) (env@(vL, fL, cL), ps) = let (nEnv, ps') = exec (Do ib) (env, ps)
                                                                                       in execLoop cond (Block (ub ++ b) nEnv) (env, ps')

exec (For init cond (Block ub _) body@(Block b lenv)) (env@(vL, fL, cL), ps) = let (nEnv, ps') = exec init (env, ps)
                                                                               in execLoop cond (Block (ub ++ b) nEnv) (env, ps')

exec (For (Block ib _) cond upd body@(Block b lenv)) (env@(vL, fL, cL), ps) = let (nEnv, ps') = exec (Do ib) (env, ps)
                                                                              in execLoop cond (Block (upd:b) nEnv) (env, ps')

exec (For init cond upd body@(Block b lenv)) (env@(vL, fL, cL), ps) = let (nEnv, ps') = exec init (env, ps)
                                                                      in execLoop cond (Block (upd:b) nEnv) (env, ps')

exec (ForIn v list body) (env@(vL, fL, cL), ps) = let (list', ps') = (eval list (env, ps))
                                                  in execLoopList v list' body (env, ps')

-- WHILE / DO WHILE LOOPS
exec (While cond (Block b lenv@(lVL, lFL, lCL))) (genv@(gVL, gFL, gCL), ps) = let nVL = (mergeLocalVarList gVL lVL)
                                                                              in let nFL = (mergeLocalFuncList gFL lFL)
                                                                              in let nCL = (mergeLocalClassList gCL lCL)
                                                                              in execLoop cond (Block b (nVL, nFL, nCL)) (genv, ps)

-- FUNCTIONS
exec (Func f@(Fn name vl ml (Block b lenv@(lVL, lFL, lCL)))) (env@(vL, fL, cL), ps) = let lFL' = setFuncList name f lFL
                                                                                      in let f' = Fn name vl ml (Block b (lVL, lFL', lCL))
                                                                                      in let fL' = setFuncList name f' fL
                                                                                      in ((vL, fL', cL), ps)

exec (FuncRun fName valLst) (genv@(gVL, gFL, gCL), ps) = 
    case lookupFuncList fName gFL of
    (Nothing) -> (genv, ps)
    (Just (Fn fName' varLst ml e@(Block exprLst lenv@(lVL, lFL, lCL)))) -> 
        let pvv = (pairVarsVals varLst valLst)
        in let retType = getVarTypes ml
        in let retDefVal = if (length retType == 0) 
                           then NullVal 
                           else let firstVarType = head retType
                                in getTypeDefValue firstVarType
        in let gVL' = setVarList ("return") ml retDefVal gVL
        in exec (Block (pvv ++ exprLst) lenv) ((gVL', gFL, gCL), ps)

-- CLASSES
exec (Cls c@(Cl name ml block)) (env@(vL, fL, cL), ps) = let cL' = setClassList name c cL
                                                         in ((vL, fL, cL'), ps)

exec (ClsRun clsObj f@(FuncRun fName valLst)) (genv@(gVL, gFL, gCL), ps) = 
    let obj = lookupVarList clsObj gVL
    in case obj of
       (NullVal) -> (genv, ps)
       (ObjVal (c@(Cl cName ml body@(Block b lenv@(lVL, lFL, lCL))))) -> 
            let func = lookupFuncList fName lFL
            in case func of
               (Nothing) -> (genv, ps)
               (Just (Fn _ _ ml' body'@(Block b' lenv'))) -> 
                    if (ModPublic `elem` ml')
                    then let (nenv, ps') = exec f (lenv, ps)
                         in let newC = (Const (ObjVal (Cl cName ml (Block b nenv))))
                         in exec (Assign clsObj ml newC) (genv, ps')
                    else (genv, ps)

exec (ClsRun clsObj a@(Assign var ml val)) (genv@(gVL, gFL, gCL), ps) = 
    let obj = lookupVarList clsObj gVL
    in case obj of
       (NullVal) -> (genv, ps)
       (ObjVal (c@(Cl cName ml body@(Block b lenv@(lVL, lFL, lCL))))) -> 
            let (ml', var') = lookupVarList' var lVL
            in if (ModPublic `elem` ml')
               then let (nenv, ps') = exec a (lenv, ps)
                    in let newC = (Const (ObjVal (Cl cName ml (Block b nenv))))
                    in exec (Assign clsObj ml newC) (genv, ps')
               else (genv, ps)

exec (ClsRun clsObj c@(ClsRun _ _)) (genv@(gVL, gFL, gCL), ps) = 
    let obj = lookupVarList clsObj gVL
    in case obj of
       (NullVal) -> (genv, ps)
       (ObjVal ((Cl cName ml body@(Block b lenv@(lVL, lFL, lCL))))) -> 
        let (nenv, ps' ) = exec c (lenv, ps)
        in let newC = (Const (ObjVal (Cl cName ml (Block b nenv))))
           in exec (Assign clsObj ml newC) (genv, ps')

-- FREE VARIABLE
exec (Delete i) (env@(vL, fL, cL), ps) = let vL' = rmVarList i vL
                                         in ((vL', fL, cL), ps)

-- OUTPUT
exec (Print msg) (env@(vL, fL, cL), ps) = let (msg', ps') = eval msg (env, ps)
                                          in (env, (msg' : ps'))

-- FAILED EXEC
exec x (env, ps) = exec (Print (Const (StrVal ("Failed to execute command: '" ++ show x ++ "'\n")))) (env, ps)

            --  List     Ind    cInd    Val       Ret. Lst
arrSetIndex :: [Value] -> Int -> Int -> Value -> [Value]
arrSetIndex (v:vs) index cInd val | cInd == index = val     : vs
arrSetIndex []     index cInd val | cInd == index = [val]
arrSetIndex (v:vs) index cInd val | cInd /= index = v       : arrSetIndex vs index (cInd + 1) val 
arrSetIndex []     index cInd val | cInd /= index = NullVal : arrSetIndex [] index (cInd + 1) val 


execList :: [Instr] -> (Env, PrintStack) -> (Env, PrintStack)
execList []     (env@(vL, fL, cL), ps) = (env, ps)
execList (x:xs) (env@(vL, fL, cL), ps) = let (env', ps') = exec x (env, ps) 
                                         in execList xs (env',ps')

execSwitch :: Value -> Instr -> (Env, PrintStack) -> (Env, PrintStack)
execSwitch v (Block ((Default exeBlock):bs) lenv) (genv@(gVL, gFL, gCL), ps) = exec exeBlock (genv, ps)
execSwitch v (Block ((Case  c exeBlock):bs) lenv) (genv@(gVL, gFL, gCL), ps) = let (checkVal, ps') = (eval c (genv, ps))
                                                                               in if (checkVal == v) then exec exeBlock (genv, ps') 
                                                                                                     else execSwitch v (Block bs lenv) (genv, ps')

execLoop :: Expr -> Instr -> (Env, PrintStack) -> (Env, PrintStack)
execLoop cond body@(Block b lenv@(lVL, lFL, lCL)) (genv@(gVL, gFL, gCL), ps) = let (retVal, ps') = (eval cond (lenv, ps)) 
                                                                               in let retValType = getType retVal
                                                                               in if (retVal == (BoolVal False) || (retValType /= TypeBol)) then 
                                                                                     let mVL = (mergeLocalVarList gVL lVL)
                                                                                     in let mFL = (mergeLocalFuncList gFL lFL)
                                                                                     in let mCL = (mergeLocalClassList gCL lCL)
                                                                                     in ((mVL, mFL, gCL), ps')
                                                                                  else
                                                                                     let (nEnv, ps'') = exec body (lenv, ps)
                                                                                     in execLoop cond (Block b nEnv) (genv, ps'')

execLoopList :: Expr -> Value -> Instr -> (Env, PrintStack) -> (Env, PrintStack)
execLoopList v (StrVal list) body@(Block b lenv@(lVL, lFL, lCL)) (genv@(gVL, gFL, gCL), ps) | (length list == 0) = let mVL = (mergeLocalVarList gVL lVL)
                                                                                                                   in let mFL = (mergeLocalFuncList gFL lFL)
                                                                                                                   in let mCL = (mergeLocalClassList gCL lCL)
                                                                                                                   in ((mVL, mFL, mCL), ps)

execLoopList var@(Var v) (StrVal (l:ls)) body@(Block b lenv) (genv@(gVL, gFL, gCL), ps) = let assignExpr = Assign v [] (Const (CharVal l))
                                                                                          in let (nEnv, ps') = exec assignExpr (lenv, ps)
                                                                                          in let (nEnv', ps'') = exec (Block b nEnv) (genv, ps')
                                                                                          in execLoopList var (StrVal ls) (Block b nEnv') (genv, ps'')

reverseList xs = foldl (\x y -> y:x) [] xs

            -- Variables   Values   Def. Vals
pairVarsVals :: [Instr] -> [Expr] -> [Instr]
pairVarsVals [] vals = []
pairVarsVals vars [] = vars
pairVarsVals ((Assign i ml exp):varRest) (val:valRest) = let newA = Assign i ml val
                                                         in [newA] ++ pairVarsVals varRest valRest

-- MANAGE VARIABLE ENVIORMENT
setVarList :: Ident -> Modifiers -> Value -> VarList -> VarList
setVarList i ml v [] = 
    let typeV = getType v
    in let typeI = getVarTypes ml
    in if length typeI == 0
       then [(i, ml, v)] -- UPDATE, Var has no type restrictions
       else if (typeV `elem` typeI) || (TypeUDF `elem` typeI)
            then [(i, ml, v)] -- UPDATE, Var is being set to matching type
            else let defV = getTypeDefValue (head typeI)
                 in [(i, ml, defV)] -- DONT UPDATE, Set var to default value

setVarList i ml v ((i', ml', v') : rest) | i' == i =
    let typeV = getType v
    in case checkReadOnly ml' of
       (True)  -> (i', ml', v') : rest -- DONT UPDATE, Var is ReadOnly
       (False) -> let typeI = getVarTypes ml'
                  in if length typeI == 0
                     then (i, ml', v) : rest -- UPDATE, Var has no type restrictions
                     else if (typeV `elem` typeI) || (TypeUDF `elem` typeI)
                          then (i, ml', v) : rest -- UPDATE, Var is being set to matching type
                          else let defV = getTypeDefValue (head typeI)
                               in (i, ml', v') : rest -- DONT UPDATE

setVarList i ml v ((i', ml', v') : rest) | i' /= i = (i', ml', v') : setVarList i ml v rest

setVarList' :: Ident -> Modifiers -> Value -> VarList -> VarList
setVarList' i ml v []              = [(i, ml, v)]
setVarList' i ml v ((x, ml', y) : rest) | x == i = (x, ml', v) : rest
setVarList' i ml v ((x, ml', y) : rest) | x /= i = (x, ml', y) : setVarList' i ml v rest

rmVarList :: Ident -> VarList -> VarList
rmVarList i []              = []
rmVarList i ((i', ml, y) : rest) | i' == i = rest
rmVarList i ((i', ml, y) : rest) | i' /= i = (i', ml, y) : rmVarList i rest

lookupVarList :: Ident -> VarList -> Value
lookupVarList i []              = NullVal
lookupVarList i ((i', ml, y) : rest) = if (i' == i) then y else lookupVarList i rest

lookupVarList' :: Ident -> VarList -> (Modifiers, Value)
lookupVarList' i []              = ([], NullVal)
lookupVarList' i ((i', ml, y) : rest) = if (i' == i) then (ml, y) else lookupVarList' i rest

existsInVarList :: Ident -> VarList -> Bool
existsInVarList i []              = False
existsInVarList i ((x, ml, y) : rest) = if (x == i) then True else existsInVarList i rest

mergeLocalVarList :: VarList -> VarList -> VarList
mergeLocalVarList [] _ = []
mergeLocalVarList glob [] = glob
mergeLocalVarList glob ((lI, lMl, lv) : locRest) = if (existsInVarList lI glob) 
                                                   then (setVarList' lI lMl lv (mergeLocalVarList glob locRest))
                                                   else (mergeLocalVarList glob locRest)

combineVarList :: VarList -> VarList -> VarList
combineVarList [] [] = []
combineVarList [] loc = loc
combineVarList glob [] = glob
combineVarList glob ((lI, lMl, lV) : locRest) = (setVarList' lI lMl lV (combineVarList glob locRest))

-- MANAGE FUNCTION ENVIORMENT
setFuncList :: Ident -> Function -> FuncList -> FuncList
setFuncList i f []              = [(i, f)]
setFuncList i f ((x, y) : rest) | x == i = (x, f) : rest
setFuncList i f ((x, y) : rest) | x /= i = (x, y) : setFuncList i f rest

lookupFuncList :: Ident -> FuncList -> Maybe Function
lookupFuncList i []                  = Nothing
lookupFuncList i ((x, y) : rest) = if (x == i) then Just y else lookupFuncList i rest

existsInFuncList :: Ident -> FuncList -> Bool
existsInFuncList i []                  = False
existsInFuncList i ((x, y) : rest) = if (x == i) then True else existsInFuncList i rest

mergeLocalFuncList :: FuncList -> FuncList -> FuncList
mergeLocalFuncList [] _ = []
mergeLocalFuncList glob [] = glob
mergeLocalFuncList glob ((lI, lf) : locRest) = if (existsInFuncList lI glob) 
                                                    then (setFuncList lI lf (mergeLocalFuncList glob locRest))
                                                    else (mergeLocalFuncList glob locRest)

combineFuncList :: FuncList -> FuncList -> FuncList
combineFuncList [] [] = []
combineFuncList [] loc = loc
combineFuncList glob [] = glob
combineFuncList glob ((lI, lF) : locRest) = (setFuncList lI lF (combineFuncList glob locRest))

-- MANAGE CLASS ENVIORMENT
setClassList :: Ident -> Class -> ClassList -> ClassList
setClassList i f []              = [(i, f)]
setClassList i f ((x, y) : rest) | x == i = (x, f) : rest
setClassList i f ((x, y) : rest) | x /= i = (x, y) : setClassList i f rest

mergeLocalClassList :: ClassList -> ClassList -> ClassList
mergeLocalClassList [] _ = []
mergeLocalClassList glob [] = glob
mergeLocalClassList glob ((lI, lv) : locRest) = if (existsInClassList lI glob) 
                                                     then (setClassList lI lv (mergeLocalClassList glob locRest))
                                                     else (mergeLocalClassList glob locRest)

combineClassList :: ClassList -> ClassList -> ClassList
combineClassList [] [] = []
combineClassList [] loc = loc
combineClassList glob [] = glob
combineClassList glob ((lI, lV) : locRest) = (setClassList lI lV (combineClassList glob locRest))

existsInClassList :: Ident -> ClassList -> Bool
existsInClassList i []              = False
existsInClassList i ((x, y) : rest) = if (x == i) then True else existsInClassList i rest

lookupClassList :: Ident -> ClassList -> Maybe Class
lookupClassList i []              = Nothing
lookupClassList i ((x, y) : rest) = if (x == i) then Just y else lookupClassList i rest

{- ------------------------------------IO LOOP----------------------------------- -}
printFileList :: [String] -> Int -> IO ()
printFileList [] count = return()
printFileList (x:xs) count = do putStrLn ((show count) ++ ") " ++ x)
                                printFileList xs (count + 1)

{- -}
envToStr :: Env -> String
envToStr ([], f, c) = ""
envToStr (((x, y, z):rest), f, c) = case z of 
                                   (NumVal i)    -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (DoubleVal i) -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (BoolVal i)   -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (StrVal i)    -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (CharVal i)   -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (ArrVal i)    -> "(" ++ x ++ ", " ++ (show i) ++ ")\n" ++ envToStr (rest, f, c)
                                   (ObjVal i)    -> "OBJECT\n" ++ envToStr (rest, f, c)
                                   (NullVal)     -> "(" ++ x ++ ", NULL)" ++ ['\n'] ++ envToStr (rest, f, c)
                             
{- -}
psToStr :: PrintStack -> String
psToStr [] = ""
psToStr (x:xs) = case x of
                 (NumVal i)    -> (show i) ++ psToStr xs
                 (DoubleVal i) -> (show i) ++ psToStr xs
                 (BoolVal i)   -> (show i) ++ psToStr xs
                 (StrVal i)    -> i ++ psToStr xs
                 (CharVal i)   -> [i] ++ psToStr xs
                 (ArrVal i)    -> (show i) ++ psToStr xs
                 (NullVal)     -> "NULL" ++ psToStr xs

{- -}
inputPrompt :: [String] -> IO String
inputPrompt files = do
                putStrLn "\nSelect file to Load: "
                printFileList files 1
                putStr ">> "
                input <- getLine
                let index = read input :: Int
                if (index > (length files) || index <= 0) then do
                    putStrLn "INVALID OPTION. Please try again."
                    inputPrompt files
                else do
                    let inputFile = files !! (index - 1)
                    return inputFile

{- -}
mapPrompt :: [String] -> IO String
mapPrompt maps = do
                    putStrLn "\nSelect Map to Use: "
                    printFileList maps 1
                    putStr ">> "
                    input <- getLine
                    let index = read input :: Int
                    if (index > (length maps) || index <= 0) then do
                        putStrLn "INVALID OPTION. Please try again."
                        mapPrompt maps
                    else do
                        let map = maps !! (index - 1)
                        return map
{- -}
processPrompt :: IO Int
processPrompt = do
                    putStrLn "\nSelect a process to do to the file: "
                    putStrLn "1) Convert File"
                    putStrLn "2) Execute File"
                    putStr ">> "
                    option <- getLine
                    let index = read option :: Int
                    if (index > 2 || index <= 0) then do
                        putStrLn "INVALID OPTION. Please try again."
                        processPrompt
                    else
                        return index

{- -}
outputPrompt :: [String] -> IO String
outputPrompt inputList = do
                            putStrLn "\nSpecify an output file: "
                            putStr ">> "
                            outputFile <- getLine
                            if (outputFile `elem` inputList) then do
                                putStrLn "Cannot overwrite input files."
                                outputPrompt inputList
                            else
                                return outputFile

main :: IO ()
main = do 
    Encoding.setLocaleEncoding Encoding.utf8
    allInput <- getDirectoryContents "./Programs"
    allMaps  <- getDirectoryContents "./Maps"
    let files = filter (isSuffixOf ".ml") allInput -- Get all input files
    let maps = filter (isSuffixOf ".map") allMaps  -- Get all Maps

    process     <- processPrompt      -- Get Process
    inputFile   <- inputPrompt files  -- Get Input File
    contents    <- readFile ("./Programs/" ++ inputFile)
    outputFile  <- outputPrompt files -- Get Output file

    let inputFileFirstLine = head (lines contents)
    let inputNoFirstLine = concat (intersperse "\n" (tail (lines contents)))

    if (process == 1) then do
        if (isInfixOf ".map" inputFileFirstLine == True)
            then do
                mapContents <- (readFile ("./Maps/" ++ inputFileFirstLine))
                let myMap = buildMap (splitBy ['\n'] mapContents) ++ [(" ", " "), ("\n", "\n"), ("\t", "\t")]
                let convertedText = convert inputNoFirstLine myMap
                writeFile ("./Outputs/" ++ outputFile) (convertedText)
                putStrLn ""
                putStrLn ("'" ++ inputFile ++ "' has been converted back to original and saved in '" ++ outputFile ++ "'.")
            else do
                myMapFile   <- mapPrompt maps       -- Get Map
                mapContents <- readFile ("./Maps/" ++ myMapFile)
                let myMap = buildMap (splitBy ['\n'] mapContents) ++ [(" ", " "), ("\n", "\n"), ("\t", "\t")]
                let convertedText = convert contents myMap

                writeFile ("./Outputs/" ++ outputFile) (myMapFile ++ "\n" ++ convertedText)
                putStrLn ""
                putStrLn ("'" ++ inputFile ++ "' has been converted with '" ++ myMapFile ++ "' and saved in '" ++ outputFile ++ "'.")
    else do
        if (isInfixOf ".map" inputFileFirstLine == False)  
            then do
                let lexed = lexer contents 1
                let parsed = reverseList (parseExpr lexed)
                let (newEnv, ps) = execList parsed (([], [], []), [])
                let ps' = reverseList ps
                let envStr = envToStr newEnv
                let psStr = psToStr ps'

                -- These are for Debug Purposes. Uncomment to watch the code compile!
                --putStrLn (show contents ++ "\n")
                --putStrLn ("Lexed: " ++ show lexed ++ "\n")
                --putStrLn ("SR: " ++ show (sr [] lexed) ++ "\n")
                --putStrLn ("Parsed: " ++ show parsed ++ "\n")
                --putStrLn ("Env: " ++ show newEnv ++ "\n")
                --putStrLn (psStr)

                let output = "Enviorment Variables:\n" ++ envStr ++ "\n\nPrint Stack:\n" ++ psStr
                writeFile ("./Outputs/" ++ outputFile) (output)
                putStrLn ("\n'" ++ inputFile ++ "' has been Executed and saved in '" ++ outputFile ++ "'.")
            else do
                mapContents <- (readFile ("./Maps/" ++ inputFileFirstLine))
                let myMap = buildMap (splitBy ['\n'] mapContents) ++ [(" ", " "), ("\n", "\n"), ("\t", "\t")]
                let convertedText = convert inputNoFirstLine myMap

                let lexed = lexer convertedText 1
                let parsed = reverseList (parseExpr lexed)
                let (newEnv, ps) = execList parsed (([], [], []), [])
                let ps' = reverseList ps
                let envStr = envToStr newEnv
                let psStr = psToStr ps'

                --putStrLn (show convertedText)
                --putStrLn (show lexed)
                --putStrLn (show parsed)
                --putStrLn (show newEnv)

                let output = "Enviorment Variables:\n" ++ envStr ++ "\n\nPrint Stack:\n" ++ psStr
                let convEnvStr = convert output myMap
                writeFile ("./Outputs/" ++ outputFile) (convEnvStr ++ "\n\n" ++ output)
                putStrLn ("\n'" ++ inputFile ++ "' has been Executed and saved in '" ++ outputFile ++ "'.")
