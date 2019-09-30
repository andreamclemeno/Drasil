{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.LanguageRenderer (
  -- * Common Syntax
  classDec, dot, doubleSlash, elseIfLabel, forLabel, inLabel, new, 
  blockCmtStart, blockCmtEnd, docCmtStart, observerListName, addExt,
  
  -- * Default Functions available for use in renderers
  packageDocD, fileDoc', moduleDocD, classDocD, enumDocD, enumElementsDocD, 
  enumElementsDocD', multiStateDocD, blockDocD, bodyDocD, oneLinerD, outDoc, printDoc,
  printFileDocD, boolTypeDocD, intTypeDocD, floatTypeDocD, charTypeDocD, 
  stringTypeDocD, fileTypeDocD, typeDocD, enumTypeDocD, listTypeDocD, listInnerTypeD, voidDocD, 
  constructDocD, paramDocD, paramListDocD, mkParam, methodDocD, 
  methodListDocD, stateVarDocD, stateVarDefDocD, constVarDocD, stateVarListDocD,
  alwaysDel, ifCondDocD, switchDocD, forDocD, forEachDocD, whileDocD, 
  tryCatchDocD, assignDocD, multiAssignDoc, plusEqualsDocD, plusEqualsDocD', 
  plusPlusDocD, plusPlusDocD', varDecDocD, varDecDefDocD, listDecDocD, 
  listDecDefDocD, statementDocD, returnDocD, commentDocD, freeDocD, throwDocD, 
  mkSt, mkStNoEnd, stringListVals', stringListLists', stratDocD, runStrategyD, listSliceD, unOpPrec, 
  notOpDocD, notOpDocD', negateOpDocD, sqrtOpDocD, sqrtOpDocD', absOpDocD, 
  absOpDocD', logOpDocD, logOpDocD', lnOpDocD, lnOpDocD', expOpDocD, expOpDocD',
  sinOpDocD, sinOpDocD', cosOpDocD, cosOpDocD', tanOpDocD, tanOpDocD', 
  asinOpDocD, asinOpDocD', acosOpDocD, acosOpDocD', atanOpDocD, atanOpDocD', 
  unOpDocD, unExpr, unExpr', typeUnExpr, powerPrec, multPrec, andPrec, orPrec, 
  equalOpDocD, notEqualOpDocD, greaterOpDocD, greaterEqualOpDocD, lessOpDocD, 
  lessEqualOpDocD, plusOpDocD, minusOpDocD, multOpDocD, divideOpDocD, 
  moduloOpDocD, powerOpDocD, andOpDocD, orOpDocD, binOpDocD, binOpDocD', 
  binExpr, binExpr', typeBinExpr, mkVal, mkVar, mkStaticVar, litTrueD, 
  litFalseD, litCharD, litFloatD, litIntD, litStringD, varDocD, extVarDocD, 
  selfDocD, argDocD, enumElemDocD, classVarCheckStatic, classVarDocD,
  objVarDocD, inlineIfD, funcAppDocD, newObjDocD, newObjDocD',
  objDecDefDocD, constDecDefDocD, listIndexExistsDocD, varD, 
  staticVarD, extVarD, selfD, enumVarD, classVarD, objVarD, objVarSelfD, listVarD, listOfD, iterVarD, valueOfD, argD, enumElementD, argsListD, funcAppD, extFuncAppD, newObjD, notNullD, objAccessD, objMethodCallD, objMethodCallNoParamsD, selfAccessD, listIndexExistsD, indexOfD,
  funcDocD, 
  castDocD, sizeDocD, listAccessFuncDocD, listSetFuncDocD, objAccessDocD, 
  castObjDocD, funcD, getD, setD, listSizeD, listAddD, listAppendD, iterBeginD, iterEndD, listAccessD, listSetD, getFuncD, setFuncD, listSizeFuncD, listAddFuncD, listAppendFuncD, iterBeginError, iterEndError, listAccessFuncD, listAccessFuncD', listSetFuncD, includeD, breakDocD, continueDocD, staticDocD, dynamicDocD, 
  privateDocD, publicDocD, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, functionDox, classDoc, moduleDoc, commentedModD, docFuncRepr,
  valList, prependToBody, appendToBody, surroundBody, getterName, setterName, 
  setMainMethod, setEmpty, intValue, filterOutObjs
) where

import Utils.Drasil (blank, capitalize, indent, indentList, stringList)

import GOOL.Drasil.CodeType (CodeType(..), isObject)
import GOOL.Drasil.Symantics (Label, Library,
  RenderSym(..), BodySym(..), BlockSym(..), PermanenceSym(..),
  TypeSym(Type, getType, getTypeString, getTypeDoc, bool, float, string, 
    listType, listInnerType, obj, enumType, iterator), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), InternalValue(..), Selector(..), FunctionSym(..), 
  SelectorFunction(..), InternalFunction(..), InternalStatement(..), 
  StatementSym(..), ControlStatementSym(..), ParameterSym(..), MethodSym(..), 
  InternalMethod(..), BlockCommentSym(..))
import qualified GOOL.Drasil.Symantics as S (TypeSym(char, int))
import GOOL.Drasil.Data (Terminator(..), FileData(..), fileD, updateFileMod, 
  FuncData(..), ModData(..), updateModDoc, MethodData(..), OpData(..), od, 
  ParamData(..), pd, TypeData(..), td, ValData(..), vd, Binding(..), 
  VarData(..), vard)
import GOOL.Drasil.Helpers (angles, doubleQuotedText, hicat, vibcat, vmap, 
  emptyIfEmpty, emptyIfNull, getInnerType, getNestDegree, convType)

import Control.Applicative ((<|>))
import Data.List (intersperse, last)
import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe)
import Prelude hiding (break,print,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

----------------------------------------
-- Syntax common to several renderers --
----------------------------------------

classDec, dot, doubleSlash, elseIfLabel, forLabel, inLabel, new, blockCmtStart, 
  blockCmtEnd, docCmtStart :: Doc
classDec = text "class"
dot = text "."
doubleSlash = text "//"
elseIfLabel = text "else if"
forLabel = text "for"
inLabel = text "in"
new = text "new"
blockCmtStart = text "/*"
blockCmtEnd = text "*/"
docCmtStart = text "/**"

observerListName :: Label
observerListName = "observerList"

addExt :: String -> String -> String
addExt ext nm = nm ++ "." ++ ext

----------------------------------
-- Functions for rendering code --
----------------------------------

packageDocD :: Label -> Doc -> FileData -> FileData
packageDocD n end f = fileD (fileType f) (n ++ "/" ++ filePath f) (updateModDoc 
  (emptyIfEmpty (modDoc $ fileMod f) (vibcat [text "package" <+> text n <> end, 
  modDoc (fileMod f)])) (fileMod f))

fileDoc' :: Doc -> Doc -> Doc -> Doc
fileDoc' t m b = vibcat [
  t,
  m,
  b]

-----------------------------------------------
-- 'Default' functions used in the renderers --
-----------------------------------------------

-- Module --

moduleDocD :: [(Doc, Bool)] -> Doc
moduleDocD cs = vibcat (map fst cs)

-- Class --

classDocD :: Label -> Maybe Label -> Doc -> Doc -> Doc -> Doc -> Doc
classDocD n p inh s vs fs = vcat [
  s <+> classDec <+> text n <+> baseClass <+> lbrace, 
  indentList [
    vs,
    blank,
    fs],
  rbrace]
  where baseClass = case p of Nothing -> empty
                              Just pn -> inh <+> text pn

enumDocD :: Label -> Doc -> Doc -> Doc
enumDocD n es s = vcat [
  s <+> text "enum" <+> text n <+> lbrace,
  indent es,
  rbrace]

enumElementsDocD :: [Label] -> Bool -> Doc
enumElementsDocD es enumsEqualInts = vcat $
  zipWith (\e i -> text e <+> equalsInt i <> interComma i) es nums
  where nums = [0..length es - 1]
        equalsInt i = if enumsEqualInts then equals <+> int i else empty 
        interComma i = if i < length es - 1 then text "," else empty

enumElementsDocD' :: [Label] -> Doc
enumElementsDocD' es = vcat $
  zipWith (\e i -> text e <+> equals <+> int i) es nums
    where nums = [0..length es - 1]

-- Groupings --

multiStateDocD :: Doc -> [(Doc, Terminator)] -> (Doc, Terminator)
multiStateDocD end sts = (vcat (applyEnd statements), needsEnd statements)
  where applyEnd [] = []
        applyEnd [(s, _)] = [s]
        applyEnd ((s, t):ss) = (s <> getTermDoc t) : applyEnd ss
        needsEnd [] = Empty
        needsEnd ss = snd (last ss)
        statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty (fst s)) && 
          (render (fst s) /= render end)

blockDocD :: Doc -> [Doc] -> Doc
blockDocD end sts = vcat statements
  where statements = filter notNullStatement sts
        notNullStatement s = not (isEmpty s) && (render s /= render end)

bodyDocD :: [Doc] -> Doc
bodyDocD bs = vibcat blocks
  where blocks = filter notNullBlock bs
        notNullBlock b = not $ isEmpty b

oneLinerD :: (RenderSym repr) => repr (Statement repr) -> repr (Body repr)
oneLinerD s = bodyStatements [s]

-- IO --

printDoc :: ValData -> ValData -> Doc
printDoc printFn v = valDoc printFn <> parens (valDoc v)

printListDoc :: (RenderSym repr) => Integer -> repr (Value repr) -> 
  (repr (Value repr) -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  (String -> repr (Statement repr)) -> 
  repr (Statement repr)
printListDoc n v prFn prStrFn prLnFn = multi [prStrFn "[", 
  for (varDecDef i (litInt 0)) (valueOf i ?< (listSize v #- litInt 1))
    (i &++) (bodyStatements [prFn (listAccess v (valueOf i)), prStrFn ", /f "]), 
  ifNoElse [(listSize v ?> litInt 0, oneLiner $
    prFn (listAccess v (listSize v #- litInt 1)))], 
  prLnFn "]"]
  where l_i = "list_i" ++ show n
        i = var l_i S.int

printObjDoc :: String -> (String -> repr (Statement repr)) 
  -> repr (Statement repr)
printObjDoc n prLnFn = prLnFn $ "Instance of " ++ n ++ " object"

outDoc :: (RenderSym repr) => Bool -> repr (Value repr) -> repr (Value repr) 
  -> Maybe (repr (Value repr)) -> repr (Statement repr)
outDoc newLn printFn v f = outDoc' (getType $ valueType v)
  where outDoc' (List t) = printListDoc (getNestDegree 1 t) v prFn prStrFn 
          prLnFn
        outDoc' (Object n) = printObjDoc n prLnFn
        outDoc' _ = printSt newLn printFn v f
        prFn = maybe print printFile f
        prStrFn = maybe printStr printFileStr f
        prLnFn = if newLn then maybe printStrLn printFileStrLn f else maybe printStr printFileStr f 

printFileDocD :: Label -> ValData -> Doc
printFileDocD fn f = valDoc f <> dot <> text fn

-- Type Printers --

boolTypeDocD :: TypeData
boolTypeDocD = td Boolean "Boolean" (text "Boolean") -- capital B?

intTypeDocD :: TypeData
intTypeDocD = td Integer "int" (text "int")

floatTypeDocD :: TypeData
floatTypeDocD = td Float "float" (text "float")

charTypeDocD :: TypeData
charTypeDocD = td Char "char" (text "char")

stringTypeDocD :: TypeData
stringTypeDocD = td String "string" (text "string")

fileTypeDocD :: TypeData
fileTypeDocD = td File "File" (text "File")

typeDocD :: Label -> TypeData
typeDocD t = td (Object t) t (text t)

enumTypeDocD :: Label -> TypeData
enumTypeDocD t = td (Enum t) t (text t)

listTypeDocD :: TypeData -> Doc -> TypeData
listTypeDocD t lst = td (List (cType t)) 
  (render lst ++ "<" ++ typeString t ++ ">") (lst <> angles (typeDoc t))

listInnerTypeD :: (RenderSym repr) => repr (Type repr) -> repr (Type repr)
listInnerTypeD = convType . getInnerType . getType

-- Method Types --

voidDocD :: TypeData
voidDocD = td Void "void" (text "void")

constructDocD :: Label -> Doc
constructDocD _ = empty

-- Parameters --

paramDocD :: VarData -> Doc
paramDocD v = typeDoc (varType v) <+> varDoc v

paramListDocD :: [ParamData] -> Doc
paramListDocD = hicat (text ", ") . map paramDoc

mkParam :: (VarData -> Doc) -> VarData -> ParamData
mkParam f v = pd v (f v)

-- Method --

methodDocD :: Label -> Doc -> Doc -> TypeData -> Doc -> Doc -> Doc
methodDocD n s p t ps b = vcat [
  s <+> p <+> typeDoc t <+> text n <> parens ps <+> lbrace,
  indent b,
  rbrace]

methodListDocD :: [Doc] -> Doc
methodListDocD ms = vibcat methods
  where methods = filter (not . isEmpty) ms

-- StateVar --

stateVarDocD :: Doc -> Doc -> VarData -> Doc -> Doc
stateVarDocD s p v end = s <+> p <+> typeDoc (varType v) <+> varDoc v <> end

stateVarDefDocD :: Doc -> Doc -> Doc -> Doc
stateVarDefDocD s p dec = s <+> p <+> dec

constVarDocD :: Doc -> Doc -> VarData -> Doc -> Doc
constVarDocD s p v end = s <+> p <+> text "const" <+> typeDoc (varType v) <+>
  varDoc v <> end

stateVarListDocD :: [Doc] -> Doc
stateVarListDocD = vcat

alwaysDel :: Int
alwaysDel = 4

-- Controls --

ifCondDocD :: Doc -> Doc -> Doc -> Doc -> [(ValData, Doc)] -> Doc
ifCondDocD _ _ _ _ [] = error "if condition created with no cases"
ifCondDocD ifStart elif bEnd elseBody (c:cs) = 
  let ifSect (v, b) = vcat [
        text "if" <+> parens (valDoc v) <+> ifStart,
        indent b,
        bEnd]
      elseIfSect (v, b) = vcat [
        elif <+> parens (valDoc v) <+> ifStart,
        indent b,
        bEnd]
      elseSect = emptyIfEmpty elseBody $ vcat [
        text "else" <+> ifStart,
        indent elseBody,
        bEnd]
  in vcat [
    ifSect c,
    vmap elseIfSect cs,
    elseSect]

switchDocD :: (Doc, Terminator) -> ValData -> Doc -> 
  [(ValData, Doc)] -> Doc
switchDocD breakState v defBody cs = 
  let caseDoc (l, result) = vcat [
        text "case" <+> valDoc l <> colon,
        indentList [
          result,
          fst breakState]]
      defaultSection = vcat [
        text "default" <> colon,
        indentList [
          defBody,
          fst breakState]]
  in vcat [
      text "switch" <> parens (valDoc v) <+> lbrace,
      indentList [
        vmap caseDoc cs,
        defaultSection],
      rbrace]

-- These signatures wont be quite so horrendous if/when we pass language options
-- (blockStart, etc.) in as shared environment
forDocD :: Doc -> Doc -> (Doc, Terminator) -> ValData -> 
  (Doc, Terminator) -> Doc -> Doc
forDocD bStart bEnd sInit vGuard sUpdate b = vcat [
  forLabel <+> parens (fst sInit <> semi <+> valDoc vGuard <> semi <+> 
    fst sUpdate) <+> bStart,
  indent b,
  bEnd]

forEachDocD :: VarData -> Doc -> Doc -> Doc -> Doc -> ValData -> Doc -> Doc
forEachDocD e bStart bEnd forEachLabel inLbl v b =
  vcat [forEachLabel <+> parens (typeDoc (varType e) <+> varDoc e <+> inLbl 
    <+> valDoc v) <+> bStart,
  indent b,
  bEnd]

whileDocD :: Doc -> Doc -> ValData -> Doc -> Doc
whileDocD bStart bEnd v b = vcat [
  text "while" <+> parens (valDoc v) <+> bStart,
  indent b,
  bEnd]

tryCatchDocD :: Doc -> Doc -> Doc 
tryCatchDocD tb cb = vcat [
  text "try" <+> lbrace,
  indent tb,
  rbrace <+> text "catch" <+> parens (text "System.Exception" <+> text "exc") 
    <+> lbrace,
  indent cb,
  rbrace]

stratDocD :: Doc -> Doc -> Doc
stratDocD b resultState = vcat [
  b,
  resultState]

runStrategyD :: (RenderSym repr) => String -> [(Label, repr (Body repr))] -> 
  Maybe (repr (Value repr)) -> Maybe (repr (Variable repr)) -> 
  repr (Block repr)
runStrategyD l strats rv av = docBlock $ maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (flip stratDocD (statementDoc $ state resultState) . bodyDoc) 
  (Map.lookup l (Map.fromList strats))
  where resultState = maybe emptyState asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (assign v) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSliceD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr) -> 
  Maybe (repr (Value repr)) -> Maybe (repr (Value repr)) ->
  Maybe (repr (Value repr)) -> repr (Block repr)
listSliceD vnew vold b e s = 
  let l_temp = "temp"
      var_temp = var l_temp (variableType vnew)
      v_temp = valueOf var_temp
      l_i = "i_temp"
      var_i = var l_i S.int
      v_i = valueOf var_i
  in
    block [
      listDec 0 var_temp,
      for (varDecDef var_i (fromMaybe (litInt 0) b)) 
        (v_i ?< fromMaybe (listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
        (oneLiner $ valState $ listAppend v_temp (listAccess vold v_i)),
      vnew &= v_temp]

-- Statements --

assignDocD :: VarData -> ValData -> Doc
assignDocD vr vl = varDoc vr <+> equals <+> valDoc vl

multiAssignDoc :: [VarData] -> [ValData] -> Doc
multiAssignDoc vrs vls = varList vrs <+> equals <+> valList vls

plusEqualsDocD :: VarData -> ValData -> Doc
plusEqualsDocD vr vl = varDoc vr <+> text "+=" <+> valDoc vl

plusEqualsDocD' :: VarData -> OpData -> ValData -> Doc
plusEqualsDocD' vr plusOp vl = varDoc vr <+> equals <+> varDoc vr <+> 
  opDoc plusOp <+> valDoc vl

plusPlusDocD :: VarData -> Doc
plusPlusDocD v = varDoc v <> text "++"

plusPlusDocD' :: VarData -> OpData -> Doc
plusPlusDocD' v plusOp = varDoc v <+> equals <+> varDoc v <+> opDoc plusOp <+>
  int 1

varDecDocD :: VarData -> Doc -> Doc -> Doc
varDecDocD v s d = bind (varBind v) <+> typeDoc (varType v) <+> varDoc v
  where bind Static = s
        bind Dynamic = d

varDecDefDocD :: VarData -> ValData -> Doc -> Doc -> Doc
varDecDefDocD v def s d = varDecDocD v s d <+> equals <+> valDoc def

listDecDocD :: VarData -> ValData -> Doc -> Doc -> Doc
listDecDocD v n s d = varDecDocD v s d <+> equals <+> new <+> 
  typeDoc (varType v) <> parens (valDoc n)

listDecDefDocD :: VarData -> [ValData] -> Doc -> Doc -> Doc
listDecDefDocD v vs s d = varDecDocD v s d <+> equals <+> new <+> 
  typeDoc (varType v) <+> braces (valList vs)

objDecDefDocD :: VarData -> ValData -> Doc -> Doc -> Doc
objDecDefDocD = varDecDefDocD

constDecDefDocD :: VarData -> ValData -> Doc
constDecDefDocD v def = text "const" <+> typeDoc (varType v) <+> varDoc v <+> 
  equals <+> valDoc def

returnDocD :: [ValData] -> Doc
returnDocD vs = text "return" <+> valList vs

commentDocD :: Label -> Doc -> Doc
commentDocD cmt cStart = cStart <+> text cmt

freeDocD :: VarData -> Doc
freeDocD v = text "delete" <+> varDoc v

throwDocD :: Doc -> Doc
throwDocD errMsg = text "throw new" <+> text "System.ApplicationException" <>
  parens errMsg

statementDocD :: (Doc, Terminator) -> (Doc, Terminator)
statementDocD (s, t) = (s <> getTermDoc t, Empty)

getTermDoc :: Terminator -> Doc
getTermDoc Semi = semi
getTermDoc Empty = empty

mkSt :: Doc -> (Doc, Terminator)
mkSt s = (s, Semi)

mkStNoEnd :: Doc -> (Doc, Terminator)
mkStNoEnd s = (s, Empty)

stringListVals' :: (RenderSym repr) => [repr (Variable repr)] -> 
  repr (Value repr) -> repr (Statement repr)
stringListVals' vars sl = multi $ checkList (getType $ valueType sl)
    where checkList (List String) = assignVals vars 0
          checkList _ = error 
            "Value passed to stringListVals must be a list of strings"
          assignVals [] _ = []
          assignVals (v:vs) n = assign v (cast (variableType v) 
            (listAccess sl (litInt n))) : assignVals vs (n+1)

stringListLists' :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr)
  -> repr (Statement repr)
stringListLists' lsts sl = checkList (getType $ valueType sl)
  where checkList (List String) = listVals (map (getType . variableType) lsts)
        checkList _ = error 
          "Value passed to stringListLists must be a list of strings"
        listVals [] = loop
        listVals (List _:vs) = listVals vs
        listVals _ = error 
          "All values passed to stringListLists must have list types"
        loop = forRange var_i (litInt 0) (listSize sl #/ numLists) (litInt 1)
          (bodyStatements $ appendLists (map valueOf lsts) 0)
        appendLists [] _ = []
        appendLists (v:vs) n = valState (listAppend v (cast (listInnerType $ 
          valueType v) (listAccess sl ((v_i #* numLists) #+ litInt n)))) 
          : appendLists vs (n+1)
        numLists = litInt (toInteger $ length lsts)
        var_i = var "stringlist_i" S.int
        v_i = valueOf var_i
        

-- Unary Operators --

unOpPrec :: String -> OpData
unOpPrec = od 9 . text

notOpDocD :: OpData
notOpDocD = unOpPrec "!"

notOpDocD' :: OpData
notOpDocD' = unOpPrec "not"

negateOpDocD :: OpData
negateOpDocD = unOpPrec "-"

sqrtOpDocD :: OpData
sqrtOpDocD = unOpPrec "sqrt"

sqrtOpDocD' :: OpData
sqrtOpDocD' = unOpPrec "math.sqrt"

absOpDocD :: OpData
absOpDocD = unOpPrec "fabs"

absOpDocD' :: OpData
absOpDocD' = unOpPrec "math.fabs"

logOpDocD :: OpData
logOpDocD = unOpPrec "log"

logOpDocD' :: OpData
logOpDocD' = unOpPrec "math.log"

lnOpDocD :: OpData
lnOpDocD = unOpPrec "ln"

lnOpDocD' :: OpData
lnOpDocD' = unOpPrec "math.ln"

expOpDocD :: OpData
expOpDocD = unOpPrec "exp"

expOpDocD' :: OpData
expOpDocD' = unOpPrec "math.exp"

sinOpDocD :: OpData
sinOpDocD = unOpPrec "sin"

sinOpDocD' :: OpData
sinOpDocD' = unOpPrec "math.sin"

cosOpDocD :: OpData
cosOpDocD = unOpPrec "cos"

cosOpDocD' :: OpData
cosOpDocD' = unOpPrec "math.cos"

tanOpDocD :: OpData
tanOpDocD = unOpPrec "tan"

tanOpDocD' :: OpData
tanOpDocD' = unOpPrec "math.tan"

asinOpDocD :: OpData
asinOpDocD = unOpPrec "asin"

asinOpDocD' :: OpData
asinOpDocD' = unOpPrec "math.asin"

acosOpDocD :: OpData
acosOpDocD = unOpPrec "acos"

acosOpDocD' :: OpData
acosOpDocD' = unOpPrec "math.acos"

atanOpDocD :: OpData
atanOpDocD = unOpPrec "atan"

atanOpDocD' :: OpData
atanOpDocD' = unOpPrec "math.atan"

unOpDocD :: Doc -> Doc -> Doc
unOpDocD op v = op <> parens v

unOpDocD' :: Doc -> Doc -> Doc
unOpDocD' op v = op <> v

unExpr :: OpData -> ValData -> ValData
unExpr u v = mkExpr (opPrec u) (valType v) (unOpDocD (opDoc u) (valDoc v))

unExpr' :: OpData -> ValData -> ValData
unExpr' u v = mkExpr (opPrec u) (valType v) (unOpDocD' (opDoc u) (valDoc v))

typeUnExpr :: OpData -> TypeData -> ValData -> ValData
typeUnExpr u t v = mkExpr (opPrec u) t (unOpDocD (opDoc u) (valDoc v))

-- Binary Operators --

compEqualPrec :: String -> OpData
compEqualPrec = od 4 . text

compPrec :: String -> OpData
compPrec = od 5 . text

addPrec :: String -> OpData
addPrec = od 6 . text

multPrec :: String -> OpData
multPrec = od 7 . text

powerPrec :: String -> OpData
powerPrec = od 8 . text

andPrec :: String -> OpData 
andPrec = od 3 . text

orPrec :: String -> OpData
orPrec = od 2 . text

equalOpDocD :: OpData
equalOpDocD = compEqualPrec "=="

notEqualOpDocD :: OpData
notEqualOpDocD = compEqualPrec "!="

greaterOpDocD :: OpData
greaterOpDocD = compPrec ">"

greaterEqualOpDocD :: OpData
greaterEqualOpDocD = compPrec ">="

lessOpDocD :: OpData
lessOpDocD = compPrec "<"

lessEqualOpDocD :: OpData
lessEqualOpDocD = compPrec "<="

plusOpDocD :: OpData
plusOpDocD = addPrec "+"

minusOpDocD :: OpData
minusOpDocD = addPrec "-"

multOpDocD :: OpData
multOpDocD = multPrec "*"

divideOpDocD :: OpData
divideOpDocD = multPrec "/"

moduloOpDocD :: OpData
moduloOpDocD = multPrec "%"

powerOpDocD :: OpData
powerOpDocD = powerPrec "pow"

andOpDocD :: OpData
andOpDocD = andPrec "&&"

orOpDocD :: OpData
orOpDocD = orPrec "||"

binOpDocD :: Doc -> Doc -> Doc -> Doc
binOpDocD op v1 v2 = v1 <+> op <+> v2

binOpDocD' :: Doc -> Doc -> Doc -> Doc
binOpDocD' op v1 v2 = op <> parens (v1 <> comma <+> v2)
  
binExpr :: OpData -> ValData -> ValData -> ValData
binExpr b v1 v2 = mkExpr (opPrec b) (numType (valType v1) (valType v2)) 
  (binOpDocD (opDoc b) (exprParensL b v1 $ valDoc v1) (exprParensR b v2 $ 
  valDoc v2))

binExpr' :: OpData -> ValData -> ValData -> ValData
binExpr' b v1 v2 = mkExpr 9 (numType (valType v1) (valType v2)) 
  (binOpDocD' (opDoc b) (valDoc v1) (valDoc v2))

numType :: TypeData -> TypeData -> TypeData
numType t1 t2 = numericType (cType t1) (cType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType _ _ = error "Numeric types required for numeric expression"

typeBinExpr :: OpData -> TypeData -> ValData -> ValData -> ValData
typeBinExpr b t v1 v2 = mkExpr (opPrec b) t (binOpDocD (opDoc b) (exprParensL b 
  v1 $ valDoc v1) (exprParensR b v2 $ valDoc v2))

mkVal :: TypeData -> Doc -> ValData
mkVal = vd Nothing

mkVar :: String -> TypeData -> Doc -> VarData
mkVar = vard Dynamic

mkStaticVar :: String -> TypeData -> Doc -> VarData
mkStaticVar = vard Static

mkExpr :: Int -> TypeData -> Doc -> ValData
mkExpr p = vd (Just p)

-- Literals --

litTrueD :: (RenderSym repr) => repr (Value repr)
litTrueD = valFromData Nothing bool (text "true")

litFalseD :: (RenderSym repr) => repr (Value repr)
litFalseD = valFromData Nothing bool (text "false")

litCharD :: (RenderSym repr) => Char -> repr (Value repr)
litCharD c = valFromData Nothing S.char (quotes $ char c)

litFloatD :: (RenderSym repr) => Double -> repr (Value repr)
litFloatD f = valFromData Nothing float (double f)

litIntD :: (RenderSym repr) => Integer -> repr (Value repr)
litIntD i = valFromData Nothing S.int (integer i)

litStringD :: (RenderSym repr) => String -> repr (Value repr)
litStringD s = valFromData Nothing string (doubleQuotedText s)

-- Value Printers --

varDocD :: Label -> Doc
varDocD = text

extVarDocD :: Library -> Label -> Doc
extVarDocD l n = text l <> dot <> text n

selfDocD :: Doc
selfDocD = text "this"

argDocD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> Doc
argDocD n args = valueDoc args <> brackets (valueDoc n)

enumElemDocD :: Label -> Label -> Doc
enumElemDocD en e = text en <> dot <> text e

classVarCheckStatic :: (VariableSym repr) => repr (Variable repr) -> 
  repr (Variable repr)
classVarCheckStatic v = classVarCS (variableBind v)
  where classVarCS Dynamic = error
          "classVar can only be used to access static variables"
        classVarCS Static = v

classVarDocD :: Doc -> Doc -> Doc
classVarDocD c v = c <> dot <> v

objVarDocD :: Doc -> Doc ->  Doc
objVarDocD n1 n2 = n1 <> dot <> n2

inlineIfD :: ValData -> ValData -> ValData -> ValData
inlineIfD c v1 v2 = vd prec (valType v1) (valDoc c <+> text "?" <+> 
  valDoc v1 <+> text ":" <+> valDoc v2)
  where prec = valPrec c <|> Just 0

funcAppDocD :: (RenderSym repr) => Label -> [repr (Value repr)] -> Doc
funcAppDocD n vs = text n <> parens (valueList vs)

newObjDocD :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD st vs = new <+> newObjDocD' st vs

newObjDocD' :: (RenderSym repr) => repr (Type repr) -> Doc -> Doc
newObjDocD' st vs = getTypeDoc st <> parens vs

listIndexExistsDocD :: OpData -> ValData -> ValData -> Doc
listIndexExistsDocD greater lst index = parens (valDoc lst <> 
  text ".Length" <+> opDoc greater <+> valDoc index) 

varD :: (RenderSym repr) => Label -> repr (Type repr) -> repr (Variable repr)
varD n t = varFromData Dynamic n t (varDocD n)

staticVarD :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Variable repr)
staticVarD n t = varFromData Static n t (varDocD n)

extVarD :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  repr (Variable repr)
extVarD l n t = varFromData Dynamic (l ++ "." ++ n) t (extVarDocD l n)

selfD :: (RenderSym repr) => Label -> repr (Variable repr)
selfD l = varFromData Dynamic "this" (obj l) selfDocD

enumVarD :: (RenderSym repr) => Label -> Label -> repr (Variable repr)
enumVarD e en = var e (enumType en)

classVarD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (Type repr) -> 
  repr (Variable repr) -> repr (Variable repr)
classVarD f c v = classVarCheckStatic $ varFromData (variableBind v) 
  (getTypeString c ++ "." ++ variableName v) 
  (variableType v) (f (getTypeDoc c) (variableDoc v))

objVarD :: (RenderSym repr) => repr (Variable repr) -> repr (Variable repr) -> 
  repr (Variable repr)
objVarD o v = varFromData Dynamic (variableName o ++ "." ++ variableName v) 
  (variableType v) (objVarDocD (variableDoc o) (variableDoc v))

objVarSelfD :: (RenderSym repr) => Label -> Label -> repr (Type repr) -> 
  repr (Variable repr)
objVarSelfD l n t = objVar (self l) (var n t)

listVarD :: (RenderSym repr) => Label -> repr (Permanence repr) -> 
  repr (Type repr) -> repr (Variable repr)
listVarD n p t = var n (listType p t)

listOfD :: (RenderSym repr) => Label -> repr (Type repr) -> repr (Variable repr)
listOfD n = listVar n static_

iterVarD :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Variable repr)
iterVarD n t = var n (iterator t)

valueOfD :: (RenderSym repr) => repr (Variable repr) -> repr (Value repr)
valueOfD v = valFromData Nothing (variableType v) (variableDoc v)

argD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) ->
  repr (Value repr)
argD n args = valFromData Nothing string (argDocD n args)

enumElementD :: (RenderSym repr) => Label -> Label -> repr (Value repr)
enumElementD en e = valFromData Nothing (enumType en) (enumElemDocD en e)

argsListD :: (RenderSym repr) => String -> repr (Value repr)
argsListD l = valFromData Nothing (listType static_ string) (text l)
 
funcAppD :: (RenderSym repr) => Label -> repr (Type repr) -> [repr (Value repr)]
  -> repr (Value repr)
funcAppD n t vs = valFromData Nothing t (funcAppDocD n vs)

extFuncAppD :: (RenderSym repr) => Library -> Label -> repr (Type repr) -> 
  [repr (Value repr)] -> repr (Value repr)
extFuncAppD l n = funcAppD (l ++ "." ++ n)

newObjD :: (RenderSym repr) => (repr (Type repr) -> Doc -> Doc) -> 
  repr (Type repr) -> [repr (Value repr)] -> repr (Value repr)
newObjD f t vs = valFromData Nothing t (f t (valueList vs))

notNullD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
notNullD v = v ?!= valueOf (var "null" (valueType v))

objAccessD :: (RenderSym repr) => repr (Value repr) -> repr (Function repr) -> 
  repr (Value repr)
objAccessD v f = valFromData Nothing (functionType f) (objAccessDocD 
  (valueDoc v) (functionDoc f))

objMethodCallD :: (RenderSym repr) => repr (Type repr) -> repr (Value repr) -> 
  Label -> [repr (Value repr)] -> repr (Value repr)
objMethodCallD t o f ps = objAccess o (func f t ps)

objMethodCallNoParamsD :: (RenderSym repr) => repr (Type repr) -> 
  repr (Value repr) -> Label -> repr (Value repr)
objMethodCallNoParamsD t o f = objMethodCall t o f []

selfAccessD :: (RenderSym repr) => Label -> repr (Function repr) -> 
  repr (Value repr)
selfAccessD l = objAccess (valueOf $ self l)

listIndexExistsD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) 
  -> repr (Value repr)
listIndexExistsD lst index = listSize lst ?> index

indexOfD :: (RenderSym repr) => Label -> repr (Value repr) -> repr (Value repr) 
  -> repr (Value repr)
indexOfD f l v = objAccess l (func f S.int [v])

-- Functions --

funcDocD :: Doc -> Doc
funcDocD fnApp = dot <> fnApp

castDocD :: TypeData -> Doc
castDocD t = parens $ typeDoc t

sizeDocD :: Doc
sizeDocD = dot <> text "Count"

listAccessFuncDocD :: (RenderSym repr) => repr (Value repr) -> Doc
listAccessFuncDocD v = brackets $ valueDoc v

listSetFuncDocD :: Doc -> Doc -> Doc
listSetFuncDocD i v = brackets i <+> equals <+> v

objAccessDocD :: Doc -> Doc -> Doc
objAccessDocD v f = v <> f

castObjDocD :: Doc -> ValData -> Doc
castObjDocD t v = t <> parens (valDoc v)

funcD :: (RenderSym repr) => Label -> repr (Type repr) -> [repr (Value repr)] 
  -> repr (Function repr)
funcD l t vs = funcFromData t (funcDocD (valueDoc $ funcApp l t vs))

getD :: (RenderSym repr) => repr (Value repr) -> repr (Variable repr) -> 
  repr (Value repr)
getD v vToGet = v $. getFunc vToGet

setD :: (RenderSym repr) => repr (Value repr) -> repr (Variable repr) -> 
  repr (Value repr) -> repr (Value repr)
setD v vToSet toVal = v $. setFunc (valueType v) vToSet toVal

listSizeD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
listSizeD v = v $. listSizeFunc

listAddD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
listAddD v i vToAdd = v $. listAddFunc v i vToAdd

listAppendD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr)
listAppendD v vToApp = v $. listAppendFunc vToApp

iterBeginD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
iterBeginD v = v $. iterBeginFunc (listInnerType $ valueType v)

iterEndD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
iterEndD v = v $. iterEndFunc (listInnerType $ valueType v)

listAccessD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr)
listAccessD v i = v $. listAccessFunc (listInnerType $ valueType v) i

listSetD :: (RenderSym repr) => repr (Value repr) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr)
listSetD v i toVal = v $. listSetFunc v i toVal

getFuncD :: (RenderSym repr) => repr (Variable repr) -> repr (Function repr)
getFuncD v = func (getterName $ variableName v) (variableType v) []

setFuncD :: (RenderSym repr) => repr (Type repr) -> repr (Variable repr) -> 
  repr (Value repr) -> repr (Function repr)
setFuncD t v toVal = func (setterName $ variableName v) t [toVal]

listSizeFuncD :: (RenderSym repr) => repr (Function repr)
listSizeFuncD = func "size" S.int []

listAddFuncD :: (RenderSym repr) => Label -> repr (Value repr) -> 
  repr (Value repr) -> repr (Function repr)
listAddFuncD f i v = func f (listType static_ $ valueType v) [i, v]

listAppendFuncD :: (RenderSym repr) => Label -> repr (Value repr) -> 
  repr (Function repr)
listAppendFuncD f v = func f (listType static_ $ valueType v) [v]

iterBeginError :: String -> String
iterBeginError l = "Attempt to use iterBeginFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

iterEndError :: String -> String
iterEndError l = "Attempt to use iterEndFunc in " ++ l ++ ", but " ++ l ++ 
  " has no iterators"

listAccessFuncD :: (RenderSym repr) => repr (Type repr) -> repr (Value repr) ->
  repr (Function repr)
listAccessFuncD t i = funcFromData t (listAccessFuncDocD $ intValue i)

listAccessFuncD' :: (RenderSym repr) => Label -> repr (Type repr) -> 
  repr (Value repr) -> repr (Function repr)
listAccessFuncD' f t i = func f t [intValue i]

listSetFuncD :: (RenderSym repr) => (Doc -> Doc -> Doc) -> repr (Value repr) -> 
  repr (Value repr) -> repr (Value repr) -> repr (Function repr)
listSetFuncD f v i toVal = funcFromData (valueType v) (f (valueDoc $ intValue i)
  (valueDoc toVal))

-- Keywords --

includeD :: Label -> Label -> Doc
includeD incl n = text incl <+> text n

-- Permanence --

staticDocD :: Doc
staticDocD = text "static"

dynamicDocD :: Doc
dynamicDocD = empty

-- Jumps --

breakDocD :: Doc
breakDocD = text "break"

continueDocD :: Doc
continueDocD = text "continue"

-- Scope --

privateDocD :: Doc
privateDocD = text "private"

publicDocD :: Doc
publicDocD = text "public"

-- Comment Functions -- 

blockCmtDoc :: [String] -> Doc -> Doc -> Doc
blockCmtDoc lns start end = start <+> vcat (map text lns) <+> end

docCmtDoc :: [String] -> Doc -> Doc -> Doc
docCmtDoc lns start end = emptyIfNull lns $
  vcat $ start : map (indent . text) lns ++ [end]

commentedItem :: Doc -> Doc -> Doc
commentedItem cmt itm = emptyIfEmpty itm cmt $+$ itm

commentLength :: Int
commentLength = 75

endCommentLabel :: Label
endCommentLabel = "End"

addCommentsDocD :: Label -> Doc -> Doc -> Doc
addCommentsDocD c cStart b = vcat [
  commentDelimit c cStart,
  b,
  endCommentDelimit c cStart]

commentDelimit :: Label -> Doc -> Doc
commentDelimit c cStart = 
  let com = cStart <> text (" " ++ c ++ " ")
  in com <> text (dashes (render com) commentLength)

endCommentDelimit :: Label -> Doc -> Doc
endCommentDelimit c = commentDelimit (endCommentLabel ++ " " ++ c)

dashes :: String -> Int -> String
dashes s l = replicate (l - length s) '-'

functionDox :: String -> [(String, String)] -> [String] -> [String]
functionDox desc params returns = [doxBrief ++ desc | not (null desc)]
  ++ map (\(v, vDesc) -> doxParam ++ v ++ " " ++ vDesc) params
  ++ map (doxReturn ++) returns

classDoc :: String -> [String]
classDoc desc = [doxBrief ++ desc | not (null desc)]

moduleDoc :: String -> [String] -> String -> String -> [String]
moduleDoc desc as date m = (doxFile ++ m) : 
  [doxAuthor ++ stringList as | not (null as)] ++
  [doxDate ++ date | not (null date)] ++ 
  [doxBrief ++ desc | not (null desc)]

commentedModD :: Doc -> FileData -> FileData
commentedModD cmt m = updateFileMod (updateModDoc (commentedItem cmt 
  ((modDoc . fileMod) m)) (fileMod m)) m

docFuncRepr :: (MethodSym repr) => String -> [String] -> [String] -> 
  repr (Method repr) -> repr (Method repr)
docFuncRepr desc pComms rComms f = commentedFunc (docComment $ functionDox desc
  (zip (map parameterName (parameters f)) pComms) rComms) f

-- Helper Functions --

valList :: [ValData] -> Doc
valList vs = hcat (intersperse (text ", ") (map valDoc vs))

valueList :: (RenderSym repr) => [repr (Value repr)] -> Doc
valueList vs = hcat (intersperse (text ", ") (map valueDoc vs))

varList :: [VarData] -> Doc
varList vs = hcat (intersperse (text ", ") (map varDoc vs))

prependToBody :: (Doc, Terminator) -> Doc -> Doc
prependToBody s b = vcat [fst $ statementDocD s, maybeBlank, b]
  where maybeBlank = emptyIfEmpty (fst s) (emptyIfEmpty b blank)

appendToBody :: Doc -> (Doc, Terminator) -> Doc
appendToBody b s = vcat [b, maybeBlank, fst $ statementDocD s]
  where maybeBlank = emptyIfEmpty b (emptyIfEmpty (fst s) blank)

surroundBody :: (Doc, Terminator) -> Doc -> (Doc, Terminator) -> Doc
surroundBody p b a = prependToBody p (appendToBody b a)

getterName :: String -> String
getterName s = "Get" ++ capitalize s

setterName :: String -> String
setterName s = "Set" ++ capitalize s

setMainMethod :: MethodData -> MethodData
setMainMethod (MthD _ ps d) = MthD True ps d

setEmpty :: (Doc, Terminator) -> (Doc, Terminator)
setEmpty (d, _) = (d, Empty)

exprParensL :: OpData -> ValData -> (Doc -> Doc)
exprParensL o v = if maybe False (< opPrec o) (valPrec v) then parens else id

exprParensR :: OpData -> ValData -> (Doc -> Doc)
exprParensR o v = if maybe False (<= opPrec o) (valPrec v) then parens else id

intValue :: (RenderSym repr) => repr (Value repr) -> repr (Value repr)
intValue i = intValue' (getType $ valueType i)
  where intValue' Integer = i
        intValue' (Enum _) = cast S.int i
        intValue' _ = error "Value passed must be Integer or Enum"

filterOutObjs :: (VariableSym repr) => [repr (Variable repr)] -> 
  [repr (Variable repr)]
filterOutObjs = filter (not . isObject . getType . variableType)

doxCommand, doxBrief, doxParam, doxReturn, doxFile, doxAuthor, doxDate :: String
doxCommand = "\\"
doxBrief = doxCommand ++ "brief "
doxParam = doxCommand ++ "param "
doxReturn = doxCommand ++ "return "
doxFile = doxCommand  ++ "file "
doxAuthor = doxCommand ++ "author "
doxDate = doxCommand ++ "date "
