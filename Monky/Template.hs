{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-|
Module      : Monky.Template
Description : This module provides a template haskell splice for including librarys
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This module is intended to be used by Monky modules, /not/ for configuration.

This module provides the 'importLib' templateHaskell function to import a
C-library easily.

To use this, set the __LANGUAGE TemplateHaskell__ pragma in your module
file and include this module.

Usage:
Use 'importLib' as top-level declaration in your file. Like: 

@
importLib "LibAlsa" "libasound.so" []
@

This will create a data type for the library, and a function to get a handle for
this library (data <LibAlsa> and get<LibAlsa>).
To call your functions use the record syntax on that handle.
-}
module Monky.Template
  ( importLib
  , module Foreign.Ptr
  )
where

import Control.Monad (liftM2)
import Data.Char (isSpace)
import Data.List (nub)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)
import Language.Haskell.TH
import Monky.Utility
import Data.Maybe (fromMaybe)
import System.Posix.DynamicLinker (DL, dlclose, dlopen, dlsym, RTLDFlags(RTLD_LAZY))

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- trim a string
ltrim :: String -> String
ltrim = dropWhile isSpace

rtrim :: String -> String
rtrim = reverse . ltrim . reverse

trim :: String -> String
trim = rtrim . ltrim

-- Split type string
prepareFun :: String -> [String]
prepareFun = map trim . splitAtEvery "->" . trim

-- get constructor name
getName :: String -> Q Name
getName name =
  fromMaybe (error $ "Could not find constructor: " ++ name) <$> lookupTypeName name

-- Get a type from a String, this can do ONE application, so IO works
getType :: String -> Q Type
getType xs = if ' ' `elem` xs
  then let [t,a] = words xs in
    liftM2 AppT (getT t) (getT a)
  else getT xs
  where getT "()" = return (TupleT 0)
        getT ys = ConT <$> getName ys


-- Apply arrows to create a function from types
applyArrows :: [Type] -> Type
applyArrows [] = error "Cannot work with empty function type"
applyArrows [x] = x
applyArrows (x:xs) = AppT (AppT ArrowT x) (applyArrows xs)


-- Create function declarations for the constructor
mkFunDesc :: (String, String) -> VarBangTypeQ
mkFunDesc (x,y) = do
  t <- applyArrows <$> mapM getType (prepareFun y)
  return (mkName x, Bang NoSourceUnpackedness NoSourceStrictness, t)


cleanName :: Char -> String
cleanName '(' = "vo"
cleanName ')' = "id"
cleanName  x  = return x


-- Get the transformer name, this is some ugly name mangeling
transName :: String -> String
transName = concatMap cleanName . ("mkFun" ++) . filter isOk
  where isOk c = not (isSpace c) && c /= '-' && c /= '>'


-- Get the function described by the three-tuple (Alias, C-Name, TypeString)
getFunQ :: Name -> (String, String, String) -> Q Stmt
getFunQ handle (alias, name, typeString) = do
  let castFPtr = [| $(varE . mkName $ transName typeString) . castFunPtr |]
  let getSym = [| dlsym $(varE handle) name |]
  BindS (VarP (mkName (alias ++ "_"))) <$> [| fmap $(castFPtr) $(getSym) |]


-- Create the return statement, this applies the constructor
mkRet :: Name -> [String] -> Name -> Exp -> Q Stmt
mkRet hname xs rawN raw= do
  let funs = map (\x -> return (mkName x, VarE (mkName (x ++ "_")))) xs
  let con = recConE hname (return (rawN,raw):funs)
  NoBindS <$> [| return $(con) |]


-- Create the statement to get the handle
mkGetHandle :: Name -> String -> Q Stmt
mkGetHandle h libname =
  BindS (VarP h) <$> [| dlopen libname [RTLD_LAZY] |]


-- Create the get<LibName> function
mkGetFun :: String -> String -> Name -> [(String, String, String)] -> Name -> Q [Dec]
mkGetFun lname name hname funs raw = do
  let funName = mkName ("get" ++ name)
  let handle = mkName "handle"
  ghandle <- mkGetHandle handle lname
  funStmts <- mapM (getFunQ handle) funs
  ret <- mkRet hname (map (\(x,_,_) -> x) funs) raw (VarE handle)
  let fun = FunD funName [Clause [] (NormalB $ DoE (ghandle:funStmts ++ [ret])) []]
  sig <- sigD funName [t| IO $(conT . mkName $ name) |]
  return [sig,fun]


-- Create the transformer function used by get<LibName>
mkTransformer :: String -> Q Dec
mkTransformer f = do
  let name = mkName . transName $ f
  let ty = applyArrows <$> mapM getType (prepareFun f)
  ForeignD . ImportF CCall Safe "dynamic" name <$>  [t| (FunPtr $(ty)) -> $(ty) |]


mkDestroyFun :: String -> Name -> Q [Dec]
mkDestroyFun name raw = do
  let libT = mkName name
  let hname = mkName "handle"
  let funName = mkName ("destroy" ++ name)
  let body = [| dlclose ($(varE raw) $(varE hname)) |]

  sig <- sigD funName [t| $(conT libT) -> IO () |]
  fun <- funD funName [clause [varP hname] (normalB body) []]
  return [sig ,fun]

-- |Import a library
importLib
  :: String -- ^The name of the library data type
  -> String -- ^The name of the library
  -> [(String, String, String)] -- ^The functions in the library (Name, CName, Declaration)
  -> Q [Dec]
importLib hname lname xs = do
  let name = mkName hname
  funs <- mapM (mkFunDesc . (\(x,_,y) -> (x,y))) xs
  transformers <- mapM mkTransformer $ nub $ map (\(_,_,x) -> x) xs
  let rawRN = mkName "rawDL"
  let raw = (rawRN, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''DL)
  let dhandle = DataD [] name [] Nothing [RecC (mkName hname) (raw:funs)] []
  fun <- mkGetFun lname hname name xs rawRN
  dest <- mkDestroyFun hname rawRN
  return (dhandle:dest ++ transformers ++ fun)
