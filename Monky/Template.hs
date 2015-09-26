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
{-|
Module      : Monky.Template
Description : This module provides a template haskell template for including librarys
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This module provides the 'importLib' templateHaskell function to import a
C-library easily.

To use this, set the "{-# LANGUAGE TemplateHaskell #-}" pragma in your module
file and include this module.

Useage:
use 'importLib" as top-level declaration in your file. Like: 
importLib "Name" "libraryfile" [functions]
This will create a data type for the library, and a function to get a handle for
this library (data <Name> and get<Name>).
To call your functions use the record syntax on that handle.
-}
module Monky.Template
(importLib, module Foreign.Ptr, module System.Posix.DynamicLinker)
where

import Foreign.Ptr (Ptr, FunPtr, castFunPtr)
import System.Posix.DynamicLinker (dlopen, dlsym, RTLDFlags(RTLD_LAZY))

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub)
import Language.Haskell.TH

import Control.Monad (liftM2)

-- trim a string
ltrim :: String -> String
ltrim [] = []
ltrim (x:xs)
  | isSpace x = ltrim xs
  | otherwise = x:xs

rtrim :: String -> String
rtrim = reverse . ltrim . reverse

trim :: String -> String
trim = rtrim . ltrim

-- |Split ys at every occurence of xs
splitAtEvery :: String -> String -> String -> [String]
splitAtEvery _ [] zs = [zs]
splitAtEvery xs (y:ys) zs = if isPrefixOf xs (y:ys)
  then zs:(splitAtEvery xs (cut ys) [])
  else splitAtEvery xs ys (zs ++ [y])
  where cut = drop (length xs -1)


-- Split function into types
prepareFun :: String -> [String]
prepareFun = map trim . flip (splitAtEvery "->") [] . trim

-- Get the type name or tell user what failed
getJust :: String -> Maybe Name -> Q Name
getJust _ (Just n) = return n
getJust name Nothing = fail ("Could not find: " ++ name)

-- get variable name
getVName :: String -> Q Name
getVName name = getJust name =<< lookupValueName name

-- get constructor name
getName :: String -> Q Name
getName name = getJust name =<< lookupTypeName name

-- Get a type from a String, this also makes sure 'IO' a works
getType :: String -> Q Type
getType xs = if ' ' `elem` xs
  then let [t,a] = words xs in
    liftM2 AppT (getT t) (getT a)
  else (getT xs)
  where getT "()" = return (TupleT 0)
        getT ys = ConT <$> getName ys


-- Apply arrows to create a function from types
applyArrows :: [Type] -> Type
applyArrows [] = error "Cannot work with empty function type"
applyArrows [x] = x
applyArrows (x:xs) = AppT (AppT ArrowT x) (applyArrows xs)


-- Create function declarations for the constructor
mkFunDesc :: (String, String) -> VarStrictTypeQ
mkFunDesc (x,y) = do
  t <- applyArrows <$> mapM getType (prepareFun y)
  return (mkName x, NotStrict, t)

-- Get the transformer name, this is some ugly name mangeling
transName :: String -> Name
transName = mkName . ("mkFun" ++) . filter isOk
  where isOk c = not (isSpace c) && c /= '-' && c /= '>'

-- Get the function described by the three-tuple (Alias, C-Name, TypeString)
getFun :: Exp -> (String, String, String) -> Q Stmt
getFun handle (alias, name, typeString) = do
  let transname = transName typeString
  dlsymN <- getVName "dlsym"
  fm <- getVName "fmap"
  dot <- getVName "."
  cast <- getVName "castFunPtr"
  let castFPtr = InfixE (Just (VarE transname)) (VarE dot) (Just (VarE cast))
  let getSym = AppE (AppE (VarE dlsymN) handle) (LitE (StringL name))
  let getF = AppE (AppE (VarE fm) castFPtr) getSym
  return $ BindS (VarP (mkName (alias ++ "_"))) getF


-- Create the return statement, this applies the constructor
mkRet :: Name -> [String] -> Q Stmt
mkRet hname xs = do
  let funs = map (\x -> (mkName x, (VarE (mkName (x ++ "_"))))) xs
  ret <- getVName "return"
  let con = RecConE hname funs
  return $ NoBindS (AppE (VarE ret) con)


-- Create the statement to get the handle
mkGetHandle :: Name -> String -> Q Stmt
mkGetHandle h libname = do
  dlopenN <- getVName "dlopen"
  lazy <- getVName "RTLD_LAZY"
  let hExp = AppE (AppE (VarE dlopenN) (LitE (StringL libname))) (ListE [(ConE lazy)])
  return $ BindS (VarP h) hExp


-- Create the get<LibName> function
mkGetFun :: String -> String -> Name -> [(String, String,String)] -> Q [Dec]
mkGetFun lname name hname funs = do
  let funName = mkName ("get" ++ name)
  let handle = mkName "handle"
  ghandle <- mkGetHandle handle lname
  funStmts <- mapM (getFun (VarE handle)) funs
  ret <- mkRet hname ((map (\(x,_,_) -> x)) funs)
  let fun = FunD funName [Clause [] (NormalB $ DoE (ghandle:funStmts ++ [ret])) []]
  io <- getName "IO"
  let libT = mkName name
  let sig = SigD funName (AppT (ConT io) (ConT libT))
  return [sig,fun]


-- Create the transformer function used by get<LibName>
mkTransformer :: String -> Q Dec
mkTransformer f = do
  let name = transName f
  t <- applyArrows <$> mapM getType (prepareFun f)
  fptr <- getName "FunPtr"
  let ftype = AppT (AppT ArrowT (AppT (ConT fptr) t)) t
  return $ ForeignD $ ImportF CCall Safe "dynamic" name ftype


-- |Import a library
importLib
  :: String -- ^The name of the library data type
  -> String -- ^The name of the library
  -> [(String, String, String)] -- ^The functions in the library (Name, CName, Declaration)
  -> Q [Dec]
importLib hname lname xs = do
  let name = mkName hname
  funs <- mapM mkFunDesc $ map (\(x,_,y) -> (x,y)) xs
  transformers <- mapM (mkTransformer) $ nub $ map (\(_,_,x) -> x) xs
  let dhandle = DataD [] name [] [RecC (mkName hname) funs] []
  fun <- mkGetFun lname hname name xs
  return (dhandle:transformers ++ fun)
