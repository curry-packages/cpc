module AnsiiToHtml where

import FilePath (replaceExtension)
import List (isPrefixOf)

import HTML.Parser

import CPC.Files (inCpcSubdir)

defaultStart:: String
defaultStart = "<!DOCTYPE html> <html> <body> <span>"

defaultEnd::String
defaultEnd   = "</span> </body> </html>"

convertToHTML::  (String,String) -> IO ()
convertToHTML (ansi,name) = 
  writeFile (inCpcSubdir $ replaceExtension name "html") 
            (convertToHTML' ansi defaultStart)

convertToHTML':: String -> String -> String
convertToHTML' ansi@(x:xs) html  
  | isPrefixOf "\ESC" ansi = handleColor xs html  
  | isPrefixOf "\n"   ansi = convertToHTML' xs (html ++ "<br>")
  | isPrefixOf "\t"   ansi = convertToHTML' xs (html ++ "&nbsp&nbsp")
  | isPrefixOf " "    ansi = convertToHTML' xs (html ++ "&nbsp")
  | otherwise              = convertToHTML' xs (html ++ [x]) 
convertToHTML' [] html     = html ++ defaultEnd


handleColor:: String -> String -> String
handleColor ansi@(a:b:c:d:rest) html  
  | isPrefixOf "[41m" ansi = convertToHTML' rest 
                    (html ++ "</span><span style=\"background-color:red;\">") 
  | isPrefixOf "[42m" ansi = convertToHTML' rest 
                    (html ++ "</span><span style=\"background-color:green;\">")
  | isPrefixOf "[43m" ansi = convertToHTML' rest 
                    (html ++ "</span><span style=\"background-color:yellow;\">")
  | isPrefixOf "[49m" ansi = convertToHTML' rest 
                    (html ++ "</span><span style=\"background-color:white;\">") 
  | otherwise              = error (a:b:c:d:rest)
handleColor [] _        = error "ansii code error"
handleColor [_] _       = error "ansii code error"
handleColor [_, _] _    = error "ansii code error"
handleColor [_, _, _] _ = error "ansii code error"

                                      