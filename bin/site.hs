{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (>>^), (***), arr)
import Control.Monad (forM_)
import Data.Monoid (mempty, mconcat)
import System.FilePath
import Hakyll
    
main :: IO ()
main = hakyll $ do

-----------------------------------------------------------------------
-- compile templates
-----------------------------------------------------------------------

    match "templates/*" $ compile templateCompiler

-----------------------------------------------------------------------
-- copy resources files & process css as necessary
-----------------------------------------------------------------------
    match   "resources/css/**" $ do
        route   $ setRoot `composeRoutes` setExtension "css"
        compile sass
    --  compile $ byExtension (error "not a valid css file")
    --          [ (".css",  sass)
    --          , (".scss", sass) ]

    match (predicate (\i -> matches "resources/**" i && not (matches "resources/css/**" i))) $ do
        route   setRoot
        compile copyFileCompiler

-- create site pages
-----------------------------------------------------------------------

    forM_ indexPages $ \p -> match p $ do
        route   $ setRoot `composeRoutes` toIndex
        compile $ unGitHubCompiler
            >>> applyTemplateCompiler "templates/page.html"
            >>> applyTemplateCompiler "templates/default.html"

    forM_ markdownPages $ \p -> match p $ do
        route   $ setRoot `composeRoutes` cleanURL
        compile $ unGitHubCompiler
            >>> applyTemplateCompiler "templates/page.html"
            >>> applyTemplateCompiler "templates/default.html"

    match "p*/**/index.html" $ do
        route   setRoot
        compile defaultCompiler

    match "p*/***.html" $ do
        route   $ setRoot `composeRoutes` cleanURL
        compile defaultCompiler

    match (predicate (\i -> matches "p*/**" i && not (matches "*.m*d" i))) $ do
        route   $ setRoot `composeRoutes` idRoute
        compile copyFileCompiler

-- special root pages
-----------------------------------------------------------------------

    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "pagetitle" "Ethan Schoonover")
        >>> requireAllA "posts/***.md" (id *** arr newest10 >>> postList)
        >>> requireAllA "projects/*/*.md" projectList
        >>> applyTemplateCompiler "templates/home.html"
        >>> applyTemplateCompiler "templates/default.html"

    match  "./posts/index.html" $ route idRoute
    create "./posts/index.html"  $ constA mempty
        >>> arr (setField "pagetitle" "Posts - Ethan Schoonover")
        >>> requireAllA "posts/***.md" postList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    match  "./projects/index.html" $ route idRoute
    create "./projects/index.html" $ constA mempty
        >>> arr (setField "pagetitle" "Projects - Ethan Schoonover")
        >>> requireAllA "projects/*/*.md" projectList
        >>> applyTemplateCompiler "templates/projects.html"
        >>> applyTemplateCompiler "templates/default.html"

-- render rss feed
-----------------------------------------------------------------------
    match  "atom.xml" $ route  idRoute
    create "atom.xml" $
        requireAll_ "projects/*/*.md" >>> renderAtom feedConfiguration

  where
    indexPages =    [ "p*/**README.*"
                    , "p*/**index.*" ]
    markdownPages = [ "p*/***.md"
                    , "p*/***.mkd"
                    , "p*/***.mkdn"
                    , "p*/***.mdown"
                    , "p*/***.markdown"
                    , "p*/***.pandoc"
                    , "p*/***.pdc"
                    , "p*/***.lhs" ]
-----------------------------------------------------------------------
-- custom compilers
-----------------------------------------------------------------------

sass :: Compiler Resource String
sass = getResourceString >>> unixFilter "sass" ["-s", "--scss"]
                         >>> arr compressCss

unGitHub :: Compiler String String
unGitHub = unixFilter "sed" ["s+https://github.com/[^/]*/\\([^:/]*\\)/raw/master+/\\1+g"]

defaultCompiler ::  Compiler Resource (Page String)
defaultCompiler =   getResourceString
                    >>> arr readPage
                    >>> addDefaultFields
                    >>> arr applySelf
                    >>> arr buildTitle
                    >>> pageRenderPandoc
                    >>> applyTemplateCompiler "templates/default.html"

unGitHubCompiler :: Compiler Resource (Page String)
unGitHubCompiler =  getResourceString
                    >>> unGitHub
                    >>> arr readPage
                    >>> addDefaultFields
                    >>> arr applySelf
                    >>> arr buildTitle
                    >>> pageRenderPandoc

-- custom routes
-----------------------------------------------------------------------
setRoot :: Routes
setRoot = customRoute stripTopDir

stripTopDir :: Identifier -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

cleanURL :: Routes
cleanURL = customRoute fileToDirectory

fileToDirectory :: Identifier -> FilePath
fileToDirectory = (flip combine) "index.html" . dropExtension . toFilePath

toIndex :: Routes
toIndex = customRoute fileToIndex

fileToIndex :: Identifier -> FilePath
fileToIndex = (flip combine) "index.html" . dropFileName . toFilePath

-- misc functions
-----------------------------------------------------------------------
newest10 :: [Page a] -> [Page a]
newest10 = take 10 . reverse . sortByBaseName

stripIndexLink :: Page a -> Page a
stripIndexLink = changeField "url" dropFileName

buildTitle :: Page a -> Page a
buildTitle = renderField "title" "pagetitle" (++ " - Ethan Schoonover")

postList :: Compiler (Page String, [Page String]) (Page String)
postList = buildList "postlist" "templates/item.html"

projectList :: Compiler (Page String, [Page String]) (Page String)
projectList = buildList "projectlist" "templates/item.html"

buildList :: String -> Identifier -> Compiler (Page String, [Page String]) (Page String)
buildList field template = setFieldA field $
    arr (reverse . sortByBaseName)
        >>> arr (map stripIndexLink)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- misc functions
-----------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration =  FeedConfiguration
    { feedTitle       = "EthanSchoonover.com"
    , feedDescription = "Projects & Posts from Ethan Schoonover"
    , feedAuthorName  = "Ethan Schoonover"
    , feedRoot        = "http://ethanschoonover.com"
    }

