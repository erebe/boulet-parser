import Network.Curl
import Network.Download

import Control.Concurrent

import Text.XML.Light
import Text.Regex.Posix
import Data.List.Utils (replace)
import System.Directory (doesFileExist)
import System.IO (writeFile)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import qualified Data.ByteString as B

url = "http://www.bouletcorp.com/#archives"


-- Get the XML of a web Page
getHTMLPage :: String -> IO(Element)
getHTMLPage url = do 
                (_, page) <- curlGetString url []
                return $ fromJust $ parseXMLDoc page


-- Extract the list of all webcomics
parseArchiveList :: Element -> [(String, String)]
parseArchiveList xml = reverse . sort . nub $ [ (link, fileRacine) | elem <- findElements (QName "a" (Just "http://www.w3.org/1999/xhtml") Nothing ) xml,
                                                              let link = fromJust $ lookupAttr (QName "href" Nothing Nothing) (elAttribs elem),
                                                              link =~ "^http://www.bouletcorp.com/blog/",
                                                              let fileRacine = replace "/" "_" . drop 31 $ link]

-- Check if we already have this page 
checkPagePresent :: (String, String) -> IO(Bool)
checkPagePresent (_,fileRacine) = doesFileExist $ fileRacine ++ "0" ++ ".png"





-- get images' urls from the page 
getPagesImgLinks :: (String, String) -> IO([String])
getPagesImgLinks (url,_) = do
    page <- getHTMLPage url
    
    --Extract the div[class="storycontent"]
    let zoneCentrale = fromJust $ filterElement (\elem -> ((qName $ elName elem) == "div") && 
                                                          (let attr = (findAttr (QName "class" Nothing Nothing) elem) 
                                                                    in (attr /= Nothing) && (fromJust attr) == "storycontent"))
                                                page

    --get src link from every <img/>
    return [ link | elem <- findElements (QName "img" (Just "http://www.w3.org/1999/xhtml") Nothing ) zoneCentrale,
                            let link = fromJust $ lookupAttr (QName "src" Nothing Nothing) (elAttribs elem),
                            link =~ "(png|jpg|jpeg|gif)$"]
    




--Download imgs
downloadImgs :: (String,String) -> [String] -> Integer -> IO()

downloadImgs (x, fileRacine) [] cpt = return ()
downloadImgs (x, fileRacine) (url:urls) cpt = do
    page <- openURI url 
    case page of
        Left page  -> print $ "Error : " ++ page ++ " occur when trying to download " ++ url
        Right page -> B.writeFile (fileRacine ++ (show cpt) ++ ".png") page
    forkIO $ downloadImgs (x,fileRacine) urls (cpt + 1) 
    return ()
    
    
process [] = return ()
process (link:links) = do
    print link 
    exist <- checkPagePresent link
    if exist then return ()
    else do
        imgLinks <- getPagesImgLinks link 
        print imgLinks
        forkIO $ downloadImgs link imgLinks 0
        process links

main = do
    archivePage <- getHTMLPage url
    let links = parseArchiveList archivePage 
    process links
