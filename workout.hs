-- cabal install happstack
import Happstack.Lite
import Data.Text (Text)
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
 
main:: IO()
main = serve Nothing workoutApp

workoutApp :: ServerPart Response
workoutApp = msum [ homePage ]

homePage :: ServerPart Response
homePage = ok $ template "home page" $ do H.h1 (toHtml "Hello!")

template :: [Char] -> Html -> Response
template title body = toResponse $
                      H.html $ do
                        H.head $ do
                          H.title (toHtml title)
                        H.body $ do
                          body
