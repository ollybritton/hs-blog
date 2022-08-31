module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt ->
            Html.h_ n txt
        Markup.Paragraph txt ->
            Html.p_ txt
        Markup.UnorderedList list ->
            Html.ul_ $ map Html.p_ list
        Markup.OrderedList list ->
            Html.ol_ $ map Html.p_ list
        Markup.CodeBlock list ->
            Html.code_ (unlines list)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
