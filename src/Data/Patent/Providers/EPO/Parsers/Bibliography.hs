module Data.Patent.Providers.EPO.Parsers.Bibliography
  ( parseBibliography
  , extractBibliography
  ) where

import           Data.Maybe                                 (fromJust)
import qualified Data.Patent.Citation.Parse                 as Parse
import           Data.Patent.Providers.EPO.Parsers.XMLDocDB
import qualified Data.Patent.Types                          as Patent
import qualified Data.Text                                  as T
import           Protolude
import qualified Text.XML                                   as XML
import           Text.XML.Cursor                            (($/), ($//), (&//),
                                                             (>=>))
import qualified Text.XML.Cursor                            as XML

formatCPC :: XML.Cursor -> Patent.CPCCode
formatCPC cursor =
  Patent.CPCCode
  { Patent._cpcSection = section
  , Patent._cpcClass = class_
  , Patent._cpcSubclass = subclass
  , Patent._cpcMainGroup = mainGroup
  , Patent._cpcSubgroup = subGroup
  }
  where
    section = headDef "" $ cursor $/ XML.laxElement "section" &// XML.content
    class_ = headDef "" $ cursor $/ XML.laxElement "class" &// XML.content
    subclass = headDef "" $ cursor $/ XML.laxElement "subclass" &// XML.content
    mainGroup =
      headDef "" $ cursor $/ XML.laxElement "main-group" &// XML.content
    subGroup = headDef "" $ cursor $/ XML.laxElement "subgroup" &// XML.content

isCPC :: XML.Cursor -> Bool
isCPC cursor = T.toLower "CPC" == T.toLower scheme
  where
    scheme =
      headDef "" $
      concat $
      XML.attribute "scheme" <$>
      (cursor $/ XML.laxElement "classification-scheme")

rightLang :: Text -> XML.Cursor -> Bool
rightLang lang cursor = languageAttr == languageFilter
  where
    languageAttr = T.toLower $ headDef "" (XML.attribute "lang" cursor)
    languageFilter = T.toLower lang

parseBibliography :: Text -> XML.Document -> Patent.Bibliography
parseBibliography lang xml = extractBibliography lang $ XML.fromDocument xml

extractBibliography :: Text -> XML.Cursor -> Patent.Bibliography
extractBibliography lang cursor = bib
  where
    pubDate =
      headDef "" $
      cursor $// XML.laxElement "publication-reference" &//
      XML.laxElement "document-id" >=>
      XML.attributeIs "document-id-type" "epodoc" &// XML.laxElement "date" &//
      XML.content
    ipcs =
      cursor $// XML.laxElement "classification-ipc" &// XML.laxElement "text" &//
      XML.content
    cpcs =
      formatCPC <$>
      (cursor $// XML.laxElement "patent-classification" >=> XML.check isCPC)
    appDate =
      headDef "" $
      cursor $// XML.laxElement "application-reference" &//
      XML.laxElement "document-id" >=>
      XML.attributeIs "document-id-type" "epodoc" &// XML.laxElement "date" &//
      XML.content
    appEPODOC =
      headMay $
      parseXMLtoCitation <$>
      (cursor $// XML.laxElement "application-reference" &//
       XML.laxElement "document-id" >=>
       XML.attributeIs "document-id-type" "docdb")
    priorityDates =
      cursor $// XML.laxElement "priority-claim" &//
      XML.laxElement "document-id" >=>
      XML.attributeIs "document-id-type" "epodoc" &// XML.laxElement "date" &//
      XML.content
    priorityDocuments =
      Parse.parseCitation <$>
      (cursor $// XML.laxElement "priority-claim" &//
       XML.laxElement "document-id" >=>
       XML.attributeIs "document-id-type" "epodoc" &//
       XML.laxElement "doc-number" &//
       XML.content)
    applicants =
      cursor $// XML.laxElement "applicant" >=>
      XML.attributeIs "data-format" "epodoc" &// XML.laxElement "name" &//
      XML.content
    inventors =
      cursor $// XML.laxElement "inventor" >=>
      XML.attributeIs "data-format" "epodoc" &// XML.laxElement "name" &//
      XML.content
    langTitle =
      headMay $
      cursor $// XML.laxElement "invention-title" >=>
      XML.check (rightLang lang) &// XML.content
    altTitle =
      headMay $ cursor $// XML.laxElement "invention-title" &// XML.content
    title =
      if isJust langTitle
        then langTitle
        else altTitle
    patentCitations :: [Patent.Citation]
    patentCitations =
      parseXMLtoCitation <$>
      (cursor $// XML.laxElement "patcit" &// XML.laxElement "document-id" >=>
       XML.attributeIs "document-id-type" "docdb")
    langAbstract =
      cursor $// XML.laxElement "abstract" >=>
      XML.check (rightLang lang) &// XML.laxElement "p" &// XML.content
    altAbstract =
      cursor $// XML.laxElement "abstract" &// XML.laxElement "p" &//
      XML.content
    abstract =
      if length langAbstract == 0
        then altAbstract
        else langAbstract
    familyID' =
      headDef ("" :: Text) $
      concat $
      XML.attribute "family-id" <$>
      (cursor $// XML.laxElement "exchange-document")
    bib =
      Patent.Bibliography
      { Patent._biblioPubDate = pubDate
      , Patent._biblioIPCs = ipcs
      , Patent._biblioCPCs = cpcs
      , Patent._biblioAppDate = appDate
      , Patent._biblioAppCitation = fromJust appEPODOC
      , Patent._biblioPriorityDates = priorityDates
      , Patent._biblioPriorityCitations = rights priorityDocuments
      , Patent._biblioApplicants = applicants
      , Patent._biblioInventors = inventors
      , Patent._biblioTitle = fromJust title
      , Patent._biblioAbstract = T.intercalate (T.pack "\n") abstract
      , Patent._biblioPatentCitations = patentCitations
      , Patent._biblioFamilyID = familyID'
      }
