module Generate.Markdown where

import Text.Pandoc
import qualified Data.Set as Set

toHtml :: String -> String
toHtml = writeHtmlString writeOptions . readMarkdown readOptions

readOptions =
    def { readerExtensions  = elmExtensions
        , readerApplyMacros = False
        }

writeOptions =
    def { writerExtensions = elmExtensions
        , writerHighlight = True
        }

elmExtensions :: Set.Set Extension
elmExtensions = Set.fromList
  [ Ext_raw_html
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  , Ext_inline_code_attributes
  , Ext_markdown_in_html_blocks
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_auto_identifiers
  , Ext_header_attributes
  , Ext_implicit_header_references
  ]