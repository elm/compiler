{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Licenses
  ( License
  , check
  , bsd3
  , encode
  )
  where


import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Json.Encode as Encode
import qualified Reporting.Suggest as Suggest



-- LICENCES


data License =
  License
    { _name :: Text.Text
    , _code :: Text.Text
    }
  deriving (Eq, Ord)


bsd3 :: License
bsd3 =
  osiApprovedSpdxLicenses ! "BSD-3-Clause"


encode :: License -> Encode.Value
encode (License _ code) =
  Encode.text code



-- CHECK


check :: Text.Text -> Either [Text.Text] License
check rawName =
  case Map.lookup rawName osiApprovedSpdxLicenses of
    Just license ->
      Right license

    Nothing ->
      let
        toPairs (License name code) =
          [ (Text.unpack code, code), (Text.unpack name, code) ]

        pairs =
          concatMap toPairs (Map.elems osiApprovedSpdxLicenses)
      in
      Left $ map snd $
        Suggest.sort (Text.unpack rawName) fst pairs



-- LIST OF LICENCES


(==>) :: Text.Text -> Text.Text -> (Text.Text, License)
(==>) code name =
  ( code, License name code )


--
-- OSI approved licenses in SPDX format.
-- <https://spdx.org/licenses/>
--
osiApprovedSpdxLicenses :: Map.Map Text.Text License
osiApprovedSpdxLicenses =
  Map.fromList
    [ "AFL-1.1" ==> "Academic Free License v1.1"
    , "AFL-1.2" ==> "Academic Free License v1.2"
    , "AFL-2.0" ==> "Academic Free License v2.0"
    , "AFL-2.1" ==> "Academic Free License v2.1"
    , "AFL-3.0" ==> "Academic Free License v3.0"
    , "APL-1.0" ==> "Adaptive Public License 1.0"
    , "Apache-1.1" ==> "Apache License 1.1"
    , "Apache-2.0" ==> "Apache License 2.0"
    , "APSL-1.0" ==> "Apple Public Source License 1.0"
    , "APSL-1.1" ==> "Apple Public Source License 1.1"
    , "APSL-1.2" ==> "Apple Public Source License 1.2"
    , "APSL-2.0" ==> "Apple Public Source License 2.0"
    , "Artistic-1.0" ==> "Artistic License 1.0"
    , "Artistic-1.0-Perl" ==> "Artistic License 1.0 (Perl)"
    , "Artistic-1.0-cl8" ==> "Artistic License 1.0 w/clause 8"
    , "Artistic-2.0" ==> "Artistic License 2.0"
    , "AAL" ==> "Attribution Assurance License"
    , "BSL-1.0" ==> "Boost Software License 1.0"
    , "BSD-2-Clause" ==> "BSD 2-clause \"Simplified\" License"
    , "BSD-3-Clause" ==> "BSD 3-clause \"New\" or \"Revised\" License"
    , "0BSD" ==> "BSD Zero Clause License"
    , "CECILL-2.1" ==> "CeCILL Free Software License Agreement v2.1"
    , "CNRI-Python" ==> "CNRI Python License"
    , "CDDL-1.0" ==> "Common Development and Distribution License 1.0"
    , "CPAL-1.0" ==> "Common Public Attribution License 1.0"
    , "CPL-1.0" ==> "Common Public License 1.0"
    , "CATOSL-1.1" ==> "Computer Associates Trusted Open Source License 1.1"
    , "CUA-OPL-1.0" ==> "CUA Office Public License v1.0"
    , "EPL-1.0" ==> "Eclipse Public License 1.0"
    , "ECL-1.0" ==> "Educational Community License v1.0"
    , "ECL-2.0" ==> "Educational Community License v2.0"
    , "EFL-1.0" ==> "Eiffel Forum License v1.0"
    , "EFL-2.0" ==> "Eiffel Forum License v2.0"
    , "Entessa" ==> "Entessa Public License v1.0"
    , "EUDatagrid" ==> "EU DataGrid Software License"
    , "EUPL-1.1" ==> "European Union Public License 1.1"
    , "Fair" ==> "Fair License"
    , "Frameworx-1.0" ==> "Frameworx Open License 1.0"
    , "AGPL-3.0" ==> "GNU Affero General Public License v3.0"
    , "GPL-2.0" ==> "GNU General Public License v2.0 only"
    , "GPL-3.0" ==> "GNU General Public License v3.0 only"
    , "LGPL-2.1" ==> "GNU Lesser General Public License v2.1 only"
    , "LGPL-3.0" ==> "GNU Lesser General Public License v3.0 only"
    , "LGPL-2.0" ==> "GNU Library General Public License v2 only"
    , "HPND" ==> "Historic Permission Notice and Disclaimer"
    , "IPL-1.0" ==> "IBM Public License v1.0"
    , "Intel" ==> "Intel Open Source License"
    , "IPA" ==> "IPA Font License"
    , "ISC" ==> "ISC License"
    , "LPPL-1.3c" ==> "LaTeX Project Public License v1.3c"
    , "LiLiQ-P-1.1" ==> "Licence Libre du Québec – Permissive version 1.1"
    , "LiLiQ-Rplus-1.1" ==> "Licence Libre du Québec – Réciprocité forte version 1.1"
    , "LiLiQ-R-1.1" ==> "Licence Libre du Québec – Réciprocité version 1.1"
    , "LPL-1.02" ==> "Lucent Public License v1.02"
    , "LPL-1.0" ==> "Lucent Public License Version 1.0"
    , "MS-PL" ==> "Microsoft Public License"
    , "MS-RL" ==> "Microsoft Reciprocal License"
    , "MirOS" ==> "MirOS Licence"
    , "MIT" ==> "MIT License"
    , "Motosoto" ==> "Motosoto License"
    , "MPL-1.0" ==> "Mozilla Public License 1.0"
    , "MPL-1.1" ==> "Mozilla Public License 1.1"
    , "MPL-2.0" ==> "Mozilla Public License 2.0"
    , "MPL-2.0-no-copyleft-exception" ==> "Mozilla Public License 2.0 (no copyleft exception)"
    , "Multics" ==> "Multics License"
    , "NASA-1.3" ==> "NASA Open Source Agreement 1.3"
    , "Naumen" ==> "Naumen Public License"
    , "NGPL" ==> "Nethack General Public License"
    , "Nokia" ==> "Nokia Open Source License"
    , "NPOSL-3.0" ==> "Non-Profit Open Software License 3.0"
    , "NTP" ==> "NTP License"
    , "OCLC-2.0" ==> "OCLC Research Public License 2.0"
    , "OGTSL" ==> "Open Group Test Suite License"
    , "OSL-1.0" ==> "Open Software License 1.0"
    , "OSL-2.0" ==> "Open Software License 2.0"
    , "OSL-2.1" ==> "Open Software License 2.1"
    , "OSL-3.0" ==> "Open Software License 3.0"
    , "OSET-PL-2.1" ==> "OSET Public License version 2.1"
    , "PHP-3.0" ==> "PHP License v3.0"
    , "PostgreSQL" ==> "PostgreSQL License"
    , "Python-2.0" ==> "Python License 2.0"
    , "QPL-1.0" ==> "Q Public License 1.0"
    , "RPSL-1.0" ==> "RealNetworks Public Source License v1.0"
    , "RPL-1.1" ==> "Reciprocal Public License 1.1"
    , "RPL-1.5" ==> "Reciprocal Public License 1.5"
    , "RSCPL" ==> "Ricoh Source Code Public License"
    , "OFL-1.1" ==> "SIL Open Font License 1.1"
    , "SimPL-2.0" ==> "Simple Public License 2.0"
    , "Sleepycat" ==> "Sleepycat License"
    , "SISSL" ==> "Sun Industry Standards Source License v1.1"
    , "SPL-1.0" ==> "Sun Public License v1.0"
    , "Watcom-1.0" ==> "Sybase Open Watcom Public License 1.0"
    , "UPL-1.0" ==> "Universal Permissive License v1.0"
    , "NCSA" ==> "University of Illinois/NCSA Open Source License"
    , "VSL-1.0" ==> "Vovida Software License v1.0"
    , "W3C" ==> "W3C Software Notice and License (2002-12-31)"
    , "Xnet" ==> "X.Net License"
    , "Zlib" ==> "zlib License"
    , "ZPL-2.0" ==> "Zope Public License 2.0"
    ]
