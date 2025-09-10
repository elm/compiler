{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Endpoint.Quotes
  ( endpoint
  )
  where


import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.Int (Int(I#))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8(W8#))
import qualified System.Random as Random

import Snap.Core



-- ENDPOINT


endpoint :: Snap ()
endpoint =
  do  modifyResponse
        $ setContentType "application/json"
        . setHeader "Access-Control-Allow-Origin" "*"
      writeBS =<< liftIO getQuote


getQuote :: IO BS.ByteString
getQuote =
  let
    (Quotes sa) = quotes
  in
  do  (I# i) <- Random.randomRIO (0, I# (sizeofSmallArray# sa -# 1#))
      case indexSmallArray# sa i of
        (# json #) ->
          return json



-- QUOTES


data Quotes =
  Quotes (SmallArray# BS.ByteString)


{-# NOINLINE quotes #-}
quotes :: Quotes
quotes =
  let
    !(I# len) = length quoteList
  in
  runST $ ST $ \s ->
    case newSmallArray# len ""       s of { (# s, sma #) ->
    case quotesHelp# sma quoteList   s of {    s         ->
    case unsafeFreezeSmallArray# sma s of { (# s, sa  #) ->
      (# s, Quotes sa #)
    }}}


quotesHelp# :: SmallMutableArray# s a -> [a] -> State# s -> State# s
quotesHelp# sma =
    go 0#
  where
    go i list s =
      case list of
        [] ->
          s

        x:xs ->
          case writeSmallArray# sma i x s of
            s ->
              go (i +# 1#) xs s


quoteList :: [BS.ByteString]
quoteList =
  map toUtf8
    [ "{\"source\":\"Civilization and its Discontents\",\"author\":\"Sigmund Freud\",\"year\":1902,\"quote\":\"The enormous expansion of communications, due to the world-wide telegraph and telephone networks, has entirely transformed the conditions of trade and commerce. Everything is done in haste, at fever pitch. The night is used for travel, the day for business; even \\u201Choliday trips\\u201D put a strain on the nervous system. Great political, industrial and financial crises carry this excitement into far wider areas of the population than ever before. Interest in political life has become universal: tempers are inflamed by political, religious and social struggles, party politics, electioneering and the immense growth of trade-unionism; people are forced to engage in constant mental activity and robbed of the time the need for relaxation, sleep and rest.\"}"
    , "{\"source\":\"A Preface to Politics\",\"author\":\"Walter Lippmann\",\"year\":1913,\"quote\":\"The Economic Man\\u2014that lazy abstraction\\u2014is still paraded in the lecture room.\"}"
    , "{\"source\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"quote\":\"You ask me to say what you should consider it particularly important to avoid. My answer is this: a mass crowd. It is something to which you cannot entrust yourself yet without risk. I at any rate am ready to confess my own frailty in this respect. I never come back home with quite the same moral character as I went out with.\"}"
    , "{\"source\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"quote\":\"December used to be a month but it is now a year\"}"
    , "{\"source\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"quote\":\"But nothing is as ruinous to the character as sitting away one\\u2019s time at a show - for it is then, through the medium of entertainment, that vices creep into one with more than usual ease.\"}"
    , "{\"source\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"quote\":\"All this hurrying from place to place won\\u2019t bring you any relief, for you\\u2019re travelling in the company of your own emotions, followed by your troubles all the way. If only they were really following you! They\\u2019d be farther away from you: as it is they\\u2019re not at your back, but on it! That\\u2019s why they weigh you down with just the same uncomfortable chafing wherever you are. It\\u2019s medicine, not a particular part of the world, that a person needs if he\\u2019s ill. Suppose someone has broken his leg or dislocated a joint; he doesn\\u2019t get into a carriage or board a ship: he calls in a doctor to have the fracture set or the dislocation reduced. Well then, when a person\\u2019s spirit is wrenched or broken at so many points, do you imagine that it can be put right by a change of scenery, that that sort of trouble isn\\u2019t so serious that it can\\u2019t be cured by an outing?\"}"
    , "{\"source\":\"Progress and Poverty\",\"author\":\"Henry George\",\"year\":1879,\"quote\":\"The present century has been marked by a prodigious increase in wealth-producing power. The utilization of steam and electricity, the introduction of improved processes and laborsaving machinery, the greater subdivision and grander scale of production, the wonderful facilitation of exchanges, have multiplied enormously the effectiveness of labor. At the beginning of this marvelous era it was natural to expect, and it was expected, that laborsaving inventions would lighten the toil and improve the condition of the laborer; that the enormous increase in the power of producing wealth would make real poverty a thing of the past.\"}"
    , "{\"source\":\"Imperialism\",\"author\":\"John Hobson\",\"year\":1902,\"quote\":\"Although the new Imperialism has been bad business for the nation, it has been good business for certain classes and certain trades within the nation. The vast expenditure on armaments, the costly wars, the grave risks and embarrassments of foreign policy, the stoppage of political and social reforms within Great Britain, though fraught with great injury to the nation, have served well the present business interests of certain industries and professions.\"}"
    , "{\"source\":\"Neo-Colonialism\",\"author\":\"Kwame Nkrumah\",\"year\":1965,\"quote\":\"The essence of neo-colonialism is that the State which is subject to it is, in theory, independent and has all the outward trappings of international sovereignty. In reality its economic system and thus its political policy is directed from the outside. The methods and form of this direction can take various shapes. For example, in an extreme case the troops of the imperial power may garrison the territory of the neo-colonial State and control the government of it. More often, however, neo-colonialist control is exercised through economic or monetary means. The neo-colonial State may be obliged to take the manufactured products of the imperialist power to the exclusion of competing products from elsewhere. Control over government policy in the neo-colonial State may be secured by payments towards the cost of running the State, by the provision of civil servants in positions where they can dictate policy, and by monetary control over foreign exchange through the imposition of a banking system controlled by the imperial power.\"}"
    , "{\"source\":\"Neo-Colonialism\",\"author\":\"Kwame Nkrumah\",\"year\":1965,\"quote\":\"In the end the situation arises that the only type of aid which the neo-colonialist masters consider as safe is \\u2018military aid\\u2019. Once a neo-colonialist territory is brought to such a state of economic chaos and misery that revolt actually breaks out then, and only then, is there no limit to the generosity of the neo-colonial overlord, provided, of course, that the funds supplied are utilised exclusively for military purposes. Military aid in fact marks the last stage of neo-colonialism and its effect is self-destructive. Sooner or later the weapons supplied pass into the hands of the opponents of the neo-colonialist regime and the war itself increases the social misery which originally provoked it.\"}"
    , "{\"source\":\"Barnyard in your Backyard\",\"author\":\"Gail Damerow\",\"year\":2002,\"quote\":\"Wild jungle fowl, the ancestors of modern chickens, met their nutritional needs by consuming a variety of plants and insects. Given enough room to roam, some of today\\u2019s breeds remain active foragers.\"}"
    , "{\"source\":\"Barnyard in your Backyard\",\"author\":\"Gail Damerow\",\"year\":2002,\"quote\":\"Another way to reduce the cost of feed is to let your chickens roam on a lawn or in a pasture for part of the day. By eating plants, seeds, and insects, they will balance their diet and eat less of the expensive commercial stuff. Take care not to put your chickens on grass or around buildings that have been sprayed with toxins.\"}"
    , "{\"source\":\"Co-operation in Danish Agriculture\",\"author\":\"Harald Faber\",\"year\":1918,\"quote\":\"A plan for improving the butter made from the milk from peasant farms by collecting the milk to be worked at one place was submitted to the Royal Agricultural Society of Denmark as early as 1852, and it pointed out that this could be done on co-operative lines, the peasants being the owners of the factory and sharing the proceeds.\"}"
    , "{\"source\":\"Co-operation in Danish Agriculture\",\"author\":\"Harald Faber\",\"year\":1918,\"quote\":\"It was the Danish peasants themselves who found a practical way of developing the dairy industry, and they found this by applying the co-operative principles introduced by the Rochdale weavers. They received no support and only lukewarm sympathy from large farmers and estate owners, until later on when the co-operative dairies were doing so well that even owners of estates with two to three hundred cows found it to their advantage to close their private dairies and to join co-operative dairies.\"}"
    , "{\"source\":\"Beekeeping: A Practical Guide\",\"author\":\"Richard Bonney\",\"year\":1993,\"quote\":\"How many of you have been in a classroom or a training program where the instructor says at the outset, \\u201CLook at the person on your right; now look at the person on your left. One of you won\\u2019t be here next year.\\u201D Beekeeping is like that. Furthermore, not all beekeepers are truly beekeepers. Some are beehavers.\"}"
    , "{\"source\":\"The Rodale Book of Composting\",\"author\":\"Deborah Martin and Grace Gershuny\",\"year\":1992,\"quote\":\"Earthworms. If bacteria are the champion microscopic decomposers, then the heavyweight champion is doubtless the earthworm. Pages of praise have been written to the earthworm, ever since it became known that this creature spends most of its time tilling and enriching the soil.\"}"
    , "{\"source\":\"War is a Racket\",\"author\":\"General Smedley Butler\",\"year\":1935,\"quote\":\"There are 40,000,000 men under arms in the world today, and our statesmen and diplomats have the temerity to say that war is not in the making. Hell\\u2019s bells! Are these 40,000,000 men being trained to be dancers?\"}"
    , "{\"source\":\"Root Cellaring\",\"author\":\"Mike and Nancy Bubel\",\"year\":1991,\"quote\":\"What is not so widely acknowledged is that the soil in which your plants grow can influence the keeping quality of the vegetables you harvest. According to studies reported in E. P. Shirakov\\u2019s \\u201CPractical Course in Storage and Processing of Fruits and Vegetables\\u201D, abundant potash in the soil promotes long storage life of fruits and vegetables grown on that soil.\"}"
    , "{\"source\":\"Angora Goats the Northern Way\",\"author\":\"Susan Black Drummond\",\"year\":1985,\"quote\":\"Because of its long wearing qualities and resistance to soil, mohair was once a major upholstery fabric in this country, and was the seat cover material in many cars and in British Railroad seats. The synthetic fiber craze put an end to mohair use as upholstery fabric here.\"}"
    , "{\"source\":\"Making a Newspaper\",\"author\":\"John Given\",\"year\":1912,\"quote\":\"Yellow journalism originated through a desire to gain readers and advertisers, and it produced results. Its original disciples have readers by the hundred thousand and they have about all the advertising that they can well handle.\"}"
    , "{\"source\":\"Making a Newspaper\",\"author\":\"John Given\",\"year\":1912,\"quote\":\"With the adding of every new train and the improvement of every new time-table, the building of every new trolley road, and the establishment of every new mail route, the big city papers are enabled to reach out further, and, wherever they reach, the local papers suffer [...] There is not a paper published in any city between New York and Boston that has not in the last ten years been hurt by the papers of these two cities.\"}"
    , "{\"source\":\"Muscles: Testing and Function (4th Edition)\",\"author\":\"Florence Kendall, Elizabeth McCreary, Patricia Provance\",\"year\":1993,\"quote\":\"The mechanics of the low back is inseparable from that of the overall posture but especially that of the pelvis and the lower extremities. Pain manifested in the leg may be due to an underlying problem in the back. Conversely, the symptoms appearing in the low back may be due to underlying faulty mechanics of the feet, legs, or pelvis.\"}"
    , "{\"source\":\"Farmers of Forty Centuries\",\"author\":\"F. H. King\",\"year\":1911,\"quote\":\"The Sikiang is one of the great rivers of China and indeed of the world. Its width at Wuchow at low water was nearly a mile and our steamer anchored in twenty-four feet of water to a floating dock made fast by huge iron chains reaching three hundred feet up the slop to the city proper, thus providing for a rise of twenty-six feet in the river at its flood stage during the rainy season.\"}"
    , "{\"source\":\"How to Grow More Vegetables\",\"author\":\"John Jeavons\",\"year\":1974,\"quote\":\"If you are having trouble with birds eating the berries in your berry patch, you could erect a wren house in the middle of it. Wrens are insectivores, and they will not bother the berries. But they will attack any bird, however large, that comes near their nest.\"}"
    , "{\"source\":\"Confidence Men and Painted Women\",\"author\":\"Karen Halttunen\",\"year\":1982,\"quote\":\"Even one\\u2019s neighbor next door, as a visitor to Pittsburgh commented in 1818, might be a stranger: \\u201CA next door neighbor is, with them, frequently unknown, and months and years pass, without their exchanging with each other the ordinary compliments of friendship and goodwill.\\u201D\"}"
    , "{\"source\":\"Secondhand Time\",\"author\":\"Svetlana Alexievich\",\"year\":2016,\"quote\":\"I couldn\\u2019t understand what was going on. I remember seeing Gaidar on TV saying, \\u201Clearn how to sell... The market will save us...\\u201D You buy a bottle of mineral water on one corner and sell it on another\\u2014that\\u2019s business. The people listened, bewildered. I would come home, lock the door, and weep.\"}"
    , "{\"source\":\"Hiroshima\",\"author\":\"John Hersey\",\"year\":1985,\"quote\":\"At exactly fifteen minutes past eight in the morning, on August 6, 1945, Japanese time, at the moment when the atomic bomb flashed above Hiroshima, Miss Toshiko Sasaki, a clerk in the personnel department of the East Asia Tin Works, had just sat down at her place in the plant office and was turning her head to speak to the girl at the next desk.\"}"
    , "{\"source\":\"The Orphan\",\"author\":\"Maxim Gorky\",\"year\":1899,\"quote\":\"On a foggy and rainy day, by the cemetery gate, a small group of people standing in a muddy puddle were bargaining with the cabman. \\u201CFifteen kopeks!\\u201D The tall and heavy-set priest exclaimed in a deep bass, in response to the cabman\\u2019s shouts asking for twenty-five kopecks.\"}"
    , "{\"source\":\"Shah of Shahs\",\"author\":\"Ryszard Kapuściński\",\"year\":1982,\"quote\":\"This plane flies out of Teheran every day and lands at Munich at noon. Waiting limousines carry the passengers to elegant restaurants for lunch. After lunch they all fly back to Teheran in the same airplane and eat their suppers at home. Hardly an expensive entertainment, the jaunt costs only two thousand dollars a head. For people in the Shah\\u2019s favor, such a sum is nothing.\"}"
    , "{\"source\":\"Introduction to Fortran IV\",\"author\":\"Paul Chirlian\",\"year\":1973,\"quote\":\"There are many programming languages that can be used. We shall discuss a language that is almost universally used, and which is particularly applicable to mathematical and scientific computations. This language is FORTRAN IV.\"}"
    ]



-- TO UTF8 BYTESTRING


toUtf8 :: [Char] -> BS.ByteString
toUtf8 chars =
  unsafePerformIO $
    do  mba <- newByteArray (sum (map getWidth chars))
        writeChars mba 0 chars


writeChars :: MBA -> Int -> [Char] -> IO BS.ByteString
writeChars mba !offset chars =
  case chars of
    [] ->
      finalize mba

    char : chars
      | n < 0x80 ->
          do  writeWord8 mba (offset    ) (fromIntegral n)
              writeChars mba (offset + 1) chars

      | n < 0x800 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 6         ) + 0xC0))
              writeWord8 mba (offset + 1) (fromIntegral ((       n   .&. 0x3F) + 0x80))
              writeChars mba (offset + 2) chars

      | n < 0x10000 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 12         ) + 0xE0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeChars mba (offset + 3) chars

      | otherwise ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 18         ) + 0xF0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n 12 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 3) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeChars mba (offset + 4) chars

      where
        n = Char.ord char


getWidth :: Char -> Int
getWidth char
  | code < 0x80    = 1
  | code < 0x800   = 2
  | code < 0x10000 = 3
  | otherwise      = 4
  where
    code = Char.ord char



-- MUTABLE BYTE ARRAYS


data MBA =
  MBA (MutableByteArray# RealWorld)


newByteArray :: Int -> IO MBA
newByteArray (I# len) =
  IO $ \s ->
    case newPinnedByteArray# len s of
      (# s, mba #) -> (# s, MBA mba #)


writeWord8 :: MBA -> Int -> Word8 -> IO ()
writeWord8 (MBA mba) (I# offset) (W8# w) =
  IO $ \s ->
    case writeWord8Array# mba offset w s of
      s -> (# s, () #)


finalize :: MBA -> IO BS.ByteString
finalize (MBA mba) =
  IO $ \s ->
    case getSizeofMutableByteArray# mba s of { (# s, len #) ->
    case mutableByteArrayContents# mba    of {       addr   ->
      (# s, BS.BS (ForeignPtr addr (PlainPtr mba)) (I# len) #)
    }}

