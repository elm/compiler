{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Endpoint.Quotes
  ( endpoint
  )
  where


import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import GHC.Int (Int(I#))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import qualified System.Random as Random

import Snap.Core



-- ENDPOINT


endpoint :: Snap ()
endpoint =
  do  modifyResponse $ setContentType "application/json"
      writeBS =<< liftIO getQuote


getQuote :: IO BS.ByteString
getQuote =
  let
    (Quotes sa#) = quotes
  in
  do  (I# i#) <- Random.randomRIO (0, I# (sizeofSmallArray# sa# -# 1#))
      case indexSmallArray# sa# i# of
        (# json #) ->
          return json



-- QUOTES


data Quotes =
  Quotes (SmallArray# BS.ByteString)


quotes :: Quotes
quotes =
  let
    !(I# len#) = length quoteList
  in
  runST $ ST $ \s ->
    case newSmallArray# len# "" s of
      (# s, sma# #) ->
        case quotesHelp# sma# quoteList s of
          s ->
            case unsafeFreezeSmallArray# sma# s of
              (# s, sa# #) ->
                (# s, Quotes sa# #)


quotesHelp# :: SmallMutableArray# s a -> [a] -> State# s -> State# s
quotesHelp# sma# =
    go 0#
  where
    go i# list s =
      case list of
        [] ->
          s

        x:xs ->
          case writeSmallArray# sma# i# x s of
            s ->
              go (i# +# 1#) xs s


quoteList :: [BS.ByteString]
quoteList =
  [ "{\"title\":\"Civilization and its Discontents\",\"author\":\"Sigmund Freud\",\"year\":1902,\"text\":\"The enormous expansion of communications, due to the world-wide telegraph and telephone networks, has entirely transformed the conditions of trade and commerce. Everything is done in haste, at fever pitch. The night is used for travel, the day for business; even “holiday trips” put a strain on the nervous system. Great political, industrial and financial crises carry this excitement into far wider areas of the population than ever before. Interest in political life has become universal: tempers are inflamed by political, religious and social struggles, party politics, electioneering and the immense growth of trade-unionism; people are forced to engage in constant mental activity and robbed of the time the need for relaxation, sleep and rest.\"}"
  , "{\"title\":\"A Preface to Politics\",\"author\":\"Walter Lippmann\",\"year\":1913,\"text\":\"The Economic Man—that lazy abstraction—is still paraded in the lecture room.\"}"
  , "{\"title\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"text\":\"You ask me to say what you should consider it particularly important to avoid. My answer is this: a mass crowd. It is something to which you cannot entrust yourself yet without risk. I at any rate am ready to confess my own frailty in this respect. I never come back home with quite the same moral character as I went out with.\"}"
  , "{\"title\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"text\":\"December used to be a month but it is now a year\"}"
  , "{\"title\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"text\":\"But nothing is as ruinous to the character as sitting away one’s time at a show - for it is then, through the medium of entertainment, that vices creep into one with more than usual ease.\"}"
  , "{\"title\":\"Letters from a Stoic\",\"author\":\"Seneca\",\"year\":54,\"text\":\"All this hurrying from place to place won’t bring you any relief, for you’re travelling in the company of your own emotions, followed by your troubles all the way. If only they were really following you! They’d be farther away from you: as it is they’re not at your back, but on it! That’s why they weigh you down with just the same uncomfortable chafing wherever you are. It’s medicine, not a particular part of the world, that a person needs if he’s ill. Suppose someone has broken his leg or dislocated a joint; he doesn’t get into a carriage or board a ship: he calls in a doctor to have the fracture set or the dislocation reduced. Well then, when a person’s spirit is wrenched or broken at so many points, do you imagine that it can be put right by a change of scenery, that that sort of trouble isn’t so serious that it can’t be cured by an outing?\"}"
  , "{\"title\":\"Progress and Poverty\",\"author\":\"Henry George\",\"year\":1879,\"text\":\"The present century has been marked by a prodigious increase in wealth-producing power. The utilization of steam and electricity, the introduction of improved processes and laborsaving machinery, the greater subdivision and grander scale of production, the wonderful facilitation of exchanges, have multiplied enormously the effectiveness of labor. At the beginning of this marvelous era it was natural to expect, and it was expected, that laborsaving inventions would lighten the toil and improve the condition of the laborer; that the enormous increase in the power of producing wealth would make real poverty a thing of the past.\"}"
  , "{\"title\":\"Imperialism\",\"author\":\"John Hobson\",\"year\":1902,\"text\":\"Although the new Imperialism has been bad business for the nation, it has been good business for certain classes and certain trades within the nation. The vast expenditure on armaments, the costly wars, the grave risks and embarrassments of foreign policy, the stoppage of political and social reforms within Great Britain, though fraught with great injury to the nation, have served well the present business interests of certain industries and professions.\"}"
  , "{\"title\":\"Neo-Colonialism\",\"author\":\"Kwame Nkrumah\",\"year\":1965,\"text\":\"The essence of neo-colonialism is that the State which is subject to it is, in theory, independent and has all the outward trappings of international sovereignty. In reality its economic system and thus its political policy is directed from the outside. The methods and form of this direction can take various shapes. For example, in an extreme case the troops of the imperial power may garrison the territory of the neo-colonial State and control the government of it. More often, however, neo-colonialist control is exercised through economic or monetary means. The neo-colonial State may be obliged to take the manufactured products of the imperialist power to the exclusion of competing products from elsewhere. Control over government policy in the neo-colonial State may be secured by payments towards the cost of running the State, by the provision of civil servants in positions where they can dictate policy, and by monetary control over foreign exchange through the imposition of a banking system controlled by the imperial power.\"}"
  , "{\"title\":\"Neo-Colonialism\",\"author\":\"Kwame Nkrumah\",\"year\":1965,\"text\":\"In the end the situation arises that the only type of aid which the neo-colonialist masters consider as safe is ‘military aid’. Once a neo-colonialist territory is brought to such a state of economic chaos and misery that revolt actually breaks out then, and only then, is there no limit to the generosity of the neo-colonial overlord, provided, of course, that the funds supplied are utilised exclusively for military purposes. Military aid in fact marks the last stage of neo-colonialism and its effect is self-destructive. Sooner or later the weapons supplied pass into the hands of the opponents of the neo-colonialist regime and the war itself increases the social misery which originally provoked it.\"}"
  , "{\"title\":\"Barnyard in your Backyard\",\"author\":\"Gail Damerow\",\"year\":2002,\"text\":\"Wild jungle fowl, the ancestors of modern chickens, met their nutritional needs by consuming a variety of plants and insects. Given enough room to roam, some of today’s breeds remain active foragers.\"}"
  , "{\"title\":\"Barnyard in your Backyard\",\"author\":\"Gail Damerow\",\"year\":2002,\"text\":\"Another way to reduce the cost of feed is to let your chickens roam on a lawn or in a pasture for part of the day. By eating plants, seeds, and insects, they will balance their diet and eat less of the expensive commercial stuff. Take care not to put your chickens on grass or around buildings that have been sprayed with toxins.\"}"
  , "{\"title\":\"Co-operation in Danish Agriculture\",\"author\":\"Harald Faber\",\"year\":1918,\"text\":\"A plan for improving the butter made from the milk from peasant farms by collecting the milk to be worked at one place was submitted to the Royal Agricultural Society of Denmark as early as 1852, and it pointed out that this could be done on co-operative lines, the peasants being the owners of the factory and sharing the proceeds.\"}"
  , "{\"title\":\"Co-operation in Danish Agriculture\",\"author\":\"Harald Faber\",\"year\":1918,\"text\":\"It was the Danish peasants themselves who found a practical way of developing the dairy industry, and they found this by applying the co-operative principles introduced by the Rochdale weavers. They received no support and only lukewarm sympathy from large farmers and estate owners, until later on when the co-operative dairies were doing so well that even owners of estates with two to three hundred cows found it to their advantage to close their private dairies and to join co-operative dairies.\"}"
  , "{\"title\":\"Beekeeping: A Practical Guide\",\"author\":\"Richard Bonney\",\"year\":1993,\"text\":\"How many of you have been in a classroom or a training program where the instructor says at the outset, “Look at the person on your right; now look at the person on your left. One of you won’t be here next year.” Beekeeping is like that. Furthermore, not all beekeepers are truly beekeepers. Some are beehavers.\"}"
  , "{\"title\":\"The Monied Metropolis\",\"author\":\"Sven Beckert\",\"year\":2001,\"text\":\"On February 10, 1897, at the tail end of the most severe economic depression the United States had experienced in the nineteenth century, 700 merchants, industrialists, bankers, and professionals assembled at New York’s Waldorf-Astoria for a costume ball. [...] Caroline Astor had gems worth $250,000 sewn into her dress. Cornelia Martin, not to be outdone, wore a necklace once owned by none other than Marie Antoinette herself. [...] the rooms themselves were decorated to resemble the great hall of Versailles [...] threats of bombs kept not only New York’s police but also a hired army of Pinkerton detectives on alert, watching “for thieves or for men of socialistic tendencies.” As a further precaution, the first-floor windows of the Waldorf Hotel were nailed shut.\"}"
  , "{\"title\":\"War is a Racket\",\"author\":\"General Smedley Butler\",\"year\":1935,\"text\":\"There are 40,000,000 men under arms in the world today, and our statesmen and diplomats have the temerity to say that war is not in the making. Hell’s bells! Are these 40,000,000 men being trained to be dancers?\"}"
  , "{\"title\":\"Root Cellaring\",\"author\":\"Mike and Nancy Bubel\",\"year\":1991,\"text\":\"What is not so widely acknowledged is that the soil in which your plants grow can influence the keeping quality of the vegetables you harvest. According to studies reported in E. P. Shirakov’s “Practical Course in Storage and Processing of Fruits and Vegetables”, abundant potash in the soil promotes long storage life of fruits and vegetables grown on that soil.\"}"
  , "{\"title\":\"Angora Goats the Northern Way\",\"author\":\"Susan Black Drummond\",\"year\":1985,\"text\":\"Because of its long wearing qualities and resistance to soil, mohair was once a major upholstery fabric in this country, and was the seat cover material in many cars and in British Railroad seats. The synthetic fiber craze put an end to mohair use as upholstery fabric here.\"}"
  , "{\"title\":\"Making a Newspaper\",\"author\":\"John Given\",\"year\":1912,\"text\":\"Yellow journalism originated through a desire to gain readers and advertisers, and it produced results. Its original disciples have readers by the hundred thousand and they have about all the advertising that they can well handle.\"}"
  , "{\"title\":\"Making a Newspaper\",\"author\":\"John Given\",\"year\":1912,\"text\":\"With the adding of every new train and the improvement of every new time-table, the building of every new trolley road, and the establishment of every new mail route, the big city papers are enabled to reach out further, and, wherever they reach, the local papers suffer [...] There is not a paper published in any city between New York and Boston that has not in the last ten years been hurt by the papers of these two cities.\"}"
  , "{\"title\":\"Muscles: Testing and Function (4th Edition)\",\"author\":\"Florence Kendall, Elizabeth McCreary, Patricia Provance\",\"year\":1993,\"text\":\"The mechanics of the low back is inseparable from that of the overall posture but especially that of the pelvis and the lower extremities. Pain manifested in the leg may be due to an underlying problem in the back. Conversely, the symptoms appearing in the low back may be due to underlying faulty mechanics of the feet, legs, or pelvis.\"}"
  , "{\"title\":\"Farmers of Forty Centuries\",\"author\":\"F. H. King\",\"year\":1911,\"text\":\"The Sikiang is one of the great rivers of China and indeed of the world. Its width at Wuchow at low water was nearly a mile and our steamer anchored in twenty-four feet of water to a floating dock made fast by huge iron chains reaching three hundred feet up the slop to the city proper, thus providing for a rise of twenty-six feet in the river at its flood stage during the rainy season.\"}"
  , "{\"title\":\"How to Grow More Vegetables\",\"author\":\"John Jeavons\",\"year\":1974,\"text\":\"If you are having trouble with birds eating the berries in your berry patch, you could erect a wren house in the middle of it. Wrens are insectivores, and they will not bother the berries. But they will attack any bird, however large, that comes near their nest.\"}"
  , "{\"title\":\"Confidence Men and Painted Women\",\"author\":\"Karen Halttunen\",\"year\":1982,\"text\":\"Even one’s neighbor next door, as a visitor to Pittsburgh commented in 1818, might be a stranger: “A next door neighbor is, with them, frequently unknown, and months and years pass, without their exchanging with each other the ordinary compliments of friendship and goodwill.”\"}"
  ]


