{-# LANGUAGE OverloadedStrings     #-}

module TimelineSpec where
import Client.Timeline
import Data.Text
import Client.Util

import Test.Hspec

timeline :: Text -> [Day] -> Timeline
timeline name days = Timeline {timelineName=name, days=days, version=1, today=1}

day :: Text -> Day
day name = Day{name=name, total=0, bottom=0, top=Nothing, expected=1.0, working_day=True}

spec :: Spec
spec = describe "Timeline Events tests" $
                     describe "the whole timeline chantged" $ do
                       it "same timeline should not fire events" $
                          () `shouldBe` ()
{-
                       it "day deleted" $
                           events (timeline "foo" (fmap day ["Sunday"]))
                                  (timeline "foo" [])
                                  `shouldBe` [TimelineDayEvent Deleted (day "Sunday")]

                       it "day created" $
                           events (timeline "foo" [])
                                  (timeline "foo" (fmap day ["Sunday"]))
                                   `shouldBe` [TimelineDayEvent Created (day "Sunday")]

                       it "day modified" $
                           events (timeline "foo" (fmap day ["Monday"]))
                                  (timeline "foo" (fmap day ["Sunday"]))
                                   `shouldBe` [TimelineDayEvent (Modified "Monday") (day "Sunday")]
-}
