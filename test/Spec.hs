import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "parses a grid" $ do
            parse "Q" `shouldBe` [[Queen]]
            parse "Q." `shouldBe` [[Queen, Empty]]
            parse ".Q" `shouldBe` [[Empty, Queen]]
            parse "Q.\n.Q" `shouldBe` [[Queen, Empty], [Empty, Queen]]
            parse ".Q\nQ." `shouldBe` [[Empty, Queen], [Queen, Empty]]
            parse "Q.\n.Q\n.Q" `shouldBe` [[Queen, Empty], [Empty, Queen], [Empty, Queen]]
            parse ".Q.\nQ..\n..Q"
                `shouldBe` [[Empty, Queen, Empty], [Queen, Empty, Empty], [Empty, Empty, Queen]]
    describe "solve" $ do
        it "solves a grid" $ do
            solve [[Empty]] `shouldBe` [[[Queen]]]
            solve [[Empty, Empty], [Empty, Empty]] `shouldBe` []
            solve
                [ [Empty, Empty, Empty, Empty],
                  [Empty, Empty, Empty, Empty],
                  [Empty, Empty, Empty, Empty],
                  [Empty, Empty, Empty, Empty]
                ]
                `shouldBe` [   [ [Blocked, Blocked, Queen, Blocked],
                                 [Queen, Blocked, Blocked, Blocked],
                                 [Blocked, Blocked, Blocked, Queen],
                                 [Blocked, Queen, Blocked, Blocked]
                               ],
                               [ [Blocked, Queen, Blocked, Blocked],
                                 [Blocked, Blocked, Blocked, Queen],
                                 [Queen, Blocked, Blocked, Blocked],
                                 [Blocked, Blocked, Queen, Blocked]
                               ]
                           ]
