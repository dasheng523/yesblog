module StateMonadSpec where

import Test.Hspec

type Age = Int

gettingOlder :: State Age String
gettingOlder = do
    age <- get
    let newAge = age + 1
    put newAge
    pure ("You're now older for 1 year: " <> show newAge)

bornAndGettingOlderTwice :: State Age [String]
bornAndGettingOlderTwice = do
    put 0
    msg1 <- gettingOlder
    msg2 <- gettingOlder
    newAge <- get
    pure [msg1, msg2, "Your age now: " <> show newAge]

spec :: Spec
spec =
    describe "State monad tests" $ do
        it "Age script" $ do
            let (msgs, s) = runState bornAndGettingOlderTwice 111
            s `shouldBe` 2
            msgs
                `shouldBe` [ "You're now older for 1 year: 1"
                           , "You're now older for 1 year: 2"
                           , "Your age now: 2"
                           ]