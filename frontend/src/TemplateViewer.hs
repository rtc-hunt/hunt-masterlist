{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module TemplateViewer where

import Control.Monad.Fix
import Data.Map as Map
import Data.Text (Text, pack)
import Data.Time
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route
import Common.Schema
import Database.Beam
import Database.Beam.Backend.SQL
import Common.View (Msg(..))
import Frontend.Types
import Templates
import Templates.Partials.ChannelList
import Templates.Partials.Message
import Templates.PuzzleList
import Templates.Puzzle

templateViewer ::
  ( Template t m
  , MonadFix m
  , MonadHold t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , Prerender t m
  , MonadIO (Performable m)
  , TriggerEvent t m
  , PerformEvent t m
  )
  => RoutedT t (R TemplateRoute) m ()
templateViewer = do
  subRoute_ $ \case
    TemplateRoute_Index -> divClass "container mx-auto p-4" $ do
      elClass "h1" "font-bold text-h1" $ text "Welcome to the template gallery"
      el "p" $ text "Choose a template below:"
      elClass "ul" "list-disc" $ do
        let templateLink r desc = el "li" $ routeLink (FrontendRoute_Templates :/ r :/ ()) (text desc)
        templateLink TemplateRoute_Login "Login"
        templateLink TemplateRoute_Signup "Signup"
        templateLink TemplateRoute_Channel "Channel"
        templateLink TemplateRoute_Main "Main"
        templateLink TemplateRoute_PuzzleList "PuzzleList"
    TemplateRoute_Channel -> channelTemplateViewer
    TemplateRoute_PuzzleList -> blank -- puzzleListTemplateViewer
    TemplateRoute_Puzzle -> puzzleTemplateViewer
    TemplateRoute_Login -> do
      _ <- Templates.login $ LoginConfig
        { _loginConfig_mode = pure LoginMode_Login
        , _loginConfig_switchModeLink = el "a" . dynText
        , _loginConfig_errors = pure (Just "Those credentials are wrong!")
        }
      pure ()
    TemplateRoute_Signup -> do
      _ <- Templates.login $ LoginConfig
        { _loginConfig_mode = pure LoginMode_Signup
        , _loginConfig_switchModeLink = el "a" . dynText
        , _loginConfig_errors = pure Nothing
        }
      pure ()
    _ -> blank

channelTemplateViewer
  :: Template t m
  => m ()
channelTemplateViewer = do
  _ <- channel $ ChannelConfig
    { _channelConfig_name = pure "Fake Channel"
    , _channelConfig_clearInput = never
    , _channelConfig_messagesConfig = MessagesConfig
      { _messagesConfig_messageList = mapM_ (message . pure) fakeMessages
      }
    , _channelConfig_channelList = ChannelListConfig $ do
      _ <- channelItem "Interviews"
      _ <- channelItem "Book Reviews"
      _ <- channelItem "Opinions"
      pure ()
    }
  pure ()

puzzleTemplateViewer
  :: forall t m js.
  ( Template t m, MonadHold t m, MonadFix m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , Prerender t m
  , MonadHold t m
  , MonadIO (Performable m)
  )
  => m ()
puzzleTemplateViewer = do
  puzzle <- button "puzzle"
  sheet <- button "sheet"
  chat <- button "chat"
  config <- button "config"
  puzzleTab <- holdDyn PuzzlePageTab_Puzzle $ leftmost [PuzzlePageTab_Puzzle <$ puzzle, PuzzlePageTab_Sheet <$ sheet, PuzzlePageTab_Chat <$ chat, PuzzlePageTab_Config <$ config]
  updatePuzzle <- button "Simulate Update Puzzle"
  fakePuz <- holdDyn (fakePuzzles ! PuzzleId 1) $ (fakePuzzles ! PuzzleId 1) <$ updatePuzzle
  dynText $ pack . show <$> puzzleTab
  puzzleView $ PuzzleConfig
    { _puzzleConfig_puzzle = constDyn $ fakePuzzles ! PuzzleId 1
    , _puzzleConfig_tab = puzzleTab
    , _puzzleConfig_chatWidget = blank
    , _puzzleConfig_configuratorWidget = do
         puzzleConfigurator $ PuzzleConfiguratorConfig fakePuz (constDyn ["solved", "stalled"]) (constDyn mempty)
         blank
    , _puzzleConfig_puzzleLink = \_ -> routeLink (FrontendRoute_Templates :/ TemplateRoute_Puzzle :/ ())
    }



{- puzzleListTemplateViewer
  :: forall t m js. (
    Template t m, MonadHold t m, MonadFix m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , Prerender t m
  , MonadIO (Performable m)
  , TriggerEvent t m
  , PerformEvent t m
  )
  => m ()
puzzleListTemplateViewer = do
  {-let puzzles :: Dynamic t ( Map (PrimaryKey Puzzle Identity) (PuzzleData t) ) = constDyn $ Map.fromList [ (PuzzleId 1, PuzzleData {
    _puzzleData_puzzle = constDyn $ Puzzle 1 "test" "someuri" (Just "somesheet") False Nothing (ChatroomId $ Just 0) (HuntId 1),
    _puzzleData_metas = constDyn $ PuzzleId 2 =: "Noodle Bowl" <> PuzzleId 3 =: "Cheezeburger",
    _puzzleData_tags = constDyn $ "Oddball" =: () <> "Extraction" =: (),
    _puzzleData_solutions = constDyn $ Map.fromList $ (\a -> (primaryKey a, a)) <$> [Solution (PuzzleId 1) "JEANLUCPICARD" False, Solution (PuzzleId 1) "LOMEIN" True],
    _puzzleData_notes = constDyn $ Map.fromList $ (\a -> (primaryKey a, a)) <$> [Note 1 (PuzzleId 1) "WTF is up with this pasta?" True],
    _puzzleData_currentSolvers = constDyn $ AccountId 1 =: "jonored" <> AccountId 2 =: "tuttleturtle",
    _puzzleData_status = constDyn ["InProgress"]
    }) ] -}
  puzzlesTable PuzzleTableConfig {
    _puzzleTableConfig_results = constDyn fakePuzzles,
    _puzzleTableConfig_puzzleLink = \_ -> routeLink (FrontendRoute_Templates :/ TemplateRoute_Puzzle :/ ())
  }
-}
  
fakePuzzles :: Reflex t => Map (PrimaryKey Puzzle Identity) (PuzzleData t)
fakePuzzles = Map.fromList [ (PuzzleId 1, PuzzleData {
    _puzzleData_puzzle = constDyn $ Puzzle 1 "test" "http://hackaday.com/" (Just "somesheet") (Nothing) False Nothing (ChatroomId $ Just 0) (HuntId 1) Nothing Nothing,
    _puzzleData_metas = constDyn $ PuzzleId 2 =: "Noodle Bowl" <> PuzzleId 3 =: "Cheezeburger",
    _puzzleData_tags = constDyn $ "Oddball" =: () <> "Extraction" =: (),
    _puzzleData_solutions = constDyn $ Map.fromList $ (\a -> (primaryKey a, a)) <$> [Solution (PuzzleId 1) "JEANLUCPICARD" False, Solution (PuzzleId 1) "LOMEIN" True],
    _puzzleData_notes = constDyn $ Map.fromList $ (\a -> (primaryKey a, a)) <$> [Note 1 (PuzzleId 1) "WTF is up with this pasta?" True],
    _puzzleData_currentSolvers = constDyn $ AccountId 1 =: "jonored" <> AccountId 2 =: "tuttleturtle" -- ,
    -- _puzzleData_status = constDyn "InProgress"
    }) ]

fakeMessages :: [Msg]
fakeMessages = Map.elems $ Map.mapWithKey (\k (h, m) -> Msg
  { _msg_id = MessageId $ fromIntegral k
  , _msg_timestamp = UTCTime day (fromIntegral k * 60)
  , _msg_handle = h
  , _msg_text = m
  }) msgs
  where
    day = fromGregorian 2030 01 01
    msgs = Map.fromList $ zip [(1 :: Int)..] rawMsgs
    rawMsgs :: [(Text, Text)]
    rawMsgs =
      -- Source: http://www.u.arizona.edu/~kimmehea/purdue/421/exampleinterview.htm
      [ ("Interviewer", "Particularly in regard to design and development, what are your duties as a mechanical engineer?")
      , ("Interviewee", "Do you mean before I took this position or in this position.")
      , ("Interviewer", "Both.")
      , ("Interviewee", "In my position I have now, about half of my time is devoted to counseling and registration and other issues like that. About thirty to forty percent of my time is involved with teaching, doing preparation, helping out in the labs, and helping students. About five to ten percent of my time is spent being involved in academic committees and working with administrative items.")
      , ("Interviewer", "Do you do any research?")
      , ("Interviewee", "Most of my research is education-related. I have a grant from the National Science Foundation to put some CNC machines in the student labs to teach students.")
      , ("Interviewer", "What types of research did you do before when you were an associate professor?")
      , ("Interviewee", "I worked primarily with acoustics and noise control, with my emphasis being in active noise and vibration control. I worked with the aircraft fuselage and all of the vibrations and noises created in there and limiting their effects on the cockpit. Of course, automobile engines are also very noisy being so close to the driver. I also worked with compressors. I worked with really small compressors to really big compressors. I worked on small refrigeration units using passive and active control techniques. You'd be surprised at how big an issue refrigerator noise is overseas, in Europe and Asia with their tight living conditions. I also worked with huge engine compressors of up to sixty horsepower. That's really big for a university, you know. I also worked with reciprocating compressors, screw compressors, scroll compressors, and rotary compressors.")
      , ("Interviewer", "Most of your current grants are education-related though, correct?")
      , ("Interviewee", "That's right, most of them are related to education. But I don't have much time in this job now to do that though. I feel that I need to teach with this job, because I need to have that link to the curriculum and the students.")
      , ("Interviewer", "How much contact have you had with industry?")
      , ("Interviewee", "I had quite a bit of contact when I worked as an associate professor. I spent quite a bit of time at the Herrick Labs. I worked with a couple of United Technologies companies, Sikorkey Helicopter and Carrier Corporation, who does refrigeration, Aspera, which is an Italian company that makes compressors, General Motors, and some governmental work.")
      , ("Interviewer", "Did you ever work out in industry before you became a professor?")
      , ("Interviewee", "I worked at NASA-Langley for a year after I graduated with my masters. It really isn't like industry though. It's an academic environment. It's a very research-oriented environment. I also received an educational grant about a year ago to work the summer at Boeing. I worked in Philadelphia with the rotorcraft division. They make all levels of military aircraft. They make the Belle Boeing 609, which is a lot like a V-22. It takes off like a helicopter, straight up, and then the wings turn over and it flies. They also work on CH-47, which is a very old helicopter, in a support mode. They also do some work with the commanche attack helicopter. As you can tell, they work at a lot of different levels in the design.")
      , ("Interviewer", "What is the difference between designing for a new product versus an older product?")
      , ("Interviewee", "There are a lot of challenges no matter what the product. The military has been bringing old CH-47's in to be repaired. Boeing has been gutting them out, leaving just a shell, and completely replacing the interior equipment. All of the design used to be on paper. The new Boeing 777 was a paperless design. They did a fly-through on the computer to check for interferences and other problems. One of the big issues with the CH-47 was whether to recreate this on the computer. It's a difficult decision. It would make it a lot easier to make changes but it would take a lot longer. So they decided not to do it for this product.")
      , ("Interviewer", "What skills are necessary for a mechanical engineer to possess?")
      , ("Interviewee", "Number 1 is the technical skills. You've got to have those. Next are communication and teamwork skills. There is a need for intangibles to be successful. One of the big things at Boeing was timing. They had to pull together over 1,000,000 parts to make the 777. The engine had to come in at the right time to be connected to the fuselage, which had to be connected to other parts. I realized that what Boeing was doing was just a large-scale integration project. It requires a phenomenal amount of communication and scheduling. Being able to plan and schedule things is so important. You're always behind time, over budget, and have to get deliverables to the customer. You have to make a decision with incomplete information. It's a lot of gut feel and just making your best engineering judgement and taking your best shot.")
      , ("Interviewer", "What are the worst skills, or characteristics, for an engineer to have?")
      , ("Interviewee", "In some jobs, being highly individualistic can be a killer. Not in all jobs, but in some jobs. In a research environment, where an engineer can go off and do his own thing, that can be okay. But in the vast majority of jobs, not being strong in communication, and of course, technical skills, can have a very negative impact on your career. In fact, in a survey in the ASME magazine about two or three years ago, the top two skills employers wanted were communication skills and teamwork skills.")
      , ("Interviewer", "What is the difference between the academic world and industry? I know there are some similarities too, what are those?")
      , ("Interviewee", "In the academic world, people tend to be more reflective, more analytical, and less hands-on. That's not always the case, but it tends to be that way. It's partially because people who are attracted to this environment tend to be that way. In industry, the people tend to be more hands-on but the analytical skills tend to atrophy when not used. The academic environment cultivates those skills. But the environment is changing. There are more hands-on activities being added to the curriculum, along with some tighter links to industry. There is more of a need to be an entrepreneur and salesmen.")
      , ("Interviewer", "What is the typical day in the life of a mechanical engineer like?")
      , ("Interviewee", "A typical day varies radically for mechanical engineers depending on the job you have. A guy doing research is more independent, a guy doing customer service is dealing with people all day long, while a manager deals mainly with projects. It can really vary depending on what you want to do.")
      , ("Interviewer", "What can a person do to improve their situation?")
      , ("Interviewee", "The first thing is to define the company's best practices. Define the process and look for ways to improve the process, to make it more efficient. I think that's the idea behind the 9000 stuff, like ISO 9000 and QS 9000, to document the process. Unfortunately, some people just go through the motions, which is really a shame and a waste of time. You've got to take it seriously to do things the most efficient way. But I think the real key issue is getting people in areas they love to work. When you do that, the effort will be there. For example, I met a young engineer at Boeing who had been hired three times in the last three years by Boeing. She loved working with people and making decisions. Unfortunately, in her first two jobs she only made decisions once every two or three months and she hated it. Now they have her in a people where she's working with people and making decisions and she loves it. I think it's real important for companies to match people with what they love to do.")
      , ("Interviewer", "In general, what methods or criteria are used to evaluate mechanical engineers?")
      , ("Interviewee", "At Boeing, the backs of the engineer's badges have criteria that is wanted for the engineers to work on at Boeing. There are twelve things: technical skills, communications, teamwork, initiative, productivity, continuous quality improvement, customer satisfaction, innovation and creativity, integrity ' that's really become a big issue in industry, especially at Boeing when I was there with the merger and all, leadership, risk-taking, and developing people.")
      , ("Interviewer", "I find it interesting to see that risk-taking is on there. It seemed like that has never been encouraged at GM.")
      , ("Interviewee", "Well, you can't just go taking incredible risks. They are calculated risks.")
      , ("Interviewer", "When designing a new product, what issues are typically given the most consideration?")
      , ("Interviewee", "Again, it varies depending on the product. First, you have to understand the customer and find a way to give them what they want. You have to get a sense of where the market is going. Take inline skates. They came out of nowhere and now they're selling four million skates a year. It was a local market in California and they took it national. Being able to see needs is very important and having the creativity to know how to meet them is the hard part.")
      , ("Interviewer", "Is the procedure for process development similar to that for products?")
      , ("Interviewee", "Yeah, I'd say they're similar. You need to do some benchmarking on what's out there to see where you stand and brainstorm to find what you can do.")
      , ("Interviewer", "How are design procedures developed and followed in corporations?")
      , ("Interviewee", "Wow, those procedures vary greatly and to tell you the truth, I don't think they're followed very tightly. Part of the problem is that I don't think they are stated explicitly. You don't want to be rigid, but you need to be efficient. You need to come up with a plan and extrapolate what you can based on your design. It's a real art at this stage. It needs to be tailored to what you are trying to accomplish. There are multiple approaches to this, but it really needs to be designed explicitly and improved from there.")
      , ("Interviewer", "What does a graduating mechanical engineer need to know that he probably does not know?")
      , ("Interviewee", "It's not so much what you don't know as much as it is what will change. The things you like to do now might now be what you like to do in the future. Interest change in time and there must be a willingness to change with them. I think another important thing to recognize for some students is that your whole life is not your job. It can be very easy to ignore other things, but I think the real key is balance. The ME program is very rigorous and everyone is working very hard, and as a result sometimes they don't recognize the need for balance.")
      , ("Interviewer", "Thanks for your time.")
      , ("Interviewee", "You're welcome.")
      ]
