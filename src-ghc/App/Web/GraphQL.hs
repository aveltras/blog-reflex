module App.Web.GraphQL where

import           Data.Morpheus.Document
import           Data.Morpheus.Types    (GQLRootResolver (..), MUTATION, QUERY,
                                         Resolver, ResolverQ, Undefined (..))
import           Data.Text              (Text)
import           RIO
import qualified Sessionula.Extra       as Session

import           App.Database
import qualified App.Database.Message   as Message
import           App.GraphQL.Schema
import           App.Types
import           App.Web.Types


rootResolver :: GQLRootResolver (RIO Ctx) () Query Mutation Undefined
rootResolver = GQLRootResolver
  { queryResolver = Query
    { queryDeity = deityResolver
    , queryArticles = articlesResolver
    , queryArticle = articleResolver
    , queryPage = pageResolver
    }
  , mutationResolver = Mutation
    { mutationLogin = loginResolver
    , mutationLogout = logoutResolver
    , mutationSendMessage = sendMessageResolver
    }
  , subscriptionResolver = Undefined
  }

pageResolver :: QueryPageArgs -> (Resolver QUERY () (RIO Ctx) (Maybe (Page (Resolver QUERY () (RIO Ctx)))))
pageResolver = error "not implemented"

articleResolver :: QueryArticleArgs -> (Resolver QUERY () (RIO Ctx) (Maybe (Article (Resolver QUERY () (RIO Ctx)))))
articleResolver = error "not implemented"

articlesResolver :: Resolver QUERY () (RIO Ctx) (Maybe [Maybe (Article (Resolver QUERY () (RIO Ctx)))])
articlesResolver = error "not implemented"

deityResolver :: QueryDeityArgs -> ResolverQ () (RIO Ctx) Deity
deityResolver QueryDeityArgs {..} = do
  mAuth <- lift $ Session.authStatus
  lift $ case mAuth of
    Nothing -> Session.authenticate $ UserId 1
    Just _  -> Session.logout
  pure Deity
    { deityName = pure $ if isJust mAuth then "Authenticated Morpheus" else "Guest Morpheus"
    , deityPower = pure (Just "Shapeshifting") -- liftEither $ pure $ Left "fail example" --
    }

loginResolver :: MutationLoginArgs -> Resolver MUTATION () (RIO Ctx) (Maybe (User (Resolver MUTATION () (RIO Ctx))))
loginResolver MutationLoginArgs {..} = do
  pure $ Just $ User { userEmail = pure "email" }

logoutResolver :: Resolver MUTATION () (RIO Ctx) Bool
logoutResolver = error "not implemented"

sendMessageResolver :: MutationSendMessageArgs -> Resolver MUTATION () (RIO Ctx) Bool
sendMessageResolver MutationSendMessageArgs {..} = do
  -- lift $ runPool $ Message.insert mutationSendMessageArgsMessage
  pure True
