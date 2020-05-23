module App.Database.Schema where

import           RIO
import           Squeal.PostgreSQL

migrations :: Path (Migration (IsoQ Definition)) EmptySchema Schema
migrations = v0 :>> Done

type EmptySchema = Public '[]
type Schema = Schema_v0

type Schema_v0 = Public
  '[ "articles" ::: 'Table ArticleT
   , "messages" ::: 'Table MessageT
   , "pages" ::: 'Table PageT
   , "users" ::: 'Table UserT
   ]

type ArticleT =
  '[ "articles_pk" ::: 'PrimaryKey '["id"]
   ] :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint4
          , "title" ::: 'NoDef :=> 'NotNull 'PGtext
          , "slug" ::: 'NoDef :=> 'NotNull 'PGtext
          , "body" ::: 'NoDef :=> 'NotNull 'PGtext
          ]

type MessageT =
  '[ "messages_pk" ::: 'PrimaryKey '["id"]
   ] :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint4
          , "from" ::: 'NoDef :=> 'NotNull 'PGtext
          , "email" ::: 'NoDef :=> 'NotNull 'PGtext
          , "phone" ::: 'NoDef :=> 'Null 'PGtext
          , "body" ::: 'NoDef :=> 'NotNull 'PGtext
          ]

type PageT =
  '[ "pages_pk" ::: 'PrimaryKey '["id"]
   ] :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint4
          , "title" ::: 'NoDef :=> 'NotNull 'PGtext
          , "slug" ::: 'NoDef :=> 'NotNull 'PGtext
          , "body" ::: 'NoDef :=> 'NotNull 'PGtext
          ]

type UserT =
  '[ "users_pk" ::: 'PrimaryKey '["id"]
   ] :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint4
          , "email" ::: 'NoDef :=> 'NotNull 'PGtext
          , "password_hash" ::: 'NoDef :=> 'NotNull 'PGtext
          ]

v0 :: Migration (IsoQ Definition) EmptySchema Schema_v0
v0 = Migration "init" IsoQ
  {
    up = createTableIfNotExists #articles
         ( serial `as` #id
           :* (text & notNullable) `as` #title
           :* (text & notNullable) `as` #slug
           :* (text & notNullable) `as` #body )
         ( primaryKey #id `as` #articles_pk )

    >>> createTableIfNotExists #messages
         ( serial `as` #id
           :* (text & notNullable) `as` #from
           :* (text & notNullable) `as` #email
           :* (text & nullable) `as` #phone
           :* (text & notNullable) `as` #body )
         ( primaryKey #id `as` #messages_pk )

    >>> createTableIfNotExists #pages
         ( serial `as` #id
           :* (text & notNullable) `as` #title
           :* (text & notNullable) `as` #slug
           :* (text & notNullable) `as` #body )
         ( primaryKey #id `as` #pages_pk )

    >>> createTableIfNotExists #users
         ( serial `as` #id
           :* (text & notNullable) `as` #email
           :* (text & notNullable) `as` #password_hash )
         ( primaryKey #id `as` #users_pk )

  , down = dropTableIfExists #articles
           >>> dropTableIfExists #pages
           >>> dropTableIfExists #messages
           >>> dropTableIfExists #users
  }
