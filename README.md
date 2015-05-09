# fas3-haskell-api

This is a Haskell library that implements and makes use of the JSON API exposed
by the new and upcoming [FAS3](https://github.com/fedora-infra/fas/tree/FAS_3.0)
project.

### What all can it do?

There are [haddock docs](https://fedora-infra.github.io/fas3-api-haskell/)
online. In addition, here are a few samples:

#### `ClientConfig`

The first thing to acquaint yourself with is
[`ClientConfig`](https://github.com/fedora-infra/fas3-api-haskell/blob/master/src/Fedora/FAS/Types/ClientConfig.hs).
This type describes how the client should connect to the API. It takes a base
URL and an API key.

We make use of the `ReaderT` monad transformer for threading the `ClientConfig`
around.

#### `getPerson`

This function implements the `/api/person/<field>/<query>` endpoint, which
returns information about a single user based on the unique result of
`<field>=<query>`.

Valid fields are: `id`, `username`, `email`, and `ircnick`. These are encoded as
a [sum type](https://github.com/fedora-infra/fas3-api-haskell/blob/master/src/Fedora/FAS/Types/SearchType.hs)
in fas3-haskell-api.

The type of `getPerson` is:

```haskell
getPerson
  :: PersonSearchType
     -> String
     -> ReaderT
          ClientConfig IO (Either SomeException (Response PersonResponse))
```

…which means that `getPerson` is a function which takes a `SearchType`
(described above), and some search query. Then it returns a `ReaderT` which can
be run with `runReaderT` and given a `ClientConfig`. The result will be an
`Either SomeException (Response PersonResponse)` confined by `IO`.

It is used like this:

```haskell
λ> let config = (ClientConfig "http://localhost:6543" "8c0e75be6a63c83090660112a7003dfa3fab6209")
config :: ClientConfig


λ> let req = getPerson Username "relrod"
req
  :: ReaderT
       ClientConfig IO (Either SomeException (Response PersonResponse))

λ> :t runReaderT req config
runReaderT req config
  :: IO (Either SomeException (Response PersonResponse))

λ> runReaderT req config
Right (Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Length","300"),("Content-Type","application/json; charset=UTF-8"),("Date","Sat, 09 May 2015 23:44:56 GMT"),("Server","waitress")], responseBody = PersonResponse {personResponseStartTimestamp = 2015-05-09 23:44:56.7158 UTC, personResponseEndTimestamp = 2015-05-09 23:44:56.727412 UTC, personResponsePeople = Person {personUsername = "relrod", personStatus = 3, personIdNumber = 14168, personAvatar = Just "", personFullname = "Ricky Elrod", personCreationDate = 2015-01-17 22:49:23 UTC, personIrcNick = Just "", personEmail = "ricky@elrod.me"}}, responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose})
```

#### `getPeople`

This function is similar to `getPerson` but instead of taking a `SearchType` and
a `String`, it simply takes two `Integer`s -- one for page number and one for the
limit of results per page.

There does not seem to be a way to filter the results via the FAS3 API.

### Note about `lens` usage

This library makes heavy use of lenses which allows for easy navigation,
traversal, and modification of various types we expose. To use the above
example:

```haskell
λ> fmap (\x -> x ^. people . fullname) relrod
Just "Ricky Elrod"
it :: Maybe String
```

# LICENSE

BSD2. See the included `LICENSE` file.
