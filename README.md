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

Most methods in `Fedora.FAS.Client` take a `ClientConfig` as their first
parameter.

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
  :: ClientConfig
     -> SearchType -> String -> IO (Maybe PersonResponse)
```

…which means that `getPerson` is a function which takes a `ClientConfig`, a
`SearchType` (described above), and some search query. Then it returns an IO
operation which might (if the JSON decode was successful) contain a
`PersonResponse`.

It is used like this:


```haskell
λ> getPerson (ClientConfig "http://localhost:6543" "8c0e75be6a63c83090660112a7003dfa3fab6209") Username "relrod"
Just (PersonResponse {personResponseStartTimestamp = 2015-01-18 01:23:30 UTC, personResponseEndTimestamp = 2015-01-18 01:23:30 UTC, personResponsePeople = Person {personUsername = "relrod", personStatus = 1, personIdNumber = 14168, personAvatar = Just "", personFullname = "Ricky Elrod", personCreationDate = 2015-01-17 22:49:23 UTC, personIrcNick = Just "", personEmail = "ricky@elrod.me"}})
it :: Maybe PersonResponse
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
