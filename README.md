# purescript-storable

The aim of this library is to abstract somewhat over the idea of
`string->string` storage and to provide a somewhat safe interface for those
kinds of storages. It is (or was) primarily made for `localStorage` from the
beginning, but the concept itself is pretty general, hence the type class.

## What's in there?

There's a type class for things that are `Storable`, which means we can
guarantee that given a reasonable implementation, things will be stored and
retrieved correctly from storage.

There's also a `MonadStorage` type class meant to abstract over the idea of a
monad that lets you store things. There are the newtypes `LocalStorage` &
`SessionStorage` that both wrap `Effect`. The library internally uses the
`Storage` type from `Web.Storage` in order to be compatible, while still
exposing a nicer(?) interface.

## Feedback

If you have any feedback, I'm on the FP Slack
(`functionalprogramming.slack.com`) as well as `GoNZooo` on GitHub, `gonz` on
GitLab and you can find my email in this repository. This is very much my first
contributed library to PureScript, so pointers on how better to fit in with the
community are most welcome.