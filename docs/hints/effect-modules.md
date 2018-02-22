
# Reviewing Effect Modules

Effect modules in the package ecosystem define “The Elm Platform”, providing nice APIs for things like web sockets, geolocation, and page visibility.

The only intent of effect modules is to help Elm communicate with **external** services. If you want to write a wrapper around GraphQL or Phoenix Channels, you are using effect modules as intended. If you are doing any other kind of thing, it may be subverting “The Elm Platform” in relatively serious ways.

So to *publish* a package that uses effect modules, you need to go through a review process to make sure these facilities are not being abused. Think of it as contributing to the compiler or core libraries. Obviously someone is going to review that PR in those cases. Same thing here.

To make this as smooth as possible, discuss it [on slack](http://elmlang.herokuapp.com/) and [the elm-dev mailing list](https://groups.google.com/forum/#!forum/elm-dev) as soon as possible. It is impossible to collaborate with people if you do not communicate! So come and talk through your goals. See if it aligns with Elm overall or if there is some nicer way.
