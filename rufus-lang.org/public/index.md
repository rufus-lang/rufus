# Rufus programming language

```rufus
module main

import "Fmt"

func main(args list[string]) {
    Fmt.Println("Hello, world!")
}
```

Rufus is for people that build and operate fault tolerant distributed systems.

## Design goals

### Grows with you

Rufus is approachable, with great documentation to help users learn how to build
and operate fault-tolerant services. It's a great technology choice for
startups, and scales up as teams and projects grow into large organizations.

### Statically typed

Rufus is statically typed to make many runtime errors detectable at compile time
instead. Furthermore, as systems get large, types help ensure that interfaces
between adjacent modules don't get silently broken. This is essential for large
teams building and operating large services.

### Builds on Erlang/OTP

Rufus builds on Erlang/OTP. The full power of OTP's fault-tolerant distributed
systems building blocks from applications to releases, and `supervisor` to
`gen_server`, are available in Rufus programs. With a consistent project layout
and common idioms for organizing service logic, Rufus services tend to be
consistent and quickly become familiar.

### Open source

Rufus and all of its related services are open source. We foster a vibrant and
positive community of people helping each other, and contributing to the
language and its ecosystem.

### Local first

Large teams need to be able to run on premises, for a variety of reasons from
secrecy to security to legal regulations and more. It's easy to setup and run
local services to create a private user experience on par with [rufus-lang.org](/).
