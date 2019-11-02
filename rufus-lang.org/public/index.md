# Rufus programming language

```rufus
module example

import "Fmt"

func main() {
    Fmt.Println("Hello, world!")
}
```

## Design goals

### Beginner-friendly

It's difficult for a new user with little experience to know how to get started
with programming. Rufus should provide a smooth path for new users to learn the
language and start writing Rufus programs.

### Startup-friendly

Startups have to make technology choices before they begin building their
products. At the beginning of the product development cycle, when you don't know
if your product is going to succeed or not, you want a language that lets you
experiment easily and move quickly. Startups need a fast iteration cycle to get
changes to production quickly and frequently. Rufus should be a great choice for
startups with tools and patterns to support a fast iteration cycle.

### Team-friendly

Startups grow into big companies. The technology choices they made when they
were small and starting out need to scale to eventually support dozens of
product lines, hundreds of teams, and thousands of developers. Big teams need a
fast iteration cycle along with tools to manage ownership of code and systems.
They often have security and compliance needs that must be met. Rufus should be
a great choice for large teams and provide tools and patterns to support
operating at scale.

### Open source

Consider a case where rufus-lang.org has a package index service, for example.
Services likes this will be open source and running them locally will be a first
class operation. Large teams need to be able to run on premises, for a variety
of reasons from secrecy to security to legal regulations and more. It should be
easy to setup and run local services to create a user experience on par with
rufus-lang.org.
