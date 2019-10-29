# Rufus programming language

```rufus
module main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
```

## Design goals

### Users first

### Large teams

Rufus caters to large engineering teams building and operating huge numbers of
services that run continuously for years, with no end in sight. In this
environment, services outlive people. Over its lifetime, service ownership will
be transferred to another team, people on the owning team will transfer to other
teams, and people will leave. Teams need to know that new team members can join
a project and quickly learn how the systems work. Considering its lifetime,
operations is the primary cost of a service

### New developers

Rufus should provide a great experience for a user that has little to no
programming experience, with documentation, tooling, mentorship opportunities,
and more.

### Open source

