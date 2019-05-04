# Rufus decision records

* Status: Accepted
* Deciders: Jamu Kakar <jkakar@kakar.ca>
* Date: 2019-03-24

## Context and problem statement

There are many decisions to make in the design of Rufus, and each one has many
tradeoffs to consider. We want to record the context around important language
decisions, both as part of the design process and as documentation for users.

## Decision drivers

* A decision record must clearly describe the context that motivates a solution,
  the options considered, and the decision made.

## Considered options

The following formats have been considered:

* [MADR](https://adr.github.io/madr/)
* [Python Enhancement Proposals](https://www.python.org/dev/peps/pep-0001/)
* [Rust RFCs](https://github.com/rust-lang/rfcs/blob/master/0000-template.md)

## Decision outcome

Chosen option: a subset of the MADR template, because it's the most easily
adaptable to our needs. See [RDR-0001](0001-template.md) for a template.
