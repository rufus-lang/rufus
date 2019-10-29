# Rufus decision records

* ID: RDR-0000
* Status: Accepted
* Authors: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Deciders: Jamu Kakar <[jkakar@kakar.ca](mailto:jkakar@kakar.ca)>
* Date: March 24, 2019

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
adaptable to our needs. Rufus decision records are assigned an RDR-_nnnn_ ID
when they're accepted. See [RDR-0001](0001-template.md) for a template.
