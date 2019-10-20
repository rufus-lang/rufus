import React from 'react';

import './Main.css';

function Main() {
    return (
        <main className="Main">
            <div className="Main-content">
                <h1>Rufus decision records</h1>

                <ul>
                    <li>ID: RDR-0000</li>
                    <li>Status: Accepted</li>
                    <li>Deciders: Jamu Kakar &lt;<a href="mailto:jkakar@kakar.ca">jkakar@kakar.ca</a>&gt;</li>
                    <li>Date: March 24, 2019</li>
                </ul>

                <h2>Context and problem statement</h2>

                <p>There are many decisions to make in the design of Rufus, and each one has many
                tradeoffs to consider. We want to record the context around important language
                decisions, both as part of the design process and as documentation for users.</p>

                <h2>Decision drivers</h2>

                <ul>
                    <li>A decision record must clearly describe the context that motivates a solution,
                    the options considered, and the decision made.</li>
                </ul>

                <h2>Considered options</h2>

                <p>The following formats have been considered:</p>

                <ul>
                    <li><a href="https://adr.github.io/madr/">MADR</a></li>
                    <li><a href="https://www.python.org/dev/peps/pep-0001">Python Enhancement Proposals</a></li>
                    <li><a href="https://github.com/rust-lang/rfcs/blob/master/0000-template.md">Rust RFCs</a></li>
                </ul>

                <h2>Decision outcome</h2>

                <p>Chosen option: a subset of the MADR template, because it's the most easily
                adaptable to our needs. Rufus decision records are assigned an RDR-<em>nnnn</em> ID
                when they're accepted. See <a href="0001-template.md">RDR-0001</a> for a template.</p>
            </div>
        </main>
    );
}

export default Main;
