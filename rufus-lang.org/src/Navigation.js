import React from 'react';

import './Navigation.css';

function Navigation() {
    return (
        <header className="Navigation-header">
            <a className="Navigation-logo" href="/">Rufus</a>
            <span className="Navigation-links">
                <a href="https://github.com/rufus-lang/rufus">GitHub</a> <a href="https://twitter.com/rufus_lang">Twitter</a>
            </span>
        </header>
    );
}

export default Navigation;
