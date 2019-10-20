import React from 'react';

import './Header.css';

class Header extends React.Component {
    render() {
        return (
            <header className="Header">
                <a className="Header-logo" href="/">Rufus</a>
                <span className="Header-links">
                    <a href="https://github.com/rufus-lang/rufus">GitHub</a> <a href="https://twitter.com/rufus_lang">Twitter</a>
                </span>
            </header>
        );
    }
}

export default Header;
