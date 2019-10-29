import React from 'react';

import MarkdownDocument from './../component/MarkdownDocument';

import './Main.css';

function Main() {
    return (
        <main className="Main">
            <div className="Main-content">
                <MarkdownDocument path="/doc/spec" />
            </div>
        </main>
    );
}

export default Main;
